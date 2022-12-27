library(tidyverse)
library(lubridate)
library(readxl)

# BLS Data
# https://www.bls.gov/help/hlpforma.htm#WM

bls_start_date <- 2004

# Create blank vector to store output from below logic
bls_years <- c()

# Get distinct years since 2004 up to current year
while (bls_start_date <= Sys.Date() %>% year() - 1) {
    bls_years <- append(bls_start_date, bls_years)
    bls_start_date <- bls_start_date + 1
}

bls_years <- bls_years %>% substr(3,4)

if(!dir.exists("data/bls/oesm")) {dir.create("data/bls/oesm")}


# Download Data from BLS
f_get_bls <- function(x) {
    
    for(i in x) {
        
        temp <- tempfile()
    
        download.file(paste0("https://www.bls.gov/oes/special.requests/oesm", i, "st.zip"),temp)
        
        df_temp <- 
            read_excel(unzip(temp, exdir = "data/bls/oesm")[grepl("state", unzip(temp, exdir = "data/bls/oesm"))], sheet = 1) %>%
            mutate(year = paste0("20", i))
        
        if(!exists("df_out")) {
            
            df_out <- df_temp
        } else {
            df_out <- bind_rows(df_out, df_temp)
        }
        
        unlink(temp)
    }
    
    return(df_out)
}

jobs <- c("database administrators", "database architects", 
          "operations research analysts", "data scientists", 
          "all occupations", "computer and information research scientists")

df_bls_data_base <- f_get_bls(bls_years)

occ_codes <- 
    df_bls_data_base %>%
    mutate(OCC_TITLE = tolower(OCC_TITLE)) %>%
    filter(OCC_TITLE %in% jobs) %>%
    .$OCC_CODE %>%
    unique()

df_bls_data <- df_bls_data_base %>%
    mutate(OCC_TITLE = tolower(OCC_TITLE)) %>%
    filter(OCC_CODE %in% occ_codes) %>%
    select(year, AREA_TITLE, OCC_CODE, OCC_TITLE, O_GROUP, TOT_EMP, JOBS_1000, 
           H_MEAN, A_MEAN, H_PCT10, H_PCT90, H_MEDIAN, A_PCT10, A_PCT90, A_MEDIAN) %>%
    mutate(keyword = case_when(OCC_TITLE == "database administrators" ~ "Data Engineering",
                               OCC_TITLE == "database architects" ~ "Data Architecture",
                               OCC_TITLE == "operations research analysts" ~ "Data Analytics",
                               OCC_TITLE == "data scientists" ~ "Data Science",
                               OCC_TITLE == "computer and information research scientists" ~ "Machine Learning",
                               OCC_TITLE == "all occupations" ~ "All US Occupations")) %>%
    rename(state = AREA_TITLE) %>%
    rename(Median_Salary = A_MEDIAN) %>%
    rename(Mean_Salary = A_MEAN) %>%
    rename(Year = year) %>%
    rename(Keyword = keyword)

df_bls_data %>% write.csv(file = "data/bls/df_bls_data.csv", row.names = FALSE)

# Delete Temp Files
unlink("data/bls/oesm", recursive = TRUE)

message("Created & Cleansed: df_bls_data")
message("Object was saved in csv format in data/bls")

