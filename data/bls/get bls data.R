library(readxl)

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
          "total, all occupations", "computer and information research scientists", 
          "management analysts")

df_bls_data <- 
    f_get_bls(bls_years) %>% 
    mutate(OCC_TITLE = tolower(OCC_TITLE)) %>%
    filter(OCC_TITLE %in% jobs) %>%
    select(AREA_TITLE, OCC_TITLE, O_GROUP, TOT_EMP, JOBS_1000, 
           H_MEAN, A_MEAN, H_PCT10, H_PCT90, H_MEDIAN, A_PCT10, A_PCT90, A_MEDIAN)

df_bls_data %>% write.csv(file = "data/bls/df_bls_data.csv", row.names = FALSE)

message("Created & Cleansed: df_bls_data")
message("Object was saved in csv format in data/bls")

