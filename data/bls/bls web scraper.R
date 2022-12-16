# This script web scrapes tables from BLS website.

library(tidyverse)
library(rvest)

# Wage Model Series ID Reference Codes----
# Below is a script to parse html-text Series ID Dictionary
# Use each object in order to build time series Series ID
df_area_code <- 
    read_html("https://download.bls.gov/pub/time.series/wm/wm.area") %>% 
    html_text() %>% 
    read.csv(text = ., sep = "\t") %>%
    as_tibble()

df_ownership_code <- 
    read_html("https://download.bls.gov/pub/time.series/wm/wm.ownership") %>% 
    html_text() %>% 
    read.csv(text = ., sep = "\t") %>%
    as_tibble()

df_estimate_code <- 
    read_html("https://download.bls.gov/pub/time.series/wm/wm.estimate") %>% 
    html_text() %>% 
    read.csv(text = ., sep = "\t") %>%
    as_tibble()

df_industry_code <- 
    read_html("https://download.bls.gov/pub/time.series/wm/wm.industry") %>% 
    html_text() %>% 
    read.csv(text = ., sep = "\t") %>%
    as_tibble()

df_occupation_code <- 
    read_html("https://download.bls.gov/pub/time.series/wm/wm.occupation") %>% 
    html_text() %>% 
    read.csv(text = ., sep = "\t") %>%
    as_tibble()

df_job_characteristic_code <- 
    read_html("https://download.bls.gov/pub/time.series/wm/wm.subcell") %>% 
    html_text() %>% 
    read.csv(text = ., sep = "\t") %>%
    as_tibble()

df_exp_level_code <- 
    read_html("https://download.bls.gov/pub/time.series/wm/wm.level") %>% 
    html_text() %>% 
    read.csv(text = ., sep = "\t") %>%
    as_tibble()

df_area_code %>% write.csv(file = "data/bls/df_area_code.csv", row.names = FALSE)
df_ownership_code %>% write.csv(file = "data/bls/df_ownership_code.csv", row.names = FALSE)
df_estimate_code %>% write.csv(file = "data/bls/df_estimate_code.csv", row.names = FALSE)
df_industry_code %>% write.csv(file = "data/bls/df_industry_code.csv", row.names = FALSE)
df_occupation_code %>% write.csv(file = "data/bls/df_occupation_code.csv", row.names = FALSE)
df_job_characteristic_code %>% write.csv(file = "data/bls/df_job_characteristic_code.csv", row.names = FALSE)
df_exp_level_code %>% write.csv(file = "data/bls/df_exp_level_code.csv", row.names = FALSE)


# Employment Projections----
df_employment_projection <- 
    read_html("https://www.bls.gov/emp/tables/emp-by-detailed-occupation.htm") %>%
    html_nodes("table") %>% 
    html_table() %>% 
    .[[1]] %>%
    as_tibble()

df_employment_projection %>% write.csv(file = "data/bls/df_employment_projection.csv", row.names = FALSE)


message("Created series id reference objects: df_area_code, df_ownership_code, df_estimate_code, df_industry_code, df_occupation_code, df_job_characteristic_code, df_exp_level_code.")
message("Created additional object: df_employment_projection")
message("All objects were saved in csv format in data/bls")
