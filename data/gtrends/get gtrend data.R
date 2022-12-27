library(tidyverse)
library(lubridate)
library(gtrendsR)
library(datasets)

# Google Trends Data
# https://support.google.com/trends/answer/4365533?hl=en
# May take up to 20 mins to run due to Google's API Request limit.

f_get_trend_data <- function(type = c("relative", "independent")) {
 
    v_state_abb <- paste0("-", datasets::state.abb) %>% append("")
       
    env_get_trend_data <- environment()
    
    trend_datasets <-
        c("df_interest_over_time",
          "df_interest_by_region")
    
    lapply(trend_datasets, function(x) {if(exists(x)){rm(list = x, envir = env_get_trend_data)}})
    
    topic_keywords <- c("Data Engineering",
                        "Data Analytics", 
                        "Data Science", 
                        "Data Architecture", 
                        "Machine Learning")
    
    # Relative download enables comparison between topic keywords
    if(type == "relative") {
        
        for(state in v_state_abb) {
            
            # Download US Search Trend Data from Google Trends API, Iterate over States.
            l_temp <- gtrends(keyword = topic_keywords, geo = paste0("US",state), time = "all")
           
            # Store Temp Data, and use tryCatch to error handle no data situations.
            df_interest_over_time_temp <- 
                tryCatch(expr = l_temp$interest_over_time %>% mutate(across(everything(), as.character)),
                         error = function(e) {data.frame()})
            df_interest_by_region_temp <- 
                tryCatch(expr = l_temp$interest_by_region %>% mutate(across(everything(), as.character)),
                         error = function(e) {data.frame()})
            
            if(!exists("df_interest_over_time")) {
                df_interest_over_time <- df_interest_over_time_temp
            } else {
                df_interest_over_time <- bind_rows(df_interest_over_time, df_interest_over_time_temp)
                
            }
            
            if(!exists("df_interest_by_region")) {
                df_interest_by_region <- df_interest_by_region_temp
            } else {
                df_interest_by_region <- bind_rows(df_interest_by_region, df_interest_by_region_temp)
                
            }
            
            Sys.sleep(3) 
        }
        

    
    # Independent download looks at topic keywords individually
    } else if(type == "independent") {
        
        for(i in topic_keywords) {
            
            for(state in v_state_abb) {
               
                # Download US Search Trend Data from Google Trends API, Iterate over States.
                l_temp <- gtrends(keyword = i, geo = paste0("US",state), time = "all", onlyInterest = TRUE)
                
                # Store Temp Data, and use tryCatch to error handle no data situations.
                df_interest_over_time_temp <- 
                    tryCatch(expr = l_temp$interest_over_time %>% mutate(across(everything(), as.character)),
                             error = function(e) {data.frame()})
                
                if(!exists("df_interest_over_time")) {
                    df_interest_over_time <- df_interest_over_time_temp
                } else {
                    df_interest_over_time <- bind_rows(df_interest_over_time, df_interest_over_time_temp)
                }
                
                Sys.sleep(3) 
            }
            
            Sys.sleep(5)
        }
    } else {print("Incorrect Type")}
    
    if(type == "relative") {
        
        return(
            list(
                df_interest_over_time = df_interest_over_time,
                df_interest_by_region = df_interest_by_region
            )
        )
    } else if(type == "independent") {
        
        return(
            list(
                df_interest_over_time = df_interest_over_time
            )
        )
    }

}

# Create df of state abbreviations and its full name, to be used as a mapping source.
df_state_mapping <- data.frame(code = state.abb, state = state.name) %>% rbind(data.frame(code = "US", state = "All"))

l_df_trend_relative <- f_get_trend_data("relative")

l_df_trend_relative$df_interest_by_region <- 
    l_df_trend_relative$df_interest_by_region %>% 
    rename(state = location) %>%
    rename(Popularity = hits) %>%
    rename(Keyword = keyword) %>%
    mutate(Popularity = as.integer(Popularity))

l_df_trend_relative$df_interest_over_time <- 
    l_df_trend_relative$df_interest_over_time %>%
    rename(Date = date) %>%
    rename(Popularity = hits) %>%
    rename(Keyword = keyword) %>%
    mutate(Date = as_date(Date)) %>%
    mutate(Popularity = as.integer(Popularity, na.rm = TRUE)) %>%
    mutate(geo = case_when(geo == "US" ~ "US",
                           TRUE ~ substr(geo, 4,5))) %>%
    left_join(df_state_mapping, by = c("geo" = "code"))
        

l_df_trend_relative %>% write_rds(file = "data/gtrends/l_df_trend_relative.rds")



l_df_trend_independent <- f_get_trend_data("independent")

l_df_trend_independent$df_interest_over_time <- 
    l_df_trend_independent$df_interest_over_time %>%
    rename(Date = date) %>%
    rename(Popularity = hits) %>%
    rename(Keyword = keyword) %>%
    mutate(Date = as_date(Date)) %>%
    mutate(Popularity = as.integer(Popularity, na.rm = TRUE)) %>%
    mutate(geo = case_when(geo == "US" ~ "US",
                           TRUE ~ substr(geo, 4,5))) %>%
    left_join(df_state_mapping, by = c("geo" = "code"))

l_df_trend_independent %>% write_rds(file = "data/gtrends/l_df_trend_independent.rds")

