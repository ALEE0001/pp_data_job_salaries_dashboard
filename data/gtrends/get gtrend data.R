library(tidyverse)
library(lubridate)
library(gtrendsR)
library(datasets)

v_state_abb <- paste0("-", datasets::state.abb) %>% append("")

# Google Trends Data
# https://support.google.com/trends/answer/4365533?hl=en
# Takes 10~15 mins to run due to Google's API Request limit.

f_get_trend_data <- function(type = c("relative", "independent")) {
    
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
            
            l_temp <- gtrends(keyword = topic_keywords, geo = "US", time = "all")
           
            df_interest_over_time_temp <- l_temp$interest_over_time %>% mutate(across(everything(), as.character))
            df_interest_by_region_temp <- l_temp$interest_by_region %>% mutate(across(everything(), as.character))
            
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
               
                 # Download US Search Trend Data from Google Trends API
                l_temp <- gtrends(keyword = i, geo = paste0("US",state), time = "all", onlyInterest = TRUE)
                
                df_interest_over_time_temp <- l_temp$interest_over_time %>% mutate(across(everything(), as.character))
                
                if(!exists("df_interest_over_time")) {
                    df_interest_over_time <- df_interest_over_time_temp
                } else {
                    df_interest_over_time <- bind_rows(df_interest_over_time, df_interest_over_time_temp)
                }
                
                Sys.sleep(3) 
            }
            

            
            # for(i in trend_datasets){
            #     
            #     df_interest_over_time_temp <- l_temp$interest_over_time
                # df_interest_by_region_temp <- l_temp$interest_by_region
                # df_interest_by_dma_temp <- l_temp$interest_by_dma
                # df_interest_by_city_temp <- l_temp$interest_by_city
                # df_related_topics_temp <- l_temp$related_topics
                # df_related_queries_temp <- l_temp$related_queries
                
            #     ifelse(exists(i),
            #            assign(i, eval(as.symbol(i)) %>% bind_rows(eval(as.symbol(paste0(i, "_temp"))))),
            #            assign(i, eval(as.symbol(paste0(i, "_temp")))))
            # }
            
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


l_df_trend_relative <- f_get_trend_data("relative")

l_df_trend_relative$df_interest_by_region <- 
    l_df_trend_relative$df_interest_by_region %>% 
    rename(state = location) %>%
    rename(Popularity = hits)

l_df_trend_relative %>% write_rds(file = "data/gtrends/l_df_trend_relative.rds")



l_df_trend_independent <- f_get_trend_data("independent")

l_df_trend_independent$df_interest_by_region <- 
    l_df_trend_independent$df_interest_by_region %>% 
    rename(state = location) %>%
    rename(Popularity = hits)

l_df_trend_independent %>% write_rds(file = "data/gtrends/l_df_trend_independent.rds")

