library(tidyverse)
library(lubridate)
library(gtrendsR)


# Google Trends Data
# https://support.google.com/trends/answer/4365533?hl=en

f_get_trend_data <- function(type = c("relative", "independent")) {
    
    env_get_trend_data <- environment()
    
    trend_datasets <- 
        c("df_interest_over_time", 
          "df_interest_by_region",
          "df_interest_by_dma", 
          "df_interest_by_city", 
          "df_related_topics", 
          "df_related_queries")
    
    lapply(trend_datasets, function(x) {if(exists(x)){rm(list = x, envir = env_get_trend_data)}})
    
    topic_keywords <- c("Data Engineering",
                        "Data Analytics", 
                        "Data Science", 
                        "Data Architecture", 
                        "Machine Learning")
    
    # Relative download enables comparison between topic keywords
    if(type == "relative") {
        l_temp <- gtrends(keyword = topic_keywords, geo = "US", time = "all")
        
        df_interest_over_time <- l_temp$interest_over_time
        df_interest_by_region <- l_temp$interest_by_region
        df_interest_by_dma <- l_temp$interest_by_dma
        df_interest_by_city <- l_temp$interest_by_city
        df_related_topics <- l_temp$related_topics
        df_related_queries <- l_temp$related_queries
    
    # Independent download looks at topic keywords individually
    } else if(type == "independent") {
        
        for(i in topic_keywords) {
            
            # Download US Search Trend Data from Google Trends API
            l_temp <- gtrends(keyword = i, geo = "US", time = "all")
            
            for(i in trend_datasets){
                
                df_interest_over_time_temp <- l_temp$interest_over_time
                df_interest_by_region_temp <- l_temp$interest_by_region
                df_interest_by_dma_temp <- l_temp$interest_by_dma
                df_interest_by_city_temp <- l_temp$interest_by_city
                df_related_topics_temp <- l_temp$related_topics
                df_related_queries_temp <- l_temp$related_queries
                
                ifelse(exists(i),
                       assign(i, eval(as.symbol(i)) %>% bind_rows(eval(as.symbol(paste0(i, "_temp"))))),
                       assign(i, eval(as.symbol(paste0(i, "_temp")))))
            }
            
            # Pause 2 seconds after each iteration so google doesn't block me
            Sys.sleep(2)
        }
    } else {print("Incorrect Type")}
    
    return(
        list(
            df_interest_over_time = df_interest_over_time,
            df_interest_by_region = df_interest_by_region,
            df_interest_by_dma = df_interest_by_dma,
            df_interest_by_city = df_interest_by_city,
            df_related_topics = df_related_topics,
            df_related_queries = df_related_queries
        )
    )
}

#
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

