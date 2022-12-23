library(tidyverse)
library(lubridate)
library(scales)
library(skimr)
library(kaggler)
library(gtrendsR)
library(blsR)
library(rvest)
library(countrycode)
library(plotly)
library(geojsonio)
library(rgdal)
library(broom)
library(rgeos)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)

# Summary: Overview dashboard of data career in United States


# Data----

# BLS Data
# https://www.bls.gov/help/hlpforma.htm#WM

# Rerun following comment to update BLS data:
# source("data/bls/get bls data.R")

# BLS Salary
df_bls_data <- read_csv("data/bls/df_bls_data.csv")


# AI/ML/Data Science Salary Data 
# https://ai-jobs.net/salaries/download/

# Authenticate to Kaggle API 
# This is Hidden in Github. Please Generate your own for reproducibility.
kaggler::kgl_auth(creds_file = "kaggle.json")

# Download prepped data from Kaggle
# https://www.kaggle.com/datasets/cedricaubin/ai-ml-salaries

# df_salary <- kgl_datasets_download(
#   owner_dataset = "cedricaubin/ai-ml-salaries",
#   fileName = "salaries.csv",
#   datasetVersionNumber = 2)

# write.csv(df_salary, file = "data/df_salary.csv", row.names = FALSE)

df_salary_base <- read_csv("data/df_salary.csv")

df_salary <-
  df_salary_base %>% 
    
    # Filter to United States only
    rename(year = work_year) %>%
    rename(usd_salary = salary_in_usd) %>%
    filter(company_location == "US") %>%
    filter(usd_salary > 50000) %>%
    mutate(job_title = tolower(job_title)) %>%
    mutate(job_bin = case_when(grepl("data analy", job_title) ~ "Data Analyst",
                               grepl("data scien", job_title) ~ "Data Scientist",
                               grepl("data engineer", job_title) ~ "Data Engineer",
                               grepl("analytics engineer", job_title) ~ "Data Engineer",
                               grepl("etl", job_title) ~ "Data Engineer",
                               grepl("architect", job_title) ~ "Data Architect",
                               grepl("bi analyst", job_title) ~ "Business Intelligence Analyst",
                               grepl("machine", job_title) ~ "Machine Learning Engineer",
                               grepl("ml", job_title) ~ "Machine Learning Engineer",
                               TRUE ~ "Other")) %>%
    mutate(keyword = case_when(job_bin == "Data Analyst" ~ "Data Analytics",
                               job_bin == "Data Architect" ~ "Data Architecture",
                               job_bin == "Data Engineer" ~ "Data Engineering",
                               job_bin == "Data Scientist" ~ "Data Science",
                               job_bin == "Machine Learning Engineer" ~ "Machine Learning",
                               job_bin == "Business Intelligence Analyst" ~ "Business Intelligence",
                               TRUE ~ "Other")) %>%
    group_by(job_bin) %>%
    mutate(job_bin_count = n()) %>%
    ungroup() %>%
    filter(job_bin_count > 10) %>%
    filter(job_bin != "Other") %>%
    select(year,
           job_title,
           job_bin,
           keyword,
           employment_type,
           experience_level,
           usd_salary,
           remote_ratio,
           employee_residence,
           company_location,
           company_size)
    



# Goggle Trends Data
# https://support.google.com/trends/answer/4365533?hl=en


f_get_trend_data <- function(type = c("relative", "independent")) {
  
  env_get_trend_data <- environment()
  
  trend_datasets <- 
    c("df_interest_over_time", 
      # "df_interest_by_country", 
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
  
  if(type == "relative") {
      l_temp <- gtrends(keyword = topic_keywords, geo = "US", time = "all")
      
      df_interest_over_time <- l_temp$interest_over_time
      # df_interest_by_country <- df_interest_by_country
      df_interest_by_region <- l_temp$interest_by_region
      df_interest_by_dma <- l_temp$interest_by_dma
      df_interest_by_city <- l_temp$interest_by_city
      df_related_topics <- l_temp$related_topics
      df_related_queries <- l_temp$related_queries
      
  } else if(type == "independent") {
      
      for(i in topic_keywords) {
    
        # Download US Search Trend Data from Google Trends API
        l_temp <- gtrends(keyword = i, geo = "US", time = "all")
        
        for(i in trend_datasets){
          
          df_interest_over_time_temp <- l_temp$interest_over_time
          # df_interest_by_country_temp <- l_temp$interest_by_country
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
      # df_interest_by_country = df_interest_by_country,
      df_interest_by_region = df_interest_by_region,
      df_interest_by_dma = df_interest_by_dma,
      df_interest_by_city = df_interest_by_city,
      df_related_topics = df_related_topics,
      df_related_queries = df_related_queries
      )
    )
}

# l_df_trend_relative <- f_get_trend_data("relative")
# l_df_trend_relative %>% write_rds(file = "data/l_df_trend_relative.rds")
l_df_trend_relative <- read_rds(file = "data/l_df_trend_relative.rds")

# l_df_trend_independent <- f_get_trend_data("independent")
# l_df_trend_independent %>% write_rds(file = "data/l_df_trend_independent.rds")
l_df_trend_independent <- read_rds(file = "data/l_df_trend_independent.rds")



this_year <- Sys.Date() %>% year()

# Define UI
ui <- dashboardPage(
    
  skin = "black",
    
  # Application title
  dashboardHeader(title = "US Data Career"),
    
    # dashboardHeader(title = "US Data Career"),
                  # tags$li(
                  #     div(
                  #         img(src = "logo.png",
                  #             title = "Logo",
                  #             height = "67px"),
                  #         style = "margin-right: 10px;"
                  #     ),
                      # class = "dropdown",
                      # tags$style(".main-header {max-height: 70px}"),
                      # tags$style(".main-header .logo {height: 70px}")
                  # )
  # ),
                  


  dashboardSidebar(disable = TRUE),
  
  
  dashboardBody(
      includeCSS(path = "www/general.css"),
      useShinyjs(),
      
      fluidRow(
          column(width = 12,
                 pickerInput(
                     inputId = "filterkeyword",
                     label = "Keyword",
                     choices = l_df_trend_relative$df_interest_over_time$keyword %>% unique() %>% sort(),
                     multiple = TRUE,
                     options = pickerOptions(
                         "actionsBox" = TRUE,
                         "liveSearch" = TRUE,
                         "size" = 10,
                         "noneSelectedText" = "All"
                         )
                     )
                 )
          ),
      
      
      # pickerInput(
      #     inputId = "filterstate",
      #     label = "State",
      #     choices = l_df_trend_relative$df_interest_by_region$location %>% unique() %>% sort(),
      #     multiple = TRUE,
      #     options = pickerOptions(
      #         "actionsBox" = TRUE,
      #         "liveSearch" = TRUE,
      #         "size" = 10,
      #         "noneSelectedText" = "All"
      #     )
      # ),
      
      fluidRow(
          column(width = 12,
                 valueBoxOutput(width = 4, "vb_trend"),
                 valueBoxOutput(width = 4, "vb_salary")
          )
      ),
      
      fluidRow(
          column(width = 12,
                 tabBox(
                     id = "tabbox",
                     width = 12,
                     tabPanel(title = "Search Trend",
                              div("Google Search Trends", class = "title_band"),
                              plotlyOutput("trend"),
                              plotOutput("map_region")),
                     tabPanel(title = "Salary",
                              plotlyOutput("salary_trend"))
                     )
                 )
          ),
      
      fluidRow(
          column(width = 12,
                 verbatimTextOutput("test"),
                 verbatimTextOutput("test2")
                 )
          )
          
      )
         
 )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$test <- renderText(input$filterstate)
    output$test2 <- renderText(length(input$filterstate))
    
    userfilter <- function(x) {
        x %>% filter(if(length(input$filterkeyword) != 0) {keyword %in% input$filterkeyword} else {TRUE})
    }

    # Value Box for Median Trend
    output$vb_trend <- renderValueBox(
        valueBox(
            value = 
                l_df_trend_independent$df_interest_over_time %>% 
                userfilter() %>%
                filter(date == max(date)) %>% 
                .$hits %>% 
                median(na.rm = TRUE),
            subtitle = paste("Current Trend", l_df_trend_independent$df_interest_over_time$date %>% max() %>% date()),
            icon = icon("chart-simple"),
            color = "navy")
    )
    
    
    # Value Box for Median Salary
    output$vb_salary <- renderValueBox(
        valueBox(
            value = 
                df_salary %>% 
                userfilter() %>%
                filter(year == this_year) %>% 
                .$usd_salary %>% 
                median(na.rm = TRUE) %>% 
                dollar(),
            subtitle = paste("Median Salary", this_year),
            icon = icon("dollar-sign"),
            color = "navy")
    )
    
    
    l_df_trend_relative$df_interest_over_time <- 
        l_df_trend_relative$df_interest_over_time %>%
        rename(Date = date, Popularity = hits)
    
    output$trend <- renderPlotly(
        ggplotly(
            l_df_trend_relative$df_interest_over_time %>% 
            userfilter() %>%
            ggplot(aes(x = Date, y = Popularity, color = keyword)) +
            # geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
            geom_line(linewidth = 1.3) +
            geom_point(data = l_df_trend_relative$df_interest_over_time %>% 
                           userfilter() %>%
                           filter(Date == max(Date))) +
            theme_bw() +
            scale_color_brewer(palette = "Blues", direction = 1, type = "div", name = "") +
            xlab("") +
            ylab("Popularity")) 
        # %>% layout(legend = list(orientation = "h", x = 0.5, y = -0.3))
        )
    
    
    df_hex_lon_lat <- read_csv("data/hex_lon_lat.csv")
    df_hex_centers <- read_csv("data/hex_centers.csv")
    
    # display.brewer.all(colorblindFriendly = TRUE)
    
    df_region <- 
        df_hex_lon_lat %>% 
        left_join(l_df_trend_relative$df_interest_by_region, by = c("state" = "location")) %>%
        rename(Popularity = hits)
         
    output$map_region <- renderPlot(
        df_region %>%
            userfilter() %>%
            group_by(long, lat, state) %>% 
            mutate(Popularity = mean(Popularity, na.rm = TRUE)) %>%
            ungroup() %>%
            ggplot(aes(x = long, y = lat, group = state)) +
            geom_polygon(aes(fill = Popularity), color = "white") +
            scale_fill_distiller(palette = "Blues", type = "seq", direction = 1) +
            geom_text(data = df_hex_centers, aes(x = long, y = lat, label = state), col = "white") +
            theme_void() +
            coord_map()
        )
    
    
    
    output$salary_trend <- renderPlotly(
        ggplotly(
            df_salary %>% 
                userfilter() %>%
                group_by(year, keyword) %>% 
                summarise(Median_Salary = median(usd_salary, na.rm = TRUE)) %>%
                ggplot(aes(x = year, y = Median_Salary, color = keyword)) +
                # geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
                geom_line(linewidth = 1.3) +
                geom_point(data = df_salary %>%
                               userfilter() %>%
                               filter(year == max(year)) %>%
                               group_by(year, keyword) %>% 
                               summarise(Median_Salary = median(usd_salary, na.rm = TRUE))
                           ) +
                theme_bw() +
                scale_color_brewer(palette = "Blues", direction = 1, type = "div", name = "") +
                xlab("") +
                ylab("Salary ($)")) 
        # %>% layout(legend = list(orientation = "h", x = 0.5, y = -0.3))
    )
    
    
    output$bls_salary_map_region <- renderPlot(
        df_region %>%
            userfilter() %>%
            group_by(long, lat, state) %>%
            mutate(Popularity = mean(Popularity, na.rm = TRUE)) %>%
            ungroup() %>%
            ggplot(aes(x = long, y = lat, group = state)) +
            geom_polygon(aes(fill = Popularity), color = "white") +
            scale_fill_distiller(palette = "Blues", type = "seq", direction = 1) +
            geom_text(data = df_hex_centers, aes(x = long, y = lat, label = state), col = "white") +
            theme_void() +
            coord_map()
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
