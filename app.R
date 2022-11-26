library(tidyverse)
library(skimr)
library(kaggler)
library(gtrendsR)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)

# Data----

# AI/ML/Data Science Salary Data 
# https://ai-jobs.net/salaries/download/

# Authenticate to Kaggle API 
# This is Hidden in Github. Please Generate your own for reproducibility.
kaggler::kgl_auth(creds_file = "kaggle.json")

# Download prepped data from Kaggle
# https://www.kaggle.com/datasets/cedricaubin/ai-ml-salaries
df_salary <- kgl_datasets_download(
  owner_dataset = "cedricaubin/ai-ml-salaries",
  fileName = "salaries.csv",
  datasetVersionNumber = 2)

df_salary <-
  df_salary %>% 
  select(year = work_year,
         job_title,
         employment_type,
         experience_level,
         usd_salary = salary_in_usd,
         remote_ratio,
         employee_residence,
         company_location,
         company_size)




df_salary %>% 
  filter(company_location == "US") %>%
  group_by(year, job_title) %>%
  summarise(median_usd_salary = median(usd_salary, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = median_usd_salary)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~ job_title) +
  theme_bw()


# Goggle Trends Data
# https://support.google.com/trends/answer/4365533?hl=en

f_get_trend_data <- function() {
  
  env_get_trend_data <- environment()
  
  trend_datasets <- 
    c("df_interest_over_time", 
      "df_interest_by_country", 
      "df_interest_by_dma", 
      "df_interest_by_city", 
      "df_related_topics", 
      "df_related_queries")
  
  lapply(trend_datasets, function(x) {if(exists(x)){rm(list = x, envir = env_get_trend_data)}})
  
  topic_keywords <- c("Data Engineering",
                      "Data Analytics", 
                      "Data Science", 
                      "Artificial Intelligence", 
                      "Machine Learning",
                      # "Business Intelligence",
                      "Deep Learning")

  for(i in topic_keywords) {
    
    l_temp <- gtrends(keyword = i, geo = "", time = "all")
    
    for(i in trend_datasets){
      
      df_interest_over_time_temp <- l_temp$interest_over_time
      df_interest_by_country_temp <- l_temp$interest_by_country
      df_interest_by_dma_temp <- l_temp$interest_by_dma
      df_interest_by_city_temp <- l_temp$interest_by_city
      df_related_topics_temp <- l_temp$related_topics
      df_related_queries_temp <- l_temp$related_queries
      
      ifelse(exists(i),
             assign(i, eval(as.symbol(i)) %>% bind_rows(eval(as.symbol(paste0(i, "_temp"))))),
             assign(i, eval(as.symbol(paste0(i, "_temp")))))
    }
    
    # Sleep after each iteration so google doesn't block me
    Sys.sleep(2)
  }
  
  return(
    list(
      df_interest_over_time = df_interest_over_time, 
      df_interest_by_country = df_interest_by_country,
      df_interest_by_dma = df_interest_by_dma,
      df_interest_by_city = df_interest_by_city,
      df_related_topics = df_related_topics,
      df_related_queries = df_related_queries
      )
    )
}

# l_df_trend <- f_get_trend_data()
# l_df_trend %>% write_rds(file = "data/l_df_trend.rds")
l_df_trend <- read_rds("data/l_df_trend.rds")

l_df_trend$df_interest_over_time <- 
  l_df_trend$df_interest_over_time %>%
  rename(Date = date, Popularity = hits, Keyword = keyword) %>%
  filter(Keyword != "Business Intelligence")

plt_trend <- 
  l_df_trend$df_interest_over_time %>% 
  ggplot(aes(x = Date, y = Popularity, color = Keyword)) +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  geom_line(linewidth = 1.3) +
  geom_point(data = l_df_trend$df_interest_over_time %>% 
               filter(Date == max(Date))) +
  theme_bw() +
  scale_color_brewer(palette="RdBu", direction = 1, type = "div", name = "") +
  xlab("") +
  ylab("Popularity") +
  ggtitle("Google Search Trends")


# labs(x=NULL,
#      y="Popularity",
#      title = "Google Search Trends",
#      subtitle = "PlaceHolder",
#      caption  = "NOTE: PlaceHolder") +
#   theme(plot.caption = element_text(hjust = 0, face= "italic"),
#         plot.title.position = "plot", 
#         plot.caption.position =  "plot") +


ggplotly(plt_trend) %>%
  layout(title = list(text = paste0('Google Search Trends',
                                    '<br>',
                                    '<sup>',
                                    'Subtitle PlaceHolder',
                                    '</sup>')))

df_salary %>% 
  filter(year == 2022, company_location == "US") %>% 
  group_by(year, company_location, job_title) %>% 
  summarise(med_salary = median(usd_salary))




# Define UI
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "AI/ML/Data Salaries Dashboard"),

  # Sidebar with a slider input for number of bins 
  dashboardSidebar(
    useShinyjs(),
      sidebarMenu(
          pickerInput(
            inputId = "filterholder",
            label = "Place Holder",
            choices = c("Holder One", "Holder Two"),
            multiple = TRUE,
            options = list(
              "actions-box" = TRUE,
              "live-search" = TRUE,
              "size" = 10,
              "non-selected-text" = "All"
            )
      ))),

      # Show a plot of the generated distribution
      dashboardBody(
         plotlyOutput("trend")
      )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$trend <- renderPlotly({
      plt_trend
    })
    
    
    
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
