
# Overview dashboard of data careers in United States

# Library----

library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
# library(skimr)
# library(countrycode)
# library(geojsonio)
# library(rgdal)
# library(broom)
# library(rgeos)


# Data----

## BLS Data----
# Rerun following comment to update BLS data:
# source("data/bls/get bls data.R")
df_bls_data <- read_csv("data/bls/df_bls_data.csv")


## Kaggle Data----
# Rerun following comment to update Kaggle Salary data:
# source("data/kaggle/get kaggle data.R")
# df_kaggle_data <- read_csv("data/kaggle/df_kaggle_data.csv")

  
## GTrends Data----
# Rerun following comment to update GTrends data:
# source("data/gtrends/get gtrend data.R")
l_df_trend_relative <- read_rds(file = "data/gtrends/l_df_trend_relative.rds")
l_df_trend_independent <- read_rds(file = "data/gtrends/l_df_trend_independent.rds")



# this_year <- Sys.Date() %>% year()


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
                         # "size" = 6,
                         "noneSelectedText" = "All"
                         )
                     ),

                 pickerInput(
                     inputId = "filterstate",
                     label = "State",
                     choices = l_df_trend_relative$df_interest_by_region$state %>% unique() %>% sort(),
                     multiple = TRUE,
                     options = pickerOptions(
                         "actionsBox" = TRUE,
                         "liveSearch" = TRUE,
                         # "size" = 6,
                         "noneSelectedText" = "All"
                         )
                     )
          )
      ),
      
      

      
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
                              div("Salary (Bureau of Labor Statistics) ", class = "title_band"),
                              plotlyOutput("bls_salary_trend"),
                              plotOutput("bls_salary_map_region"))
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
    
    userfilterkeyword <- function(x) {
        x %>% filter(if(length(input$filterkeyword) != 0) {keyword %in% input$filterkeyword} else {TRUE})
    }
    
    userfilterstate <- function(x) {
        x %>% filter(if(length(input$filterstate) != 0) {state %in% input$filterstate} else {TRUE})
    }

    # Value Box for Median Trend
    output$vb_trend <- renderValueBox(
        valueBox(
            value = 
                l_df_trend_independent$df_interest_by_region %>% 
                userfilterkeyword() %>%
                userfilterstate() %>%
                .$Popularity %>% 
                median(na.rm = TRUE),
            subtitle = paste("Current Trend", l_df_trend_independent$df_interest_over_time$date %>% max() %>% date()),
            icon = icon("chart-simple"),
            color = "navy")
    )
    
    
    # Value Box for Median Salary
    output$vb_salary <- renderValueBox(
        valueBox(
            value = 
                df_bls_data %>% 
                userfilterkeyword() %>%
                userfilterstate() %>%
                filter(year == max(year)) %>% 
                .$Median_Salary %>% 
                as.numeric() %>%
                median(na.rm = TRUE) %>% 
                dollar(),
            subtitle = paste("Median Salary", max(df_bls_data$year)),
            icon = icon("dollar-sign"),
            color = "navy")
    )
    
    
    l_df_trend_relative$df_interest_over_time <- 
        l_df_trend_relative$df_interest_over_time %>%
        rename(Date = date, Popularity = hits)
    
    output$trend <- renderPlotly(
        ggplotly(
            l_df_trend_relative$df_interest_over_time %>% 
                userfilterkeyword() %>%
            ggplot(aes(x = Date, y = Popularity, color = keyword)) +
            # geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
            geom_line(linewidth = 1.3) +
            geom_point(data = l_df_trend_relative$df_interest_over_time %>% 
                           userfilterkeyword() %>%
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
        left_join(l_df_trend_relative$df_interest_by_region, by = c("state")) %>%
        left_join(df_bls_data %>% filter(year == max(year)), by = c("state", "keyword"))
         
    output$map_region <- renderPlot(
        df_region %>%
            userfilterkeyword() %>%
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
    
    
    
    output$bls_salary_trend <- renderPlotly(
        ggplotly(
            df_bls_data %>% 
                userfilterkeyword() %>%
                userfilterstate() %>%
                group_by(year, keyword) %>% 
                summarise(Median_Salary = median(as.numeric(Median_Salary), na.rm = TRUE)) %>%
                ggplot(aes(x = year, y = Median_Salary, color = keyword)) +
                # geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
                geom_line(linewidth = 1.3) +
                geom_point(data = df_bls_data %>%
                               userfilterkeyword() %>%
                               userfilterstate() %>%
                               filter(year == max(year)) %>%
                               group_by(year, keyword) %>% 
                               summarise(Median_Salary = median(as.numeric(Median_Salary), na.rm = TRUE))
                           ) +
                theme_bw() +
                scale_color_brewer(palette = "Blues", direction = 1, type = "div", name = "") +
                xlab("") +
                ylab("Salary ($)")) 
        # %>% layout(legend = list(orientation = "h", x = 0.5, y = -0.3))
    )
    
    
    output$bls_salary_map_region <- renderPlot(
        df_region %>%
            userfilterkeyword() %>%
            group_by(long, lat, state) %>%
            mutate(Median_Salary = mean(as.numeric(Median_Salary), na.rm = TRUE)) %>%
            ungroup() %>%
            ggplot(aes(x = long, y = lat, group = state)) +
            geom_polygon(aes(fill = Median_Salary), color = "white") +
            scale_fill_distiller(palette = "Blues", type = "seq", direction = 1) +
            geom_text(data = df_hex_centers, aes(x = long, y = lat, label = state), col = "white") +
            theme_void() +
            coord_map()
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
