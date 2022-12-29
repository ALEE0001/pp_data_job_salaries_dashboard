
# Overview dashboard of data careers in United States
# Author: Alex Lee

# Library----

library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(mapproj)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)


# Data----

## BLS Data----
# Rerun following comment to update BLS data:
# source("data/bls/get bls data.R")
df_bls_data <- read_csv("data/bls/df_bls_data.csv")
df_bls_projection <- read_csv("data/bls/df_bls_projection.csv", locale=locale(encoding="latin1"))
  
## GTrends Data----
# Rerun following comment to update GTrends data:
# source("data/gtrends/get gtrend data.R")
l_df_trend_relative <- read_rds(file = "data/gtrends/l_df_trend_relative.rds")
l_df_trend_independent <- read_rds(file = "data/gtrends/l_df_trend_independent.rds")

# Build Hex Map Components, to be used in server logic. 
df_hex_lon_lat <- read_csv("data/hex_lon_lat.csv")
df_hex_centers <- read_csv("data/hex_centers.csv")

df_region <- 
    df_hex_lon_lat %>% 
    left_join(l_df_trend_relative$df_interest_by_region, by = c("state")) %>%
    left_join(df_bls_data %>% filter(Year == max(Year)), by = c("state", "Keyword"))

# Define UI----
ui <- dashboardPage(
    
  skin = "black",
    
  # Application title
  dashboardHeader(title = "US Data Careers",
                  dropdownMenu(type = "messages",
                               messageItem(
                                   from = "Published Date",
                                   message = "2022-12-27",
                                   icon = icon("calendar")
                               ),
                               messageItem(
                                   from = "Author",
                                   message = "Alex Lee"
                               ),
                               messageItem(
                                   from = "Contact Me",
                                   message = "alexmize@my.unt.edu",
                                   icon = icon("envelope")
                               ),
                               messageItem(
                                   from = "Portfolio",
                                   message = "Click Here",
                                   href = "https://alee0001.github.io",
                                   icon = icon("pen")
                               ),
                               messageItem(
                                   from = "Github",
                                   message = "Click Here",
                                   href = "https://github.com/ALEE0001/pp_us_data_job_dashboard",
                                   icon = icon("github")
                               )
                  )),

  dashboardSidebar(disable = TRUE),

  dashboardBody(
      
      tags$style(".fa-chart-simple {color:#FFFFFF}"),
      tags$style(".fa-person-walking-luggage {color:#FFFFFF}"),
      tags$style(".fa-seedling {color:#FFFFFF}"),
      tags$style(".fa-dollar-sign {color:#FFFFFF}"),
      
      
      includeCSS(path = "www/general.css"),
      useShinyjs(),
      
      fluidRow(
          column(width = 12,
                 box(status = "primary",
                     width = 12,
                     column(width = 6,
                            pickerInput(
                                inputId = "filterkeyword",
                                label = "Keyword Filter",
                                choices = l_df_trend_relative$df_interest_over_time$Keyword %>% unique() %>% sort(),
                                multiple = TRUE,
                                options = pickerOptions(
                                    "actionsBox" = TRUE,
                                    "liveSearch" = TRUE,
                                    "size" = 6,
                                    "noneSelectedText" = "All"
                                    )
                                )
                            ),
                     
                     column(width = 6,
                            pickerInput(
                                inputId = "filterstate",
                                label = "State Filter",
                                choices = l_df_trend_relative$df_interest_by_region$state %>% unique() %>% sort(),
                                multiple = TRUE,
                                options = pickerOptions(
                                    "actionsBox" = TRUE,
                                    "liveSearch" = TRUE,
                                    "size" = 6,
                                    "noneSelectedText" = "All"
                                    )
                                )
                            )
                     )
                 )
          ),
      
      fluidRow(
          column(width = 12,
                 valueBoxOutput(width = 3, "vb_trend"),
                 valueBoxOutput(width = 3, "vb_employment"),
                 valueBoxOutput(width = 3, "vb_employment_projection"),
                 valueBoxOutput(width = 3, "vb_salary")
                 
          )
      ),
      
      fluidRow(
          column(width = 12,
                 tabBox(
                     id = "tabbox",
                     width = 12,
                     tabPanel(title = "Search Trend",
                              div("Google Search, Relative Trends", class = "title_band"),
                              plotlyOutput("trend"),
                              plotOutput("trend_map")),
                     tabPanel(title = "Employment",
                              div("Employment (Bureau of Labor Statistics)", class = "title_band"),
                              plotlyOutput("bls_emp_count_trend"),
                              plotOutput("bls_emp_count_map_region")),
                     tabPanel(title = "Salary",
                              div("Salary (Bureau of Labor Statistics)", class = "title_band"),
                              plotlyOutput("bls_salary_trend"),
                              plotOutput("bls_salary_map_region"))

                     )
                 )
          ),
      
      # fluidRow(
      #     column(width = 12,
      #            verbatimTextOutput("test"),
      #            verbatimTextOutput("test2")
      #            )
      #     )
          
      )
         
 )

# Define Server----
server <- function(input, output) {
    
    # output$test <- renderText(input$filterstate)
    # output$test2 <- renderText(length(input$filterstate))
    
    
    ## User Filters----
    userfilterkeyword <- function(x) {
        x %>% filter(if(length(input$filterkeyword) != 0) {Keyword %in% input$filterkeyword} else {TRUE})
    }
    
    userfilterstate <- function(x) {
        x %>% filter(if(length(input$filterstate) != 0) {state %in% input$filterstate} else {TRUE})
    }
    
    userfilterstate_w_us <- function(x) {
        x %>% filter(if(length(input$filterstate) != 0) {state %in% input$filterstate} else {state == "All"})
    }

    ## Value Boxes----
    
    # Value Box for Median Trend
    output$vb_trend <- renderValueBox(
        valueBox(
            value = 
                l_df_trend_independent$df_interest_over_time %>% 
                userfilterkeyword() %>%
                userfilterstate_w_us() %>%
                filter(Date == max(Date)) %>%
                .$Popularity %>%
                median(na.rm = TRUE) %>%
                paste0(., " / ", "100"),
            subtitle = paste("Google Trend", l_df_trend_independent$df_interest_over_time$Date %>% max() %>% date()),
            icon = icon("chart-simple"),
            color = "navy")
    )
    
    # Value Box for Employment
    output$vb_employment <- renderValueBox(
        valueBox(
            value = 
                df_bls_data %>% 
                filter(Keyword != "All US Occupations") %>%
                userfilterkeyword() %>%
                userfilterstate() %>%
                filter(Year == max(Year)) %>% 
                .$TOT_EMP %>% 
                as.numeric() %>%
                sum(na.rm = TRUE) %>% 
                number(),
            subtitle = paste("Employed Jobs", max(df_bls_data$Year)),
            icon = icon("person-walking-luggage"),
            color = "navy")
    )
    
    # Value Box for Employment Projection
    output$vb_employment_projection <- renderValueBox(
        valueBox(
            value =
                df_bls_projection %>%
                filter(Keyword != "All US Occupations") %>%
                userfilterkeyword() %>%
                .$employment_projection %>%
                as.numeric() %>%
                median(na.rm = TRUE) %>%
                paste0("%"),
            subtitle = paste("Job Growth Next 10 Yrs"),
            icon = icon("seedling"),
            color = "navy")
    )
    
    # Value Box for Median Salary
    output$vb_salary <- renderValueBox(
        valueBox(
            value = 
                df_bls_data %>% 
                filter(Keyword != "All US Occupations") %>%
                userfilterkeyword() %>%
                userfilterstate() %>%
                filter(Year == max(Year)) %>% 
                .$Median_Salary %>% 
                as.numeric() %>%
                median(na.rm = TRUE) %>% 
                dollar(),
            subtitle = paste("Median Base Salary", max(df_bls_data$Year)),
            icon = icon("dollar-sign"),
            color = "navy")
    )
    
    ## Line & HexMap Plots----
    # Google Trend Charts
    output$trend <- renderPlotly(
        ggplotly(
            l_df_trend_relative$df_interest_over_time %>% 
            userfilterkeyword() %>%
            userfilterstate_w_us() %>%
            group_by(Date, Keyword) %>%
            mutate(Popularity = median(Popularity, na.rm = TRUE)) %>%
            ggplot(aes(x = Date, y = Popularity, color = Keyword)) +
            geom_line(linewidth = 1.3) +
            geom_point(data = l_df_trend_relative$df_interest_over_time %>% 
                           userfilterkeyword() %>%
                           userfilterstate_w_us() %>%
                           filter(Date == max(Date)) %>%
                           group_by(Keyword) %>%
                           mutate(Popularity = median(Popularity, na.rm = TRUE))) +
            theme_bw() +
            scale_color_brewer(palette = "Paired", direction = 1, type = "div", name = "") +
            xlab("") +
            ylab("Comparative Popularity")) %>%
            layout(legend = list(orientation = "h", x = 1, xanchor = "right"))
        )
    
    # display.brewer.all(colorblindFriendly = TRUE)
         
    output$trend_map <- renderPlot(
        df_region %>%
            userfilterkeyword() %>%
            group_by(long, lat, state) %>% 
            mutate(Popularity = median(Popularity, na.rm = TRUE)) %>%
            ungroup() %>%
            ggplot(aes(x = long, y = lat, group = state)) +
            geom_polygon(aes(fill = Popularity), color = "white") +
            scale_fill_distiller(palette = "Blues", type = "seq", direction = 1) +
            geom_text(data = df_hex_centers, aes(x = long, y = lat, label = state), col = "white") +
            theme_void() +
            coord_map()
        )
    
    # Employment Charts
    output$bls_emp_count_trend <- renderPlotly(
        
        suppressWarnings({ 
            
            ggplotly(
                
                df_bls_data %>% 
                    filter(Keyword != "All US Occupations") %>%
                    userfilterkeyword() %>%
                    userfilterstate() %>%
                    group_by(Year, Keyword) %>% 
                    summarise(TOT_EMP = sum(as.numeric(TOT_EMP), na.rm = TRUE)) %>%
                    ggplot(aes(x = Year, y = TOT_EMP, color = Keyword)) +
                    geom_line(linewidth = 1.3) +
                    geom_point(data = df_bls_data %>%
                                   filter(Keyword != "All US Occupations") %>%
                                   userfilterkeyword() %>%
                                   userfilterstate() %>%
                                   filter(Year == max(Year)) %>%
                                   group_by(Year, Keyword) %>% 
                                   summarise(TOT_EMP = sum(as.numeric(TOT_EMP), na.rm = TRUE))
                    ) +
                    theme_bw() +
                    scale_color_brewer(palette = "Paired", direction = 1, type = "div", name = "") +
                    xlab("") +
                    ylab("Number of Employed Jobs")) %>%
                layout(legend = list(orientation = "h", x = 1, xanchor = "right"))
        })
    )
    
    output$bls_emp_count_map_region <- renderPlot(
        
        suppressWarnings({
            
            df_region %>%
                filter(Keyword != "All US Occupations") %>%
                userfilterkeyword() %>%
                group_by(long, lat, state) %>%
                mutate(TOT_EMP = mean(as.numeric(TOT_EMP), na.rm = TRUE)) %>%
                ungroup() %>%
                ggplot(aes(x = long, y = lat, group = state)) +
                geom_polygon(aes(fill = TOT_EMP), color = "white") +
                scale_fill_distiller(palette = "Blues", type = "seq", direction = 1) +
                geom_text(data = df_hex_centers, aes(x = long, y = lat, label = state), col = "white") +
                theme_void() +
                coord_map()
        })
    )
    
    # Salary Charts
    output$bls_salary_trend <- renderPlotly(
        
        suppressWarnings({    
        
        ggplotly(
            df_bls_data %>% 
                filter(Keyword != "All US Occupations") %>%
                userfilterkeyword() %>%
                userfilterstate() %>%
                group_by(Year, Keyword) %>% 
                summarise(`10th_Percentile_Salary ($)` = median(as.numeric(A_PCT10), na.rm = TRUE),
                          `Median_Salary ($)` = median(as.numeric(Median_Salary), na.rm = TRUE),
                          `90th_Percentile_Salary ($)` = median(as.numeric(A_PCT90), na.rm = TRUE)) %>%
                ggplot(aes(x = Year, y = `Median_Salary ($)`, color = Keyword)) +
                # geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
                geom_line(linewidth = 1.3, aes(label = `10th_Percentile_Salary ($)`, label2 = `90th_Percentile_Salary ($)`)) +
                geom_point(data = df_bls_data %>%
                               userfilterkeyword() %>%
                               userfilterstate() %>%
                               filter(Year == max(Year)) %>%
                               group_by(Year, Keyword) %>% 
                               summarise(`10th_Percentile_Salary ($)` = median(as.numeric(A_PCT10), na.rm = TRUE),
                                         `Median_Salary ($)` = median(as.numeric(Median_Salary), na.rm = TRUE),
                                         `90th_Percentile_Salary ($)` = median(as.numeric(A_PCT90), na.rm = TRUE)),
                           aes(label = `10th_Percentile_Salary ($)`, label2 = `90th_Percentile_Salary ($)`)
                           ) +
                
                # All Jobs
                geom_line(data = df_bls_data %>%
                               filter(Keyword == "All US Occupations") %>%
                               userfilterkeyword() %>%
                               userfilterstate() %>%
                               group_by(Year, Keyword) %>% 
                               summarise(`10th_Percentile_Salary ($)` = median(as.numeric(A_PCT10), na.rm = TRUE),
                                         `Median_Salary ($)` = median(as.numeric(Median_Salary), na.rm = TRUE),
                                         `90th_Percentile_Salary ($)` = median(as.numeric(A_PCT90), na.rm = TRUE)),
                          aes(x = Year, y = `Median_Salary ($)`, label = `10th_Percentile_Salary ($)`, label2 = `90th_Percentile_Salary ($)`),
                          linewidth = 1.3,
                          linetype = "dashed"
                ) +
                
                theme_bw() +
                scale_color_brewer(palette = "Paired", direction = 1, type = "div", name = "") +
                xlab("") +
                ylab("Base Salary ($)")) %>%
                layout(legend = list(orientation = "h", x = 1, y = -0.2, xanchor = "right"))
        })
    )
    
    output$bls_salary_map_region <- renderPlot(
        
        suppressWarnings({
        
        df_region %>%
            filter(Keyword != "All US Occupations") %>%
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
        })
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
