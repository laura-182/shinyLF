#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dygraphs)

source("global.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Labor Force by County"),

    # Sidebar with a slider input for number of bins 
    verticalLayout(
    sidebarLayout(
        sidebarPanel(

            selectInput(inputId = "metric",
                        label = "Select metric:",
                        choices = c("Labor Force" = "LF_ICRIowa",
                                    "Employment" = "Emp_ICRIowa",
                                    "Unemployment" = "Unemp_ICRIowa"),
                          
                        ),
            checkboxGroupInput(inputId = "counties",
                               label = "Select counties:",
                               choices = c("Benton", "Linn", "Jones", "Johnson", "Washington", "Cedar", "Iowa"),
                               selected = c("Benton", "Linn", "Jones")),
            dateRangeInput(inputId = "daterange",
                           label = "Select date range",
                           format = "MM yyyy",
                           start = "2019-01-01")
        ),
    

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("LFgraph"),
           textOutput("testing")
        )
    ),
    plotOutput("stackedbar")
    ),
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$testing2 <- renderText({
   lubridate::ymd(input$daterange[1])
  })
  sum_fun <- function(x){
    
    return(data.frame(y = sum(x), label = sum(x)))
  }
    output$LFgraph <- renderPlot({
        dataset <- get(input$metric)
        ggplot(dataset %>% 
                 filter(county %in% input$counties), 
                 aes(x = date, 
                     y = value)) + 
        # geom_line() +
        stat_summary(na.rm = TRUE, fun = "sum", geom = "line", size = 2) +
        stat_summary(na.rm = TRUE, fun = "sum", geom = "point", size = 5, 
                     data = filter(dataset, date == max(date)) %>% 
                       filter(county %in% input$counties)) +
          stat_summary(na.rm = TRUE, fun = "sum", geom = "label", 
                       aes(label = scales::comma(sum(value))), # https://stackoverflow.com/questions/56490813/how-do-i-add-a-comma-separator-to-a-text-label-in-geom-text
                       size = 5, hjust = -0.2,
                       data = filter(dataset, date == last(date)) %>%
                         filter(county %in% input$counties)) +
                  # stat_summary(fun.data = sum_fun, geom = "label") +
        scale_x_date(limits = input$daterange,
                     date_breaks = "6 months",
                     date_labels = "%b %Y",
                     expand = expansion(mult = c(0,0.1))) +
        scale_y_continuous(labels = scales::comma) +
          ggtitle(unique(dataset$metric_name))
          
                
    })
    
    output$stackedbar <- renderPlot({
      stackeddata <- ICRIowaData %>%
        filter(!grepl("3$", series_id)) %>%
        mutate(metricname = case_when(
          grepl("6$", series_id) ~ "Labor Force",
          grepl("5$", series_id) ~ "Employment",
          grepl("4$", series_id) ~ "Unemployment"
        ))
      stackeddata$metricname <- as.factor(stackeddata$metricname) %>% fct_rev()
      y_axis_min <- stackeddata %>%
        filter(county %in% input$counties) %>%
        filter(date >= input$daterange[1] & date <= input$daterange[2]) %>% 
        filter(metricname == "Employment") %>% 
        group_by(date) %>% 
        summarise(total = sum(value)) %>% 
        ungroup() %>% 
        select(total) %>% 
        min()
      lowestEmp <- y_axis_min * .95
      ggplot(
        stackeddata %>% 
          filter(!grepl("6$", series_id)) %>% 
          filter(county %in% input$counties), 
        aes(x = date,
            y = value)) +
          geom_col(aes(fill = metricname)) +
          scale_x_date(limits = input$daterange,
                       date_breaks = "6 months",
                       date_labels = "%b %Y") +
        scale_y_continuous(labels = scales::comma) +
        coord_cartesian(ylim = c(lowestEmp,NA))
      
     
        
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
