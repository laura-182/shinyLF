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
library(lubridate)

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
            fluidRow(
              column(6,
            checkboxGroupInput(inputId = "counties",
                               label = "Select counties:",
                               choices = c("Benton", "Linn", "Jones", "Johnson", "Washington", "Cedar", "Iowa"),
                               selected = c("Benton", "Linn", "Jones"))),
              column(6,
                     actionButton("CRMSA", "Cedar Rapids, IA MSA"),
                     br(),
                     br(),
                     actionButton("ICMSA", "Iowa City, IA MSA"),
                     ),),
           # https://shiny.rstudio.com/reference/shiny/0.14/dateRangeInput.html
             dateRangeInput(inputId = "daterange",
                           label = "Select date range",
                           format = "MM yyyy",
                           start = "2019-01-01", 
                           end = mostRecent,
                           min = leastRecent,
                           max = mostRecent)
        ),
    

        mainPanel(
           plotOutput("LFgraph"),
           verbatimTextOutput("testing2")
        )
    ),
    plotOutput("stackedbar")
    ),
    tableOutput("table")
    )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  react_begOfMonth <- reactive({floor_date(input$daterange[1], "month")})
  react_endOfMonth <- reactive({ceiling_date(input$daterange[2], "month") - 1})
  
  output$testing2 <- renderPrint({
 print(c(react_begOfMonth(), react_endOfMonth()))
  
  })
  
  # https://shiny.rstudio.com/reference/shiny/0.14/updateCheckboxGroupInput.html
  # https://shiny.rstudio.com/articles/action-buttons.html
  
  observeEvent(input$CRMSA, {
    updateCheckboxGroupInput(session, "counties",
                             selected = c("Benton", "Linn", "Jones"))
  })
  
  observeEvent(input$ICMSA, {
    updateCheckboxGroupInput(session, "counties",
                             selected = c("Johnson", "Washington"))
  })
  
  observe({
    if(length(input$counties) < 1){
      updateCheckboxGroupInput(session, "counties",
                               selected = c("Linn"))
    }
  })

 
  
  sum_fun <- function(x){
    return(data.frame(y = sum(x), label = sum(x)))
  }
  
  output$LFgraph <- renderPlot({
      # the way I did it before the reactive expressions above
      # begOfMonth <- floor_date(input$daterange[1], "month")
      # endOfMonth <- ceiling_date(input$daterange[2], "month") - 1
      dataset <- get(input$metric)
      dataForStatSumm <- 
        filter(dataset, date >= react_begOfMonth() & date <= react_endOfMonth()) %>% 
        filter(date == last(date)) %>%
        filter(county %in% input$counties)
      
        ggplot(dataset %>% 
                 filter(county %in% input$counties), 
                 aes(x = date, 
                     y = value)) + 
        # https://ggplot2tutor.com/tutorials/summary_statistics
        stat_summary(na.rm = TRUE, fun = "sum", geom = "line", size = 2) +
          # add a point at the end of the line
        stat_summary(na.rm = TRUE, fun = "sum", geom = "point", size = 5, 
                     data = dataForStatSumm) +
        # these lines are for debugging
          stat_summary(na.rm = TRUE, fun = "sum", geom = "point", size = 5,
                       data = 
                         filter(dataset, date >= react_begOfMonth() & date <= react_endOfMonth()) %>% 
                         filter(date == react_begOfMonth()) %>%
                         filter(county %in% input$counties)) +
          # end debugging
          
        # labels the last point
        stat_summary(na.rm = TRUE, fun = "sum", geom = "label", 
                       aes(label = scales::comma(sum(value))), # https://stackoverflow.com/questions/56490813/how-do-i-add-a-comma-separator-to-a-text-label-in-geom-text
                       size = 5, hjust = -0.2,
                       data = dataForStatSumm) +
        scale_x_date(limits = c(react_begOfMonth(), react_endOfMonth()),
                     date_breaks = "6 months",
                     date_labels = "%b %Y",
                     expand = expansion(mult = c(0,0.15))) + # https://www.rdocumentation.org/packages/ggplot2/versions/3.3.6/topics/expansion
        scale_y_continuous(labels = scales::comma) +
          ggtitle(unique(dataset$metric_name))
          
                
    })
    
  
    output$stackedbar <- renderPlot({
      
      stackeddata <- ICRIowaData %>%
        select(-footnote_codes, -period, -year) %>% 
        filter(!grepl("3$", series_id)) %>%
        mutate(metricname = case_when(
          grepl("6$", series_id) ~ "Labor Force",
          grepl("5$", series_id) ~ "Employment",
          grepl("4$", series_id) ~ "Unemployment"
        )) %>% 
        mutate(date = as.Date(as.character(date)))
      
      # This reverses the metrics in the chart.
      stackeddata$metricname <- as.factor(stackeddata$metricname) %>% fct_rev()
      
      # This is the first step in adjusting the y axes so it's easier to see the nuances in the data better
      y_axis_min <- stackeddata %>%
        filter(county %in% input$counties) %>%
        filter(date >= react_begOfMonth() & date <= react_endOfMonth()) %>% 
        filter(metricname == "Employment") %>% 
        group_by(date) %>% 
        summarise(total = sum(value)) %>% 
        ungroup() %>% 
        select(total) %>% 
        min()
      # This is the second step that takes 95% of the lowest value to add some buffer.
      lowestEmp <- y_axis_min * .95
      
      ggplot(
        stackeddata %>% 
          filter(!grepl("6$", series_id)) %>% # Filter out total labor force
          filter(county %in% input$counties), 
        aes(x = date,
            y = value)) +
          geom_col(aes(fill = metricname, group = metricname)) +
        # https://stackoverflow.com/questions/57359552/missing-only-one-bar-in-bar-chart
          scale_x_date(limits = c(react_begOfMonth() %m-% months(1), react_endOfMonth()),
                       date_breaks = "6 months",
                       date_labels = "%b %Y") +
        scale_y_continuous(labels = scales::comma) +
        coord_cartesian(ylim = c(lowestEmp,NA))
        # stat_summary(na.rm = TRUE, fun = "sum", geom = "text", 
        #              aes(label = date), # https://stackoverflow.com/questions/56490813/how-do-i-add-a-comma-separator-to-a-text-label-in-geom-text
        #              size = 5, angle = 90)
    
    })
    

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
