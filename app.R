library(shinydashboard)
library(shiny)
library(reshape2)
library(dplyr)
library(readr)
library(stringr)
library(magrittr)
library(plotly)
library(shinythemes)
library(ggplot2)
library(DT)

#https://rstudio.github.io/shinydashboard/structure.html#boxes

GA2019 <- read_csv("data/2019_NAAEE_GoogleAnalytics.csv")

header <- dashboardHeader(title = "NAAEE Web Traffic")
  
# see icons here: https://fontawesome.com/icons?d=gallery&q=bar
# reference: https://shiny.rstudio.com/gallery/date-and-date-range.html
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Pages", tabName = "pages", icon = icon("file")),
    menuItem("Pageviews/Sessions", tabName = "pageviews", icon = icon("chart-bar")),
    menuItem("Users", tabName = "users", icon = icon("users")),
    dateRangeInput("date", 
                   label = "Input date range",
                   start = as.Date("2019-08-01"), end = as.Date("2019-08-31"),
                   min = as.Date("2019-01-01"), max = as.Date("2019-09-22"),
                   format = "mm/dd/yy", startview = 'month'),
    textInput("text",
              label = "Input URL search here",
              value = "naaee"),
    checkboxInput(inputId = "previous",label = "Compare previous period")

  )
)





body <- dashboardBody(
  tabItems(
    tabItem(tabName = "pages",
    fluidRow(
    box(title = "Data Table", DT::dataTableOutput(outputId = "datatable"))
    )
),
    tabItem(tabName = "pageviews",
            fluidRow(
              box(width = 6, plotOutput(outputId = "pageviews")),
              box(width = 6, conditionalPanel("input.previous", 
                               column(width = 12, plotOutput(outputId = "pageviews.previous")))))
    ),
tabItem(tabName = "users",
        fluidRow(
          box(width = 6, plotOutput(outputId = "users")),
          box(width = 6, conditionalPanel("input.previous", 
                                          column(width = 12, plotOutput(outputId = "users.previous")))))
)
)
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { 
  
  output$previousCheck <- reactive({
    if (input$previous == FALSE) {
      "test4"
    }
  })
  
  #outputOptions(output, "previousCheck", suspendWhenHidden = FALSE)
  
  GA.data <- reactive({
    req(input$date, input$text)
    subset(GA2019, GA2019$date >= input$date[1] & 
                   GA2019$date <= input$date[2] & 
                   (str_detect(GA2019$pagePath, tolower(input$text), negate=FALSE) == TRUE))
  })
  
  GA.data.previous <- reactive({
    req(input$date, input$text, input$previous)
    date.difference <- input$date[2] - input$date[1]
    if (input$previous) {
      subset(GA2019, GA2019$date >= input$date[1] - date.difference & 
               GA2019$date <= input$date[2] - date.difference & 
               (str_detect(GA2019$pagePath, tolower(input$text), negate=FALSE) == TRUE))
    }
    
  })
  
  
  output$datatable <- 
    DT::renderDataTable(
      DT::datatable(data = GA.data()#,
                    # rownames = c("Date", "Page Path", "Page Title", "Users", "Pageviews", "Sessions", "Bounce Rate", "Exit Rate"))
    ))
  

  # output$pageviews.previous <- renderPlot({
  #   ggplot(GA.data.previous(), aes(x=date)) + 
  #     geom_bar()
  # })
  # 
  # output$pageviews <- renderPlot({
  #   ggplot(GA.data(), aes(x=date)) + 
  #     geom_bar()
  # })
  
  
  output$pageviews <- renderPlot({
    data <- GA.data()[,colnames(GA.data()) == "date"|colnames(GA.data()) == "pageviews"]
    ggplot(data, aes(x=date)) + 
      geom_histogram()
  })
  
  output$pageviews.previous <- renderPlot({
    data <- GA.data.previous()[,colnames(GA.data.previous()) == "date"|colnames(GA.data.previous()) == "pageviews"]
    ggplot(data, aes(x=date)) + 
      geom_histogram()
  })
  
  
  
  output$users <- renderPlot({
    data <- GA.data()[,colnames(GA.data()) == "date"|colnames(GA.data()) == "users"]
    ggplot(data, aes(x=date)) + 
      geom_histogram()
  })
  
  
  output$users.previous <- renderPlot({
    data <- GA.data.previous()[,colnames(GA.data.previous()) == "date"|colnames(GA.data.previous()) == "users"]
    ggplot(data, aes(x=date)) + 
      geom_histogram()
  })
  
  
  
  
  }

shinyApp(ui, server)




# downloadButton(outputId = "download",
#               label = "Download data")



# output$download <- downloadHandler(
#   filename = function() {
#     paste('')
#   },
#   content = function(con) {
#     write_csv(exports, con)
#   }
# )