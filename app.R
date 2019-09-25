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
              value = "jobs"),
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
              if (textOutput(outputId = "previous") == "checked") {
                column(title = "Pageviews", plotOutput(outputId = "pageviews"), width = 3)
                column(title = "Bar chart", plotOutput(outputId = "pageviews.previous"), width = 3)
              } else {
                box(title = "Bar chart", plotOutput(outputId = "pageviews"), width = 6)
              }

            )
    )
)
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { 
  
  output$previous <- reactive({
    if(input$previous) {
      "checked"
    }
  })
  
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
  

  output$pageviews.previous <- renderPlot({
    ggplot(GA.data.previous(), aes(x=date)) + 
      geom_bar()
  })
  
  output$pageviews <- renderPlot({
    ggplot(GA.data(), aes(x=date)) + 
      geom_bar()
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