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
    menuItem("Pageviews", tabName = "pageviews", icon = icon("chart-bar")),
    menuItem("Users", tabName = "users", icon = icon("users")),
    dateRangeInput("date", 
                   label = "Input date range",
                   start = as.Date("2019-08-01"), end = as.Date("2019-08-31"),
                   min = as.Date("2019-01-01"), max = as.Date("2019-09-22"),
                   format = "mm/dd/yy", startview = 'month'),
    textInput("text",
              label = "Input URL search here",
              value = "naaee"),
    checkboxInput(inputId = "previous",label = "Compare to previous period")

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
                               column(width = 12, plotOutput(outputId = "pageviews.previous"))))
              ),
            fluidRow(infoBoxOutput("pageviewsTotal", width = 3), infoBoxOutput("pageviewsPages", width = 3)),
            fluidRow(infoBoxOutput("pageviewsAvg"), width = 3)
    ),
tabItem(tabName = "users",
        fluidRow(
          box(width = 6, plotOutput(outputId = "users")),
          box(width = 6, conditionalPanel("input.previous", 
                                          column(width = 12, plotOutput(outputId = "users.previous"))))),
        fluidRow(infoBoxOutput("usersTotal", width = 3), infoBoxOutput("usersPages", width = 3)),
        fluidRow(infoBoxOutput("usersAvg"), width = 3)
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
    date.difference <- input$date[2] - input$date[1] + 1
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
  

  # info box ----  
  output$pageviewsTotal <- renderInfoBox({
    sum <- format(sum(GA.data()$pageviews, na.rm = T), big.mark = ",")
    infoBox("Total pageviews", value = sum,
            icon = icon("file"),
            color = "aqua")
  })
  
  # info box ----  
  output$pageviewsPages <- renderInfoBox({
    sum <- format(nrow(GA.data()), big.mark = ",")
    infoBox("Total pages", value = sum,
            icon = icon("file"),
            color = "green")
  })
  
  # info box ----
  output$pageviewsAvg <- renderInfoBox({
    sum <- round(sum(GA.data()$pageviews, na.rm = TRUE)/nrow(GA.data()), digits = 1)
    infoBox("Avg users per page", value = sum,
            icon = icon("file-alt"),
            color = "green")
  })
  
  # info box ----
  output$usersTotal <- renderInfoBox({
    sum <- format(sum(GA.data()$users, na.rm = TRUE), big.mark = ",")
    infoBox("Total users", value = sum,
            icon = icon("users"),
            color = "aqua")
  })
  
  # info box ----
  output$usersPages <- renderInfoBox({
    sum <- format(nrow(GA.data()), big.mark = ",")
    infoBox("Total pages", value = sum,
            icon = icon("file"),
            color = "green")
  })
  
  # info box ----
  output$usersAvg <- renderInfoBox({
    sum <- round(sum(GA.data()$users, na.rm = TRUE)/nrow(GA.data()), digits = 1)
    infoBox("Avg users per page", value = sum,
            icon = icon("file-alt"),
            color = "green")
  })
  
  output$pageviews <- renderPlot({
    data <- GA.data()[,colnames(GA.data()) == "date"|colnames(GA.data()) == "pageviews"]
    ggplot(data, aes(x=date, y=pageviews)) + 
      geom_bar(stat="identity") + 
      ggtitle(paste("Pageviews from", input$date[1], "to", input$date[2]))
  })
  
  output$pageviews.previous <- renderPlot({
    data <- GA.data.previous()[,colnames(GA.data.previous()) == "date"|colnames(GA.data.previous()) == "pageviews"]
    ggplot(data, aes(x=date, y=pageviews)) + 
      geom_bar(stat="identity") + 
      ggtitle(paste("Pageviews from", min(GA.data.previous()$date), "to", max(GA.data.previous()$date)))
  })
  
  output$users <- renderPlot({
    data <- GA.data()[,colnames(GA.data()) == "date"|colnames(GA.data()) == "users"]
    ggplot(data, aes(x=date, y=users)) + 
      geom_bar(stat="identity") + 
      ggtitle(paste("Users from", input$date[1], "to", input$date[2]))
  })
  
  output$users.previous <- renderPlot({
    data <- GA.data.previous()[,colnames(GA.data.previous()) == "date"|colnames(GA.data.previous()) == "users"]
    ggplot(data, aes(x=date, y=users)) + 
      geom_bar(stat="identity") + 
      ggtitle(paste("Users from", min(GA.data.previous()$date), "to", max(GA.data.previous()$date)))
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