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
                   label = "Date range",
                   start = as.Date("2019-08-01"), end = as.Date("2019-08-31"),
                   min = as.Date("2019-01-01"), max = as.Date("2019-09-22"),
                   format = "mm-dd-yy", startview = 'month'),
    textInput("text",
              label = "URL search",
              value = "groups"),
    checkboxInput(inputId = "previous",label = "Compare to previous period")))

# Reference: https://rstudio.github.io/shinydashboard/structure.html#boxes
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "pages",
            fluidRow(
              box(title = "Page Information Table", DT::dataTableOutput(outputId = "datatable"), width = 12))),
    tabItem(tabName = "pageviews",
            fluidRow(
              box(width = 6, plotlyOutput(outputId = "pageviews")),
              box(width = 6, conditionalPanel("input.previous", 
                               column(width = 12, plotlyOutput(outputId = "pageviews.previous"))))),
            fluidRow(infoBoxOutput("pageviewsTotal", width = 3), 
                     infoBoxOutput("bounceAvg", width = 3))),
    tabItem(tabName = "users",
        fluidRow(
          box(width = 6, plotlyOutput(outputId = "users")),
          box(width = 6, conditionalPanel("input.previous", 
                                column(width = 12, plotlyOutput(outputId = "users.previous"))))),
        fluidRow(infoBoxOutput("usersTotal", width = 3), 
                 infoBoxOutput("usersAvg", width = 3)))))

ui <- dashboardPage(header, sidebar, body, skin = "green")

server <- function(input, output) { 
  
  #Create dataset given user's inputs
  GA.data <- reactive({
    req(input$date, input$text)
    subset(GA2019, GA2019$date >= input$date[1] & 
                   GA2019$date <= input$date[2] & 
                   (str_detect(GA2019$pagePath, tolower(input$text), negate=FALSE) == TRUE))
  })
  
  # Create dataset for previous period
  GA.data.previous <- reactive({
    req(input$date, input$text, input$previous)
    date.difference <- input$date[2] - input$date[1] + 1
    if (input$previous) {
      subset(GA2019, GA2019$date >= input$date[1] - date.difference & 
               GA2019$date <= input$date[2] - date.difference & 
               (str_detect(GA2019$pagePath, tolower(input$text), negate=FALSE) == TRUE))
    }
  })
  
  # Data Table ----
  output$datatable <- 
    DT::renderDataTable({
      DT::datatable(data = GA.data(), options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = 400
      ), colnames = c("Date", "URL", "Page Title", "Users", "Pageviews", "Sessions", "Bounce Rate", "Exit Rate", "Avg Time on Page", "Entrances"))
    })
  
  # info box for Total Pageviews ----  
  output$pageviewsTotal <- renderInfoBox({
    sum <- format(sum(GA.data()$pageviews, na.rm = T), big.mark = ",")
    infoBox("Total pageviews", value = sum,
            icon = icon("file"),
            color = "aqua")
  })
  
  # info box for Average Bounce Rate ----
  output$bounceAvg <- renderInfoBox({
    avg <- round(mean(GA.data()$bounceRate), digits = 1)
    avg <- paste(avg, "%")
    infoBox("Average Bounce Rate", value = avg,
            icon = icon("percent"),
            color = "green")
  })
  
  # info box for Total Users ----
  output$usersTotal <- renderInfoBox({
    sum <- format(sum(GA.data()$users, na.rm = TRUE), big.mark = ",")
    infoBox("Total users", value = sum,
            icon = icon("users"),
            color = "aqua")
  })
  
  # info box for Average Number of Users ----
  output$usersAvg <- renderInfoBox({
    sum <- round(sum(GA.data()$users, na.rm = TRUE)/nrow(GA.data()), digits = 1)
    infoBox("Avg users per page", value = sum,
            icon = icon("file-alt"),
            color = "green")
  })
  
  # Pageviews graph
  #Reference: http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
  output$pageviews <- renderPlotly({
    data <- GA.data()[,colnames(GA.data()) == "date"|colnames(GA.data()) == "pageviews"]
    ggplotly(
      ggplot(data, aes(x=date, y=pageviews)) + 
        geom_bar(stat="identity", fill = "#087795", position = "identity") + 
        labs(title = "Pageviews") +
        theme(
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray80"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray80"),
          axis.title.x = element_blank()
        ) +
        scale_y_continuous(name = "Pageviews")
    )
  })
  
  # Pageviews graph for previous period
  output$pageviews.previous <- renderPlotly({
    data <- GA.data.previous()[,colnames(GA.data.previous()) == "date"|colnames(GA.data.previous()) == "pageviews"]
    ggplotly(
      ggplot(data, aes(x=date, y=pageviews)) + 
        geom_bar(stat="identity", fill = "#087795", position = "identity") + 
        labs(title = "Pageviews") +
        theme(
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray80"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray80"),
          axis.title.x = element_blank()
        ) +
        scale_y_continuous(name = "Pageviews") 
    )
  })
  
  #Users graph
  output$users <- renderPlotly({
    data <- GA.data()[,colnames(GA.data()) == "date"|colnames(GA.data()) == "users"]
    ggplotly(
      ggplot(data, aes(x=date, y=users)) + 
        geom_bar(stat="identity", fill = "#087795", position = "identity") + 
        labs(title = "Users") +
        theme(
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray80"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray80"),
          axis.title.x = element_blank() +
            scale_y_continuous(name = "Users")
        ) 
    )
  })
  
  # Users graph for previous period
  output$users.previous <- renderPlotly({
    data <- GA.data.previous()[,colnames(GA.data.previous()) == "date"|colnames(GA.data.previous()) == "users"]
    ggplotly(
      ggplot(data, aes(x=date, y=users)) + 
        geom_bar(stat="identity", fill = "#087795", position = "identity") + 
        labs(title = "Users") +
        theme(
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray80"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray80"),
          axis.title.x = element_blank()
        ) +
        scale_y_continuous(name = "Users")
    )
  })
  
  }

shinyApp(ui, server)
