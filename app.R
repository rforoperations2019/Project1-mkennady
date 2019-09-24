library(shinydashboard)
library(shiny)
library(reshape2)
library(dplyr)
library(readr)
library(magrittr)
library(plotly)
library(shinythemes)



header <- dashboardHeader(title = "NAAEE Web Traffic Dashboard")
  
# see icons here: https://fontawesome.com/icons?d=gallery&q=bar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Website Traffic", tabName = "dashboard", icon = icon("chart-bar"),
             badgeLabel = "Staff", badgeColor = "light-blue"),
    menuItem("Website Analysis", tabName = "dashboard", icon = icon("sort-numeric-up"),
             badgeLabel = "Staff", badgeColor = "light-blue"),
    menuItem("eePRO Groups", tabName = "widgets", icon = icon("users"), 
             badgeLabel = "Moderators", badgeColor = "green")
  )
)

body <- dashboardBody()
  

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { 
  
  
  
  }

shinyApp(ui, server)