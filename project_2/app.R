library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(zoo)
library(BTKR)
library(multcomp)
library(flextable)
library(summarytools)
library(png)
library(shiny)
library(shinydashboard)

source("helper_functions.R")

## app.R ##
header <- dashboardHeader(
  title = tags$img(src="umd_logo.png",contentType = "image/png"),
  
  # Set height of dashboardHeader
  tags$li(class = "dropdown",
          tags$style(".main-header {max-height: 60px}"),
          tags$style(".main-header .logo {height: 60px;}"),
          tags$style(".sidebar-toggle {height: 60px; padding-top: 1px !important;}"),
          tags$style(".navbar {min-height:60px !important}")),
  
  titleWidth = 480
)

sidebar <- dashboardSidebar(
  width = 480,
  sidebarMenu(
    # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    # menuItemOutput("menuitem"),
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search...")
    ),
  sliderInput(inputId = "slider", h3("Years of visits"),
              min=1,max=20, value=3),
  selectInput(inputId = "select", h3("Goods & Services"),
              choices=c("FoodProvided","Food(lbs)","Clothing","Diapers","SchoolKit","HygieneKit"))
)

body <- dashboardBody(
  fluidPage(
    tabsetPanel(
      tabPanel("Plot", 
               plotOutput(outputId = "piechart"),
               plotOutput(outputId = "trendbar"),
               plotOutput(outputId = "q3"),
               plotOutput(outputId = "q4")),
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Table", tableOutput("table"))
    )
    
    
    # title = "How many years did the clients visit?"
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  skin = "black",
  header,
  sidebar,
  body
)

server <- function(input, output,session) {
  output$piechart <- renderPlot({
    pie.func(dat,input$slider)
  })
  
  output$trendbar <- renderPlot({
    if (input$select=="Food(lbs)") {
      trendbar.func(dat, "Food_lbs")
    } else {
      trendbar.func(dat, input$select)
    }
  })
  
  output$q3 <- renderPlot({
    q3.func(dat)
  })
  
  output$q4 <- renderPlot({
    q4.func(dat)
  })
  # output$image1 <- renderImage({
  #     return(list(
  #       src = "umd.png",
  #       contentType = "image/png",
  #       alt = "image1"
  #     ))
  #   } , deleteFile = FALSE)
}

shinyApp(ui, server)

