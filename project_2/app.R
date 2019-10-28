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

  br(),  
  
  helpText('Urban Ministries of Durham (UMD) is a non-profit organization providing shelter, food, clothing, hygiene kits, and other services to people in needs
           in Durham. This shiny dashboard aims to provide stakeholders of UMD
           with tools to visualize the different services provided between 1999 and 2019.
           The data set used in this dashboard is supplied by UMD.'),
  
  # sidebarMenu(
  #   menuItem("Introduction", tabName = "background", icon = icon("home"), id = "intro"),
  #   menuItem("Goals", tabName = "goals", icon = icon("star"), id = "goals"),
  #   menuItem("Data Source", tabName = "data", icon = icon("database"), id = "data"),
  #   menuItem("Data Analysis", tabName = "analysis", icon = icon("chart-bar"), id = "analysis"),
  #   menuItem("Question 1", tabName = "Question 1", icon = icon("chart-pie"), id = "p1"),
  #   menuSubItem("p1", tabName = "Customers with Multiple visits"),
  #   menuItem("Question 2", tabName = "Question 2", icon = icon("chart-line"), id = "p2"),
  #   menuSubItem("p2", tabName = "Goods/Service Trends")
  # ) ,
  
  # br(),  
  
  sliderInput(inputId = "slider", h3("Years of visits"), min=1,max=20, value=3),
  selectInput(inputId = "select", h3("Goods & Services"),
              choices=c("FoodProvided","Food(lbs)","Clothing","Diapers","SchoolKit","HygieneKit"))
)

body <- dashboardBody(
  fluidPage(
    tabsetPanel(
      # output$value1 <- renderValueBox({
      #   valueBox(
      #     formatC(sales.account$value, format="d", big.mark=',')
      #     ,paste('Top Account:',sales.account$Account)
      #     ,icon = icon("stats",lib='glyphicon')
      #     ,color = "purple")  
      # })
      
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

}

shinyApp(ui, server)


