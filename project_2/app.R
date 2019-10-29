source("helper_functions.R") #Read in the functions in helper_functions.R

## app.R ##
# Create header with UMD logo image
header <- dashboardHeader(
  title = tags$img(src="umd_logo_resize.png", contentType = "image/png"),
  
  # Set height of dashboardHeader
  tags$li(class = "dropdown",
          tags$style(".main-header {max-height: 60px}"),
          tags$style(".main-header .logo {height: 60px;}"),
          tags$style(".sidebar-toggle {height: 60px; padding-top: 1px !important;}"),
          tags$style(".navbar {min-height:60px !important}")),
  
  titleWidth = 330
)

# Create Sidebar with helpTexts and Inputs
sidebar <- dashboardSidebar(
  width = 330,
  br(), # empty row added
  
  helpText("This project aims to provide UMD a dynamic results of analysis based on the data provided by UMD. 
    UMD will be able to find meaningful findings that will aid to improve their service. Below are a list of variables that can be selected based on your interest."),

  # Input 1
  sliderInput(inputId="yearrange", label=h5("Year Range of Interest"), 
              min=1990, max=2018, value=c(1990,2018)),
  helpText("Select the years to see the increase rate of number of case filed. The upper range will be used as a compaison year when calculating predicted increase rate in the future as well. Note that, year 2019 cannot be selected since data was collected in mid 2019, hence does not reflect the yearly demand.") ,
  
  # Input 2
  numericInput(inputId="newyear", label=h5("Predict Demand of Year (yyyy)"), 
              min=2019, max=NA, value=2019),
  helpText("Choose a year to predict the service demand and the increase rate. Minimum value is 2019.") ,
  br(),
  
  # Input 3
  selectInput(inputId = "select", h5("Services"),
              choices=c("Food Provided","Food(lbs)","Clothing","Diapers","School Kit","Hygiene Kit")),
  helpText("Has each service/goods increased in demand over the years? Select the service/goods to see the trend."),
  
  # Add multiple empty rows
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  
  # Input 4
  sliderInput(inputId = "slider", h5("Number of Visited Years"), min=1,max=20, value=3),
  helpText("How many clients visit UMD more than once? Select the number of years to see the proportion 
           of clients visited UMD more than that number.")
)


# Create the Body part 
body <- dashboardBody(
  fluidPage(
    br(), #empty row
    
    # Row 1
    fluidRow(
      # Left side column
      column(8, box(width=24, title= "Number of service and financial support provided over years",
                 plotOutput(outputId = "q4"))) ,
      # Right side column with 3 text boxes
      column(4,
             br(),
             br(),
             fluidRow(column(12, box(width=36, title="Increase rate in Demand for UMD",
                                     textOutput("q5")))),
             fluidRow(column(12, box(width=36, textOutput("q6")))),
             fluidRow(column(12, box(width=36, title= "Predicted Demand and Increase Rate", textOutput("q7")))))
      ),
    
    # Row 2
    fluidRow(
      # Left column
      column(6, 
             box(width=18, title=textOutput("boxtitle2"),
                 plotOutput(outputId = "trendbar"))),
      # Right column
      column(6, 
             box(width=18, title= "Overall Trends of Services by Season",
                 plotlyOutput(outputId = "q3")))
    ), 
    
    # Row 3
    fluidRow(
      # Left column with 3 text boxes
      column(4, 
             br(),
             fluidRow(column(12, box(width=36, title = "Demand Increase in Recent Years",textOutput("q8")))),
             fluidRow(column(12, box(width=36, title = "Seasonal Trend", textOutput("q9")))),
             fluidRow(column(12, box(width=36, title = "Number of Continuing Customers",
                                     textOutput("q10"))))),
      
      # Right column with pie chart
      column(8, 
             box(width=24, title= textOutput("boxtitle1"),
                 plotOutput(outputId = "piechart")))
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  skin = "black",
  header,
  sidebar,
  body
)

# Create Server with rendered objects
server <- function(input, output,session) {

  # Render titles using inputs
  output$boxtitle1 <- renderText({
    paste0("Proportion of Clients with more than ", numtostr(input$slider), " visits")
  })

  output$boxtitle2 <- renderText({
    paste0("Trend of ", input$select, " provided over years")
  })

  # Render plots using functions from helper_functions.R
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
  
  output$q3 <- renderPlotly({
    print(ggplotly(q3.func(dat)))
  })
  
  output$q4 <- renderPlot({
    q4.func(dat, input$yearrange)
  })
  
  output$q5 <- renderText({
    q5.func(dat,date= input$yearrange)
  })

  output$q6 <- renderText({
    paste0("The overall demand for UMD has an increasing trend over the years. Although the increasing rate has been decreasing in the past few years, statistically, there is still expected increase in demand in the future.")
  })
  
  output$q7 <- renderText({
   paste0("To be specific, based on linear model estimation, the expected number of service demand in year ", input$newyear, " is ", q6.func(dat,input$newyear,input$yearrange[2])[[1]], ". ",
    q6.func(dat,input$newyear,input$yearrange[2])[[2]])
  })
  
  output$q8 <- renderText({
    paste0("UMD must be aware of the services with increasing demand in the past years. In particular, results show that Diapers and School Kits.")
  })
  
  output$q9 <- renderText({
    paste0("Seasonally, there seems to be an increased demand in clothings and diapers. UMD must be aware of this demand increase and prepare for the seasons.")
  })
  
  output$q10 <- renderText({
    paste0("From the pie chart, we can see that the percentage of customers with multiple visits (not necessarily continuous) are quite small. It is crucial for UMD to identify the cause to improve the service.")
  })
}

# Run shiny app
shinyApp(ui, server)
# rsconnect::deployApp("/Users/jieun/github_updated/bios611-projects-fall-2019-jennprk/project_2")

