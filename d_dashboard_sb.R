
library(shinydashboard)
library(shiny)
# Define UI for application ...
ui <- dashboardPage(
    dashboardHeader(title="Sampling"),##dashboardHeader
    dashboardSidebar(
      sidebarMenu(
          menuItem("Uniform Distribution",tabName = "uniform",icon=icon("hospital")),
          menuItem("Normal Distribution", tabName = "normal", icon = icon("heart"))
      )
        ),##sidebar
        dashboardBody(
           tabItems(
               tabItem(
              tabName = "uniform",
              fluidRow(
                box(
                  title = "Select a Number",
                  solidHeader = TRUE,
                  background="yellow",
                  status="warning",
                  height = 312,
                  sliderInput(inputId = "number",
                              label = "",
                              value = 500,min = 25,max = 1000)),
                box(
                  title = "Histogram",
                  solidHeader = TRUE,
                  background="light-blue",
                  status="primary",
                  plotOutput("hist",height = 250)),
              
              valueBoxOutput("meanBox"),
              valueBoxOutput("medianBox"),
              valueBoxOutput("sdBox")
              )
               ),
               tabItem(tabName = "normal",
                       fluidRow(
                         box(
                           title = "Select a Number",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status="warning",
                           height = 312, 
                           sliderInput(inputId = "normnumber",
                                       label = "",
                                       value = 500,min = 25,max = 1000)),
                         box(
                           title = "Density Plot",
                           solidHeader = TRUE,
                           background="light-blue",
                           status="primary",
                           plotOutput("density",height = 250)),
               infoBoxOutput("meanInfoBox"),
               infoBoxOutput("medianInfoBox"),
               infoBoxOutput("sdInfoBox")
                         )
           ) #tabItems
        ) ##dashboardBody
    )
    )## dashboardPage

# Define server logic required to draw a histogram
server <- function(input, output) {

    histdata <- reactive({runif(input$number,min = 0,max = 1)})
    densitydata <- reactive({rnorm(input$normnumber)})
    
    output$hist <- renderPlot({
        hist(histdata(),xlab = "Value", main = paste(input$number,"random values between 0 and 1"))
        })
    
    output$density <- renderPlot({
        hist(densitydata(),xlab = "Value", 
             main = paste("standard norm \n",
                 input$normnumber,"random values"),
             probability = TRUE)
        lines(density(densitydata()))
        })
    output$meanInfoBox <- renderInfoBox({
      infoBox("Mean",
              round(mean(densitydata()),3),
              icon=icon("align-center"),
              color = "navy")
    })
    output$medianInfoBox <- renderInfoBox({
      infoBox("Median",
              round(median(densitydata()),3),
              icon=icon("area-chart"),
              color = "aqua")
    })
    output$sdInfoBox <- renderInfoBox({
      infoBox("Standard Deviation",
              round(sd(densitydata()),3),
              icon=icon("scribd"),
              fill = F,
              color = "blue")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
