library(shiny)
library(leaflet)
library(shinydashboard)
library(tidyverse)

patients <- read.csv("C:/Users/JayRoy/Documents/Shiny/GNHA/Test3_csv.csv",stringsAsFactors = FALSE)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Patients Map"),
  dashboardSidebar(
    selectInput("area",label = "GreaterNashville",choices = patients$neighborhood)
  ),
  dashboardBody(
    fluidRow(box(width = 12, leafletOutput(outputId = "mymap")))
  
  ))

server <- function(input, output){
  output$mymap <- renderLeaflet({
 patients%>%
    filter(neighborhood %in% input$area)%>%
    leaflet() %>%
    addTiles()%>%
    addCircleMarkers(lng = ~lon, lat = ~lat)
     })
}

shinyApp(ui=ui, server = server)