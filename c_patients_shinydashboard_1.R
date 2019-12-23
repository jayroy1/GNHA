library(shiny)
library(leaflet)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)

patients <- read.csv("C:/Users/JayRoy/Documents/Shiny/Test3_csv.csv",stringsAsFactors = FALSE)
patients$patientId <- as.character(patients$patientId) 

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Patients Map"),
  dashboardSidebar(
    pickerInput("area",label = "Greater Nashville Patients",choices = c("Belle Meade", "Brentwood", "Hendersonville","Mt.Juliet","Murfreesboro","Nashville","Smithville"),multiple = T )
  ),
  dashboardBody(
    fluidRow(box(width = 12, leafletOutput(outputId = "mymap")))
  
  ))

server <- function(input, output){
  output$mymap <- renderLeaflet({
    
 patients%>%
    filter(neighborhood %in% input$area)%>%
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")%>%
      setView(lng =-86.6611 ,lat =36.1299 ,zoom = 11)%>%
    addCircleMarkers(lng = ~lon, lat = ~lat,stroke=FALSE,popup = ~paste0("Patient ", patientId))
     })
}

shinyApp(ui=ui, server = server)