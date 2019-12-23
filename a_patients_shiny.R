library(leaflet)
library(shiny)
library(dplyr)

patients <- read.csv("C:/Users/JayRoy/Documents/Shiny/GNHA/Test3_csv.csv",stringsAsFactors = FALSE)

palette_fn<- colorFactor(palette = "Dark2", domain=shootings[["neighborhood"]])


my_ui <- fluidPage(
  titlePanel("Patients in Greater Nashville"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "analysis_var",
        label = "Level of Analysis",
        choices = c("neighborhood","gender")
      )
    ),
    mainPanel(
      leafletOutput(outputId = "patients_map"),
      tableOutput(outputId = "grouped_table")
    )
  )
)## fluid Page

my_server <- function(input, output){
output$patients_map <- renderLeaflet({
  palette_fn <- colorFactor(
    palette = "Dark2",
    domain = patients[[input$analysis_var]]
  )
  
  leaflet(data = patients)%>% 
    addProviderTiles("Stamen.TonerLite")%>% addCircleMarkers(
    lat = ~lat,
    lng = ~lon,
    ##label = ~paste0(name,",",age),
    color = ~palette_fn(patients[[input$analysis_var]]),
    fillOpacity = .7,
    radius = 4,
    stroke = FALSE
  )%>%
    addLegend(
      position = "bottomright",
      title = input$analysis_var,
      pal = palette_fn,
      values = patients[[input$analysis_var]],
      opacity = 1
    )
})
output$grouped_table <- renderTable({
  table <- patients %>% group_by(patients[[input$analysis_var]])%>%count()%>% arrange(-n)
  colnames(table) <- c(input$analysis_var,"Number of Patients")
table  
})   
}
shinyApp(ui=my_ui,server = my_server)
