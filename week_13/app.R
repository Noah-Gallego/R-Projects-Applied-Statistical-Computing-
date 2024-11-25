library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(tmap)
library(rsconnect)

# Define account information for deployment
rsconnect::setAccountInfo(name = 'USERNAME',
                          token = 'TOKEN',
                          secret = 'SECRET')

# Define UI for application that maps geographic data
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$style(HTML(
      "body {font-family: 'Helvetica', sans-serif;}
       .title-panel {color: #2c3e50; text-align: center; padding-bottom: 10px;}
       .sidebar {background-color: #f8f9fa; padding: 15px; border-radius: 5px;}
       .main-panel {padding: 15px;}"
    ))
  ),
  titlePanel(h3("Geographic Data Mapping", class = "title-panel")),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      fileInput("file", "Upload CSV File", accept = ".csv"),
      selectInput("geoLevel", "Select Geographic Level:", 
                  choices = c("County", "State", "Country"),
                  selected = "County"),
      actionButton("mapButton", "Generate Map", class = "btn-primary")
    ),
    mainPanel(
      class = "main-panel",
      leafletOutput("map", height = 600)
    )
  )
)

# Define server logic required to map geographic data
server <- function(input, output, session) {
  # Reactive expression to read uploaded data
  datasetInput <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df
  })
  
  # Observe event when map button is clicked
  observeEvent(input$mapButton, {
    df <- datasetInput()
    
    # Detect geographic columns based on user selection
    if (input$geoLevel == "County") {
      geo_col <- "county"
    } else if (input$geoLevel == "State") {
      geo_col <- "state"
    } else {
      geo_col <- "country"
    }
    
    # Ensure the selected geographic column exists
    if (!(geo_col %in% colnames(df))) {
      showNotification("Selected geographic level not found in dataset.", type = "error")
      return()
    }
    
    # Assuming df has latitude and longitude columns for mapping
    if (!all(c("latitude", "longitude") %in% colnames(df))) {
      showNotification("Latitude and/or longitude columns not found in dataset.", type = "error")
      return()
    }
    
    # Create a customized map based on the geographic level
    output$map <- renderLeaflet({
      leaflet(df) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          ~longitude, ~latitude,
          label = ~as.character(df[[geo_col]]),
          radius = 5,
          color = "#007bff",
          fillColor = "#007bff",
          fillOpacity = 0.7,
          popup = ~paste("<strong>", geo_col, ":</strong> ", df[[geo_col]], "<br><strong>Latitude:</strong> ", latitude, "<br><strong>Longitude:</strong> ", longitude)
        )
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
