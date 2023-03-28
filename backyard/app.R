# This is a Shiny web application. 
# One may run the application by clicking the 'Run App' button above.
#
# To populate the 'data' folder with the neccessary files to run this app,
# first run 'code/01_data_source.R' then code/02_data_prep.R'.


# Setup -------------------------------------------------------------------

# Load libraries
library(shiny)
library(leaflet)
library(sf)

# Load coastal types data
if(!exists("coastal_type")) load("data/coastal_type.RData")
if(!exists("coastal_type_point")) load("data/coastal_type_point.RData")


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- bootstrapPage(

  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  # Base of map
  leafletOutput("baseMap", width = "100%", height = "100%")
  
)


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Base of map
  output$baseMap <- renderLeaflet({
    leaflet(coastal_type_point) |> addTiles() |>
      setView(lng = 15, lat = 45, zoom = 5) |> 
      # leaflet::addTiles()
      leaflet::addMarkers()
      # leaflet::addPolygons()
      # leaflet::addCircles()
  })
}


# Run ---------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

