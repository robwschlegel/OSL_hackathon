# This is a Shiny web application. 
# One may run the application by clicking the 'Run App' button above.
# To populate the 'data' folder first run 'code/01_data_source.R' 


# Setup -------------------------------------------------------------------

# Load libraries
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)

# Load coastal types data
load("data/coastal_type_df.RData")

# Colour palette
coast_col <- colorFactor(palette = 'RdYlGn', coastal_type_df$morpho)

# Image link
src_darse <- "http://www.beachrex.com/img/beaches/FR-20a-k-provansa/FR-20a-k-provansa-sl14.jpg"

# Manual species info for demo
spp_info <- data.frame(spp_name = c("Sea grass",
                                    "Octopus",
                                    "Urchin",
                                    "Grouper",
                                    "Sea turtle"),
                       spp_link = c('<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Posidonia_oceanica_%28L%29.jpg/220px-Posidonia_oceanica_%28L%29.jpg" height="100"></img>',
                                    '<img src="https://images.theconversation.com/files/443875/original/file-20220201-25-lb03xa.jpg?ixlib=rb-1.1.0&q=45&auto=format&w=1200&h=900.0&fit=crop" height="100"></img>',
                                    '<img src="https://upload.wikimedia.org/wikipedia/commons/5/54/Arbacia_lixula_(oursin_noir).JPG" height="100"></img>',
                                    '<img src="https://thumbs.dreamstime.com/b/typical-fish-mediterranean-sea-10107134.jpg" height="100"></img>',
                                    '<img src="https://cdn.unitycms.io/images/AKDs2OsvKwAAzbfxBGZf3W.jpg?op=focus&val=1200,1200,1000,1000,0,0,500,500&sum=8rkP1R-eoD4" height="120"></img>'))

# Dummy time series for demo
spp_ts <- data.frame(obs_date = c(2014:2023, 
                                  2015, 2017, 2019, 2020,
                                  2014, 2017, 2019, 2020, 2021, 2022, 2023,
                                  2014, 2019, 2021),
                     obs_spp = c(rep("Sea grass", 10),
                                 rep("Octopus", 4),
                                 rep("Urchin", 7),
                                 rep("Grouper", 3)))

# Association for demo
ass_info <- data.frame(logo = c('<img src="https://darse.fr/v2/wp-content/uploads/2023/01/cropped-photosite6-e1672867323901-1.png" height="100"></img>',
                                '<img src="http://www.radedevillefranche.fr/wp-content/uploads/2021/02/8.png" height="100"></img>',
                                '<img src="https://nausicaa-plongee.com/images/nausicaa/logo-full.svg" height="100"></img>',
                                '<img src="https://www.initiativesoceanes.org/wp-content/uploads/2020/10/logobleu.svg" height="100"></img>'),
                       link = c('<a target="_blank" rel="noopener noreferrer" href="https://darse.fr/v2/">Association de la sauvegarde du patrimoine maritime de Villefranche-sur-Mer</a>',
                                '<a target="_blank" rel="noopener noreferrer" href="http://www.radedevillefranche.fr">Association des amis de la rade de Villefranche-sur-Mer</a>',
                                '<a target="_blank" rel="noopener noreferrer" href="https://www.nausicaa-plongee.com">Club de plongée nausicaa</a>',
                                '<a target="_blank" rel="noopener noreferrer" href="https://www.initiativesoceanes.org/agir/organiser/">Organise your own beach waste collection</a>'))


# UI ----------------------------------------------------------------------

# UI for map as the landing page
# ui <- bootstrapPage(

  # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  # Base of map
  # leafletOutput("baseMap", width = "100%", height = "100%"),
  
  # Buttons etc.
#   absolutePanel(top = 10, right = 10,
#                 actionButton("myBeach", "My Beach")
#   )
#   
# )

# UI for picture as the landing page
ui <- fluidPage(

  # Title
  # tags$h2("What's happening in my back yard"),
  # titlePanel("What's happening in my back yard"),
  # Background image
  setBackgroundImage(
    src = "https://st2.depositphotos.com/29169508/45409/i/1600/depositphotos_454090468-stock-photo-mediterranean-sea-peaceful-rocky-beach.jpg"
  ),

  # Buttons
  absolutePanel(top = 400, left = 600, class = "panel panel-default",
                tags$h1("What's happening in my back yard"),
                fluidRow(
                  column(width = 1),
                  column(width = 4,
                         actionButton("beachMap", "EU Coastline")),
                  column(width = 2),
                  column(width = 4,
                         actionButton("myBeach", "My Beach")),
                  column(width = 1)),
                fluidRow(),
                fluidRow(
                  column(width = 4),
                  column(width = 4,
                         actionButton("references", "References")),
                  column(width = 4)
                  )
                )
)


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  
  # Images ------------------------------------------------------------------
  
  output$picture_darse <- renderText({c('<img src="',src_darse,'" width=800px>')})

  
  # Base of map -------------------------------------------------------------

  output$baseMap <- renderLeaflet({
    leaflet(coastal_type_df) |> addTiles() |>
      setView(lng = 15, lat = 45, zoom = 5) |> 
      leaflet::addCircles(radius = 10000, lng = ~lon, lat = ~lat,
                          color = ~coast_col(morpho), 
                          popup = ~as.character(coasttype))
  })
  
  # Unrendered, for testing
  baseMapFlat <- leaflet(coastal_type_df) |> addTiles() |>
      setView(lng = 15, lat = 45, zoom = 5) |> 
      leaflet::addCircles(radius = 10000, lng = ~lon, lat = ~lat,
                          color = ~coast_col(morpho), 
                          popup = ~as.character(coasttype))

  
  # Modal panels ------------------------------------------------------------

  # Prep photo links
  main_photo <- "picture_darse"
  
  # Modal UI code
  observeEvent(input$myBeach, {
    showModal(
      modalDialog(size = "l",
                  title = "Plage de la Darse",
                  easyClose = TRUE,
                  fluidPage(
                    tabsetPanel(
                      tabPanel(title = "Photos",
                               br(),
                               htmlOutput(main_photo)),
                      tabPanel(title = "Locals",
                               br(),
                               renderDataTable({
                                 datatable(spp_info, escape = FALSE,
                                           options = list(pageLength = 10))
                               })),
                      tabPanel(title = "Sightings",
                               br(),
                               renderPlotly({
                                            ggplotly(
                                              spp_ts |> 
                                                ggplot(aes(x = obs_date)) +
                                                geom_bar(aes(fill = obs_spp)) +
                                                labs(x = NULL, y = "Locals") +
                                                theme(legend.position = "none",
                                                      panel.border = element_rect(fill = NA, colour = "black"))
                                            )
                                          }),
                               shiny::selectInput(label = "Local sightings", inputId = "sightings", 
                                                  choices = unique(spp_info$spp_name), multiple = TRUE),
                               shiny::actionButton("sight_upload", "Submit")
                               
                      ),
                      tabPanel(title = "Community",
                               br(),
                               renderDataTable({
                                 datatable(ass_info, escape = FALSE,
                                           options = list(pageLength = 10))
                               }))
                    )
                  )
      )
      )
  })
  
  # Modal map code
  observeEvent(input$beachMap, {
      showModal(
        modalDialog(size = "l",
                    title = "EU coastal types",
                    easyClose = TRUE,
                  renderLeaflet({baseMapFlat})
      )
    )
  })
  
  # References map code
  observeEvent(input$references, {
    showModal(
      modalDialog(size = "l",
                  title = "EU coastal types",
                  easyClose = TRUE,
                  tags$p("Krystalli A, Fernández-Bejarano S, Salmon M (????). _EMODnetWFS: Access EMODnet Web Feature Service data through R_. R
  package version 2.0.1.9001. Integrated data products created under the European Marine Observation Data Network (EMODnet)
  Biology project (EASME/EMFF/2017/1.3.1.2/02/SI2.789013), funded by the by the European Union under Regulation (EU) No
  508/2014 of the European Parliament and of the Council of 15 May 2014 on the European Maritime and Fisheries Fund,
  <https://github.com/EMODnet/EMODnetWFS>.")))
  })
  
}


# Run ---------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

