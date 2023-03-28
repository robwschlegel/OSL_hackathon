# This is a Shiny web application. 
# One may run the application by clicking the 'Run App' button above.
#
# To populate the 'data' folder with the neccessary files to run this app,
# first run 'code/01_data_source.R' then code/02_data_prep.R'.


# Setup -------------------------------------------------------------------

# Load libraries
library(shiny)
library(shinyBS)
library(leaflet)
library(DT)
# library(RColorBrewer)

# Load coastal types data
if(!exists("coastal_type")) load("data/coastal_type.RData")
if(!exists("coastal_type_df")) load("data/coastal_type_df.RData")
# coastal_type_df <- coastal_type_df[1:100,]

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
                       spp_link = c('<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Posidonia_oceanica_%28L%29.jpg/220px-Posidonia_oceanica_%28L%29.jpg" height="70"></img>',
                                    '<img src="https://images.theconversation.com/files/443875/original/file-20220201-25-lb03xa.jpg?ixlib=rb-1.1.0&q=45&auto=format&w=1200&h=900.0&fit=crop" height="70"></img>',
                                    '<img src="https://upload.wikimedia.org/wikipedia/commons/5/54/Arbacia_lixula_(oursin_noir).JPG" height="70"></img>',
                                    '<img src="https://thumbs.dreamstime.com/b/typical-fish-mediterranean-sea-10107134.jpg" height="70"></img>',
                                    '<img src="https://cdn.unitycms.io/images/AKDs2OsvKwAAzbfxBGZf3W.jpg?op=focus&val=1200,1200,1000,1000,0,0,500,500&sum=8rkP1R-eoD4" height="70"></img>'))

# Dummy time series for demo
spp_ts <- data.frame(obs_date = c(),
                     obs_spp = c())

# Association for demo
ass_info <- data.frame(logo = c("https://darse.fr/v2/wp-content/uploads/2023/01/cropped-photosite6-e1672867323901-1.png",
                                "http://www.radedevillefranche.fr/wp-content/uploads/2021/02/8.png",
                                "https://nausicaa-plongee.com/images/nausicaa/logo-full.svg",
                                "https://www.initiativesoceanes.org/wp-content/uploads/2020/10/logobleu.svg"),
                       name = c("Association de la sauvegarde du patrimoine maritime de Villefranche-sur-Mer",
                                "Association des amis de la rade de Villefranche-sur-Mer",
                                "Club de plongée nausicaa",
                                "Organise your own beach waste collection"),
                       link = c("https://darse.fr/v2/",
                                "http://www.radedevillefranche.fr",
                                "https://www.nausicaa-plongee.com",
                                "https://www.initiativesoceanes.org/agir/organiser/"))


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- bootstrapPage(

  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  # Base of map
  leafletOutput("baseMap", width = "100%", height = "100%"),
  
  # Buttons etc.
  absolutePanel(top = 10, right = 10,
                actionButton("myBeach", "My Beach")
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
  

  # Modal panel -------------------------------------------------------------

  # Prep photo links
  main_photo <- "picture_darse"
  
  # Modal code
  observeEvent(input$myBeach, {
    showModal(
      modalDialog(size = "l",
                  title = "Plage de la Darse",
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
                               renderDataTable({
                                 datatable(modalDataLocals,
                                           options = list(pageLength = 10))
                               })),
                      tabPanel(title = "Community",
                               br(),
                               renderDataTable({
                                 datatable(modalDataLocals,
                                           options = list(pageLength = 10))
                               })),
                      # tabPanel(title = "Time series",
                      #          br(),
                      #          renderPlotly({
                      #            ggplotly(
                      #              modalData %>% 
                      #                ggplot(aes(x = date, y = T2m)) +
                      #                geom_line() +
                      #                geom_point() +
                      #                geom_smooth(method = "lm", se = F) +
                      #                scale_x_date(expand = c(0, 0)) +
                      #                labs(x = NULL, y = "T2m (°C)") +
                      #                theme(panel.border = element_rect(fill = NA, colour = "black"))
                      #            )
                      #          })
                      )
                    )
                  )
      )
  })
  
}


# Run ---------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

