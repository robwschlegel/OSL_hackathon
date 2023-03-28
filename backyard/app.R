# This is a Shiny web application. 
# One may run the application by clicking the 'Run App' button above.
#
# To populate the 'data' folder with the neccessary files to run this app,
# first run 'code/01_data_source.R' then code/02_data_prep.R'.


# Setup -------------------------------------------------------------------

# Load libraries
library(shiny)
library(shinyBS)
library(shinyWidgets)
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
                       spp_link = c('<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Posidonia_oceanica_%28L%29.jpg/220px-Posidonia_oceanica_%28L%29.jpg" height="100"></img>',
                                    '<img src="https://images.theconversation.com/files/443875/original/file-20220201-25-lb03xa.jpg?ixlib=rb-1.1.0&q=45&auto=format&w=1200&h=900.0&fit=crop" height="100"></img>',
                                    '<img src="https://upload.wikimedia.org/wikipedia/commons/5/54/Arbacia_lixula_(oursin_noir).JPG" height="100"></img>',
                                    '<img src="https://thumbs.dreamstime.com/b/typical-fish-mediterranean-sea-10107134.jpg" height="100"></img>',
                                    '<img src="https://cdn.unitycms.io/images/AKDs2OsvKwAAzbfxBGZf3W.jpg?op=focus&val=1200,1200,1000,1000,0,0,500,500&sum=8rkP1R-eoD4" height="120"></img>'))

# Dummy time series for demo
spp_ts <- data.frame(obs_date = c(),
                     obs_spp = c())

# Association for demo
ass_info <- data.frame(logo = c('<img src="https://darse.fr/v2/wp-content/uploads/2023/01/cropped-photosite6-e1672867323901-1.png" height="100"></img>',
                                '<img src="http://www.radedevillefranche.fr/wp-content/uploads/2021/02/8.png" height="100"></img>',
                                '<img src="https://nausicaa-plongee.com/images/nausicaa/logo-full.svg" height="100"></img>',
                                '<img src="https://www.initiativesoceanes.org/wp-content/uploads/2020/10/logobleu.svg" height="100"></img>'),
                       link = c('<a target="_blank" rel="noopener noreferrer" href="https://darse.fr/v2/">Association de la sauvegarde du patrimoine maritime de Villefranche-sur-Mer</a>',
                                '<a target="_blank" rel="noopener noreferrer" href="http://www.radedevillefranche.fr">Association des amis de la rade de Villefranche-sur-Mer</a>',
                                '<a target="_blank" rel="noopener noreferrer" href="https://www.nausicaa-plongee.com">Club de plongée nausicaa</a>',
                                '<a target="_blank" rel="noopener noreferrer" href="https://www.initiativesoceanes.org/agir/organiser/">Organise your own beach waste collection</a>'))

# set image urls -- for ease, I'm calling them here
top_left <- "https://images.unsplash.com/photo-1495834041987-92052c2f2865?ixlib=rb-0.3.5&ixid=eyJhcHBfaWQiOjEyMDd9&s=3d771d2cc226047515072dba7a5f03bc&auto=format&fit=crop&w=1050&q=80"
top_right <- "https://images.unsplash.com/photo-1494088391210-792bbadb00f4?ixlib=rb-0.3.5&ixid=eyJhcHBfaWQiOjEyMDd9&s=a421613e91c8475243ad4630171f4374&auto=format&fit=crop&w=1050&q=80"
bottom_left <- "https://images.unsplash.com/photo-1526411061437-7a7d51ec44c8?ixlib=rb-0.3.5&ixid=eyJhcHBfaWQiOjEyMDd9&s=e507916666b43919185fb16cf4e71813&auto=format&fit=crop&w=1050&q=80"
bottom_right <- "https://images.unsplash.com/photo-1525869916826-972885c91c1e?ixlib=rb-0.3.5&ixid=eyJhcHBfaWQiOjEyMDd9&s=f7cce16b11befb3dc6ed56074727b7b6&auto=format&fit=crop&w=1050&q=80"



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
# ui <- fluidPage(
#   
#   # Title
#   # tags$h2("What's happening in my back yard"),
#   titlePanel("What's happening in my back yard""),
#   # Background image
#   setBackgroundImage(
#     src = "https://st2.depositphotos.com/29169508/45409/i/1600/depositphotos_454090468-stock-photo-mediterranean-sea-peaceful-rocky-beach.jpg"
#   ),
#   
#   # My beach button
#   absolutePanel(top = 80, left = 50,
#                   actionButton("myBeach", "My Beach")),
#   
#   # Launch coastline map
#   absolutePanel(top = 80, left = 100,
#                 actionButton("beachMap", "EU Coastline")),
#   
#   # Buttons etc.
#   absolutePanel(top = 100, left = 75,
#                 actionButton("references", "References")),
#   
# )

# UI for css controlled page
# ui
ui <- tagList(

  # head + css
  tags$head(
    tags$link(href="app.css", rel="stylesheet", type="text/css")
  ),
  
  # UI
  shinyUI(
    
    # layout
    # navbarPage(title = 'National Park',
               
               
               # tab 1: landing page
               # tabPanel(title = "Home", 
                        
                        # parent container
                        tags$div(class="landing-wrapper",
                                 
                                 # child element 1: images
                                 tags$div(class = "landing-block background-content",
                                          
                                          # background
                                          img(src = "https://st2.depositphotos.com/29169508/45409/i/1600/depositphotos_454090468-stock-photo-mediterranean-sea-peaceful-rocky-beach.jpg"),
                                          
                                 ),
                                 
                                 # child element 2: content
                                 tags$div(class = "landing-block foreground-content",
                                          tags$div(class = "foreground-text",
                                                   tags$h1("What's happening in my back yard"),
                                                   actionButton("myBeach", "My Beach"),
                                                   actionButton("beachMap", "EU Coastline"),
                                                   actionButton("references", "References")
                                                   # tags$p("This shiny app demonstrates
                                                   #   how to create a 2 x 2 layout
                                                   #            using css grid and
                                                   #            overlaying content."),
                                                   # tags$p("Isn't this cool?"),
                                                   # tags$p("Yes it is!")
                                          )
                                 )
                        )
               ),
               
               #'////////////////////////////////////////
               # tab 2: data
               # tabPanel(title = "Data")
    # )
  # )
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
                               renderDataTable({
                                 datatable(modalDataLocals,
                                           options = list(pageLength = 10))
                               })),
                      tabPanel(title = "Community",
                               br(),
                               renderDataTable({
                                 datatable(ass_info, escape = FALSE,
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
  
  # Modal map code
  observeEvent(input$beachMap, {
    # renderUI({
      showModal(
        modalDialog(size = "l",
                    title = "EU coastal types",
                    easyClose = TRUE,
                  # bootstrapPage(
                    
                    # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                    
                    # Base of map
                    # leafletOutput("baseMap", width = "100%", height = "100%")
                  renderLeaflet({baseMapFlat})
                  # )
      )
    )
  # })
  })
  
}


# Run ---------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

