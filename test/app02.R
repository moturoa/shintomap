
# shintomap Example app 02
# - Advanced example with a 'circle select' tool to filter and immediately display data

# Dependencies
library(shiny)
library(softui)
library(leaflet)
library(leafpm)
library(sf)

# Load this package
devtools::load_all()

# Geo data (in git)
geo <- readRDS("geo_Eindhoven.rds")
buurt_locaties <- sf::st_centroid(geo$buurten)

#----- Shiny app ------
ui <- softui::simple_page(
  softui::fluid_row(
    softui::box(title = "shintomap", icon = bsicon("geo-alt-fill"), width = 6,

                tags$p(bsicon("info-circle-fill", status = "info"),
                       "Click on the circle select tool (topright), draw a circle."),
                tags$p("Then, use the move tool to move the circle around the map to filter the data."),
                uiOutput("ui_circlesize"),

                shintomap::shintoMapUI("map", height = 800)
    ),

    # Debugger panel so we see what's going on
    softui::box(title = "Debug", icon = bsicon("bug"), width = 6,

                verbatimTextOutput("txt_out")
    )
  )

)

server <- function(input, output, session) {

  # debugger panel
  output$txt_out <- renderPrint({
    reactiveValuesToList(input)
  })

  # Base map (see app01 for more comments)
  # - we can add anything to the base map that we can add to a leaflet() object,
  # including a draw toolbar from the leafpm package
  # Since it is difficult to know what sort of reactives are returned by addons,
  # we have the debugger panel to inspect in detail
  my_base_map <- shintomap::shintoBaseMap(set_view = list(
    lng = 5.463155,
    lat = 51.4497,
    zoom = 12
  )) %>%
    leafpm::addPmToolbar(
      drawOptions = pmDrawOptions(snappable = FALSE,
                                  finishOn = "dblclick"),
      targetGroup = "main_layer",
      toolbarOptions = pmToolbarOptions(
        drawMarker = FALSE,
        drawPolygon = FALSE,
        drawCircle = TRUE,
        drawPolyline = FALSE,
        drawRectangle = FALSE,
        cutPolygon = FALSE,
        editMode = FALSE,
        removalMode = TRUE,
        position = "topright"
      ),

      editOptions = pmEditOptions(draggable = FALSE)
    )


  # All the data
  data_all <- reactiveVal(buurt_locaties)

  # Filtered data by the circle
  selected_data <- reactiveVal()

  # Make sure that on init, the selected data is everything
  # Otherwise the map is empty
  observeEvent(data_all(), {
    selected_data(data_all())
  })

  # Call the map module.
  # This map has just one layer with few settings
  callModule(shintoMapModule, "map",
             base_map = my_base_map,
             border = reactive(geo$grens),
             label_function = reactive(function(data,...)data$bu_naam),

             layers = list(

               # Layer: points
               reactive(

                 list(
                   data = selected_data(),
                   toggle = TRUE,
                   group = "area_layer",
                   geom = "CircleMarkers",
                   id_column = "bu_code"
                 )

               )

             )
  )


  # Function to decide whether a point is in our drawn circle
  which_in_circle <- function(data, feature){

    my_circle <- list(
      lon = feature$geometry$coordinates[[1]],
      lat = feature$geometry$coordinates[[2]],
      radius = feature$properties$radius
    )

    circle_point <- sf::st_sfc(sf::st_point(c(my_circle$lon,my_circle$lat))) %>%
      sf::st_set_crs(4326)

    sf::st_is_within_distance(data, circle_point, dist = my_circle$radius, sparse = FALSE)
  }

  # New feature drawn = new circle drawn on map
  observeEvent(input$`map-map_draw_new_feature`, {

    p <- input$`map-map_draw_new_feature`

    # Circle
    if(!is.null(p$properties$radius)){

      i_y <- which_in_circle(data_all(), p)

      # Filter the data and store in our reactiveVal
      selected_data(data_all()[which(i_y),])

    }

  })

  # Keep track of the circle size, to plot above the map
  cur_circle_size <- reactiveVal()

  observeEvent(input$`map-map_draw_new_feature`$properties$radius, {
    cur_circle_size(input$`map-map_draw_new_feature`$properties$radius)
  })
  observeEvent(input$btn_reload_map, {
    cur_circle_size(NULL)
  })

  output$ui_circlesize <- renderUI({

    r <- cur_circle_size()
    req(r)

    if(r < 1000){
      val <- paste0(round(2*r,0),"m")
    } else {
      val <- paste0(round(2*r/1000,1),"km")
    }

    tags$span(
      style = "font-size: 1.2em;",
      paste0("Diameter cirkel: ", val)
    )

  })

  # Circle was edited
  observeEvent(input$`map-map_draw_edited_features`, {

    p <- input$`map-map_draw_edited_features`

    # Circle
    if(!is.null(p$properties$radius)){

      i_y <- which_in_circle(data_all(), p)
      selected_data(data_all()[which(i_y),])

    }

  })

  # Circle was deleted
  observeEvent(input$map_draw_deleted_features, {
    selected_data(NULL)
    cur_circle_size(NULL)
  })


}

shinyApp(ui, server)

