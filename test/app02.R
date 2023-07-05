


library(shiny)
library(softui)
library(leaflet)
library(leafpm)
library(sf)

devtools::load_all()


geo <- readRDS("geo_Eindhoven.rds")

buurt_locaties <- sf::st_centroid(geo$buurten)

ui <- softui::simple_page(
  softui::fluid_row(
    softui::box(title = "shintomap", icon = bsicon("geo-alt-fill"), width = 6,

                uiOutput("ui_circlesize"),
                shintoMapUI("map", height = 800)
    ),
    softui::box(title = "Debug", icon = bsicon("bug"), width = 6,

                verbatimTextOutput("txt_out")
    )
  )

)

server <- function(input, output, session) {

  output$txt_out <- renderPrint({
    reactiveValuesToList(input)
  })


  my_base_map <- shintoBaseMap(set_view = list(
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


  data_all <- reactiveVal(buurt_locaties)

  callModule(shintoMapModule, "map",
             base_map = my_base_map,
             border = reactive(geo$grens),
             label_function = reactive(function(data,...)data$bu_naam),

             layers = list(

               # Layer: gebouwen
               reactive(

                 list(
                   data = data_all(),
                   toggle = TRUE,
                   group = "area_layer",
                   geom = "CircleMarkers",
                   id_column = "bu_code"

                 )

               )

             )
  )


  selected_data <- reactiveVal()

  which_in_circle <- function(data, feature){

    my_circle <- list(
      lon = feature$geometry$coordinates[[1]],
      lat = feature$geometry$coordinates[[2]],
      radius = feature$properties$radius
    )
    circle_point <- st_sfc(st_point(c(my_circle$lon,my_circle$lat))) %>%
      st_set_crs(4326)

    st_is_within_distance(data, circle_point, dist = my_circle$radius, sparse = FALSE)
  }

  observeEvent(input$`map-map_draw_new_feature`, {

    p <- input$`map-map_draw_new_feature`

    # Circle
    if(!is.null(p$properties$radius)){

      i_y <- which_in_circle(data_all(), p)
      selected_data(data_all()[which(i_y),])

    }

  })


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


  observeEvent(input$`map-map_draw_edited_features`, {

    p <- input$`map-map_draw_edited_features`

    # Circle
    if(!is.null(p$properties$radius)){

      i_y <- which_in_circle(data_all(), p)
      selected_data(data_all()[which(i_y),])

    }

  })


  observeEvent(input$map_draw_deleted_features, {
    selected_data(NULL)
    cur_circle_size(NULL)
  })


}

shinyApp(ui, server)

