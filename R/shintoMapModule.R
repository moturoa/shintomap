

#----- UI -------

#' Shinto Map UI Module
#' @param id Shiny ID for the module
#' @param debugger_panel If TRUE, add a verbatimtextoutput panel showing all shiny input values, handy for debugging
#' @param \dots Further arguments passed to [leafletOutput()], such as `height`, `width`
#' @seealso [shintoMapModule()]
#' @export
#' @importFrom shiny NS verbatimTextOutput reactive
#' @importFrom leaflet leafletOutput
shintoMapUI <- function(id, debugger_panel = FALSE, ...){

  ns <- shiny::NS(id)

  id_map <- ns("map")

  shiny::tagList(

    leaflet::leafletOutput(id_map, ...),

    if(debugger_panel){
      shiny::verbatimTextOutput(ns("txt_out"))
    }

  )


}


#' Shinto Map server module
#' @param input shiny input, do not set
#' @param output shiny output, do not set
#' @param session shiny session, do not set
#' @param base_map Static map (with tiles and view setting) made with [shintoBaseMap()]
#' @param render_not_visible If TRUE, renders the map even when not visible. Works in tabsetpanels (tab_box),
#' but does not work in modals! For modals, set render_not_visible = FALSE (map should be rendered only after
#' the modal is opened)
#' @param border Reactive sf dataframe with a polygon used as a border
#' @param border_weight Thickness of border polygon
#' @param border_color Color of border outline
#' @param auto_recenter If TRUE, zooms to area when the border changes (reactively)
#' @param label_function Function (with `data` and `...` as arguments) to make labels. Optional.
#' @param label_params List of further arguments to pass to `label_function`
#' @param color_default Default (fill) color if color is not computed for the layer
#' @param color_outline Fixed color of the outline of polygons
#' @param toggle_reload Reactive to trigger a reload of the map. Sometimes necessary, such as in modals.
#' @param layers List of reactives (each with a list of settings). See example apps.
#' @importFrom shiny renderPrint reactiveValuesToList isolate outputOptions
#' @importFrom leaflet leaflet renderLeaflet leafletProxy clearGroup addPolygons fitBounds addCircleMarkers
#' @importFrom leaflet addLegend removeControl setView addPolylines
#' @importFrom sf st_bbox
#' @importFrom shiny observe reactiveVal observeEvent
#' @importFrom leafgl addGlPoints
#' @export
shintoMapModule <- function(input, output, session,
                      base_map,

                      render_not_visible = TRUE,

                      border = shiny::reactive(NULL),
                      border_weight = 2,
                      border_color = "#4f4f4f",
                      auto_recenter = FALSE,

                      label_function = function(data,...)NULL,   # functie om van de id een label (hover) te maken
                      label_params = list(),   # settings passed to label_function (can be anything)

                      color_default = "#023e8a",
                      color_outline = "#FFFFFF",

                      toggle_reload = shiny::reactive(TRUE), # complete map reloaden
                      proxy = TRUE,
                      layers = list()){



  output$txt_out <- shiny::renderPrint({
    shiny::reactiveValuesToList(input)
  })


  ui_ping <- reactiveVal()

  output$map <- leaflet::renderLeaflet({

    toggle_reload()

    # Use shintoBaseMap to make the static leaflet map (with map tiles and a view)
    map <- base_map

    # Border
    border_data <- shiny::isolate(border())

    if(!is.null(border_data)){
      map <- map %>%
        leaflet::addPolygons(data = border_data,
                    group = "grens",
                    fill = FALSE,
                    stroke = TRUE,
                    weight = border_weight,
                    color = border_color)
    }

    ui_ping(runif(1))


    if(!proxy){

      for(i in seq_along(layers)){

        lay <- layers[[i]]()
        map <- add_map_layer(map, lay, color_default, color_outline, label_function)
      }

      if(auto_recenter){
        map <- map %>% leaflet::fitBounds(bb[1], bb[2], bb[3], bb[4])
      }
    }

    map

  })

  # Bug fix: map does not load on an unselected tab
  if(render_not_visible){
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)
  }


  # Reactive border data
  if(proxy){

    shiny::observe({

      border_data <- border()
      ui_ping()
      shiny::req(border_data)
      shiny::req(nrow(border_data) > 0)

      bb <- unname(sf::st_bbox(border_data))

      map <- leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("grens") %>%
        leaflet::addPolygons(data = border_data,
                             group = "grens",
                             fill = FALSE,
                             stroke = TRUE,
                             weight = border_weight,
                             color = border_color)

      if(auto_recenter){
        map <- map %>% leaflet::fitBounds(bb[1], bb[2], bb[3], bb[4])
      }

      map

    })

  }




  group_names <- reactive({
    sapply(layers, function(layer)layer()$group)
  })

  observeEvent(group_names(), {
    if(any(duplicated(group_names()))){
      stop("Duplicate group names in layers in shintomap, fix please!")
    }
  })


  if(proxy){

    map <- leaflet::leafletProxy("map")

    lapply(layers, function(layer){

      shiny::observe({

        ui_ping()
        lay <- layer()

        map <- add_map_layer(map, lay, color_default, color_outline, label_function)

      })

    })

  }


  clicked_id <- shiny::reactiveVal()

  shiny::observeEvent(input$map_marker_click, {
    clicked_id(input$map_marker_click)
  })

  shiny::observeEvent(input$map_shape_click, {
    clicked_id(input$map_shape_click)
  })

  return(clicked_id)
}
