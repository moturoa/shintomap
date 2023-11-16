

#----- UI -------

#' Shinto Map UI Module
#' @param id Shiny ID for the module
#' @param debugger_panel If TRUE, add a verbatimtextoutput panel showing all shiny input values, handy for debugging
#' @param \dots Further arguments passed to [leafletOutput()], such as `height`, `width`
#' @rdname shinymapmodule
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


#' Shinto Map Shiny Module
#' @description Shiny module (ui+server), a wrapper around [leaflet()], making use of leafletProxy for performance,
#' and many settings specific for Shinto applications. Includes many fixes for awkward situations with
#' maps in tabs, modals, hidden/not hidden, etc. Returns a reactive(), with information on what polygon
#' or point was clicked on. Handy for making your own popups (the leaflet popup is not supported since
#' it is very limited in use).
#' @details The layers argument is quite special, and takes a few arguments. Best to start with the example apps
#' to understand what is going on. Provide a list of reactives (list(reactive(),reactive())), with each reactive
#' itself a list with the following components.
#'
#' - `data` : sf-dataframe with spatial data
#' - `geo_column` : (optional), the name of the geo-column in the sf-dataframe. If not specified,
#' uses the active geo column (don't know what that means? Read up on the `sf` package!)
#' - `toggle` :  TRUE or FALSE to show the layer (or not)
#' - `group` : name of the group for the layer, just like in [leaflet()]
#' - `geom` :  how are we plotting the data (one of CircleMarkers, Polygons, and some others but untested/unused)
#' - `id_column` : which column of `data` contains unique row identifiers? Important for clicking/hovering
#' - `color_column` : (optional) which column of `data` will be used to color the map?
#' - `color_function` : (optional) a list with a few components, see examples.
#' - `color_outline` :  color for the outline around the points or polygons
#' - `legend` :  a list controlling the placement of the legend, see examples.
#' - `weight` : thickness of line around point or polygn
#' - `opacity` : (optional) the opacity (0-1) of the fill color (not the outline color)
#' - `stroke` : TRUE/FALSE, whether to plot the outline around polygon or point
#' - `highlightOptions` : options for highlighting, see [leaflet::addPolygons()]
#' - `radius` (only for circlemarkers) : radius of the points
#' - `clustering` (only for circlemarkers) : whether to use the clustering option
#'
#' **Colors**
#'
#' - if `color_column` and `color_function` are omitted, `color_default` is used to color everything
#' - if only `color_column` is provided, the column is assumed to contain colors ("#9bc11f")
#' - otherwise, `color_column` will contain data used to generate colors, using the specs in `color_function`
#'
#' `color_function` has the following components:
#'
#' - `palfunction` : the actual function used to generate colors from data (standard: `shinto_auto_color`),
#' can be some function you defined. !! Has to be first component of the list.
#' - All other arguments will be sent to the `palfunction` defined.
#' - If using `shinto_auto_color`, that function will use either [binned_numeric_map_color()] (when data are numeric),
#' of [factor_map_color()] (when data are factor or character). See help pages there for info.
#'
#' @rdname shinymapmodule
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
#' @param proxy If TRUE (default), uses leafletProxy() to update the map. This is better for performance, but in
#' some cases (for maps that are hidden), try using proxy = FALSE to avoid certain display bugs.
#' @param layers List of reactives (each with a list of settings). See example apps in test/, and Details below
#' @importFrom shiny renderPrint reactiveValuesToList isolate outputOptions
#' @importFrom leaflet leaflet renderLeaflet leafletProxy clearGroup addPolygons fitBounds addCircleMarkers
#' @importFrom leaflet addLegend removeControl setView addPolylines
#' @importFrom sf st_bbox
#' @importFrom shiny observe reactiveVal observeEvent is.reactive req
#' @importFrom leafgl addGlPoints
#' @importFrom stats runif
#' @export
shintoMapModule <- function(input, output, session,
                      base_map,

                      render_not_visible = TRUE,

                      border = shiny::reactive(NULL),
                      border_weight = 2,
                      border_color = "#4f4f4f",
                      auto_recenter = FALSE,

                      label_function = reactive(function(data,...)NULL),   # functie om van de id een label (hover) te maken
                      label_params = list(),   # settings passed to label_function (can be anything)

                      color_default = "#023e8a",
                      color_outline = "#FFFFFF",

                      toggle_reload = shiny::reactive(TRUE), # complete map reloaden
                      proxy = TRUE,
                      layers = list()){

  if(shiny::is.reactive(layers)){
    stop("'layers' argument must be a list of reactive's")
  }

  # allow a NULL in layers (easier on the shiny sidex)
  layers <- dropNulls(layers)

  output$txt_out <- shiny::renderPrint({
    shiny::reactiveValuesToList(input)
  })


  ui_ping <- reactiveVal()

  output$map <- leaflet::renderLeaflet({

    toggle_reload()

    # Use shintoBaseMap to make the static leaflet map (with map tiles and a view)
    map <- base_map

    # Border
    shiny::req(border())
    border_data <- shiny::isolate(border())

    if(!is.null(border_data)){

      suppressWarnings({
        map <- map %>%
          leaflet::addPolygons(data = border_data,
                               group = "grens",
                               fill = FALSE,
                               stroke = TRUE,
                               weight = border_weight,
                               color = border_color)
      })

    }

    ui_ping(stats::runif(1))


    if(!proxy){

      for(i in seq_along(layers)){

        lay <- layers[[i]]()
        map <- add_map_layer(map, lay, color_default, color_outline, label_function())
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

        if(is.null(lay$color_outline)){
          lay$color_outline <- color_outline
        }

        if(shiny::is.reactive(label_function)){
          label_function <- label_function()
        }

        suppressWarnings({
          map <- add_map_layer(map = map,
                               lay = lay,
                               color_default = color_default,
                               color_outline = lay$color_outline,
                               label_function = label_function)
        })

      })

    })

  }


  # Return a clicked ID; either from an area or a point.
  clicked_id <- shiny::reactiveVal()

  shiny::observeEvent(input$map_marker_click, {
    clicked_id(input$map_marker_click)
  })

  shiny::observeEvent(input$map_shape_click, {
    clicked_id(input$map_shape_click)
  })

  return(clicked_id)
}
