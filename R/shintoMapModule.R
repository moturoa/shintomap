

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

    # Idee hiervan is dat de kaart altijd opnieuw wordt gerenderd als deze
    # zichtbaar wordt, om de grijze kaart bug te vermijden.
    # werkt alleen niet helemaal lekker, en geeft extra knipperende kaarten,
    # niet wenselijk
    # zie ook a)
    tags$script(glue::glue("

      $(document.body).click( function() {
                const id = '<<<id_map>>>';
        let id_out = id+'_visible';
        let element = document.getElementById(id);
        let viz = $(element).is(':visible');

        Shiny.setInputValue(id_out, viz);
      });



    ", .open = "<<<", .close = ">>>")),

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
                      layers = list()){



  output$txt_out <- shiny::renderPrint({
    shiny::reactiveValuesToList(input)
  })


  ui_ping <- reactiveVal()

  output$map <- leaflet::renderLeaflet({

    toggle_reload()

    # zie a)
    #input$map_visible

    # Use shintoBaseMap to make the static leaflet map (with map tiles and a view)
    map <- base_map

    # # Border
    # border_data <- shiny::isolate(border())
    #
    # if(!is.null(border_data)){
    #   map <- map %>%
    #     leaflet::addPolygons(data = border_data,
    #                 group = "grens",
    #                 fill = FALSE,
    #                 stroke = TRUE,
    #                 weight = border_weight,
    #                 color = border_color)
    # }

    ui_ping(runif(1))

    map

  })

  # Bug fix: map does not load on an unselected tab
  if(render_not_visible){
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)
  }




  map <- leaflet::leafletProxy("map")

  group_names <- reactive({
    sapply(layers, function(layer)layer()$group)
  })

  observeEvent(group_names(), {
    if(any(duplicated(group_names()))){
      stop("Duplicate group names in layers in shintomap, fix please!")
    }
  })

  # Every time either is invalidated, draw near ran nr
  render_map_raw <- reactive({

    input$map_visible
    ui_ping()

    runif(1)
  })

  # now throttle it
  render_map <- throttle(render_map_raw, 1000)


  # Reactive border data
  shiny::observe({

    #input$map_visible
    #ui_ping()
    render_map()
    border_data <- border()

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


  lapply(layers, function(layer){

    shiny::observe({

      #input$map_visible
      #ui_ping()

      render_map()


      lay <- layer()

      lay <- validate_map_layer(lay)

      if(is.null(lay) || is.null(lay$data) || nrow(lay$data) == 0){

        if(is.null(lay$group))lay$group <- "none"

        map <- map %>%
          leaflet::clearGroup(lay$group) %>%
          leaflet::removeControl(paste0(lay$group,"_color_fill_legend"))

      } else {


        # No function to compute colors given; the column contains colors.
        if(is.null(lay$color_function)){

          if(is.null(lay$color_column)){
            color <- color_default  # single color for everything (fallback)
          } else {
            color <- lay$data[[lay$color_column]]   # or, color is present in the data as a column
          }
          lay$data$FILL_COLOR <- color
          lay$data$FILL_OPACITY <- lay$opacity

        } else {  # or, a function has been provided to make the colors

          # Compute colors, add a Legend
          col_fun_name <- lay$color_function$palfunction
          opt <- lay$color_function[-1]
          opt$vals <- lay$data[[lay$color_column]]
          p_color_fun <- do.call(base::get(col_fun_name), opt)

          lay$data$FILL_COLOR <- p_color_fun(lay$data[[lay$color_column]])
          lay$data$FILL_OPACITY <- opt$opacity

        }

        if(lay$toggle){

          if(lay$geom == "CircleMarkers"){

            map <- map %>%
              leaflet::clearGroup(lay$group) %>%
              leaflet::addCircleMarkers(data = lay$data,
                               layerId = lay$data[[lay$id_column]],
                               color = lay$data$FILL_COLOR,
                               radius = lay$radius,
                               group = lay$group,
                               stroke = lay$stroke,
                               weight = lay$weight,
                               label = label_function(lay$data, params = label_params),
                               fillOpacity = lay$data$FILL_OPACITY)

          } else if(lay$geom == "Polygons"){

            map <- map %>%
              leaflet::clearGroup(lay$group) %>%
              leaflet::addPolygons(data = lay$data,
                          layerId = lay$data[[lay$id_column]],
                          fillColor = lay$data$FILL_COLOR,
                          group = lay$group,
                          stroke = lay$stroke,
                          color = color_outline,
                          label = label_function(lay$data, params = label_params),
                          weight = lay$weight,
                          highlightOptions = lay$highlightOptions,
                          fillOpacity = lay$data$FILL_OPACITY)

          } else if(lay$geom == "Polylines"){

            map <- map %>%
              leaflet::clearGroup(lay$group) %>%
              leaflet::addPolylines(data = lay$data,
                                    layerId = lay$data[[lay$id_column]],
                                    stroke = lay$stroke,
                                    weight = lay$weight,
                                    color = lay$data$FILL_COLOR,
                                    group = lay$group,
                                    label = label_function(lay$data, params = label_params))

          } else if(lay$geom == "GlPolygons"){

            map <- map %>%
              leaflet::clearGroup(lay$group) %>%
              leafgl::addGlPolygons(data = lay$data,
                                   layerId = lay$data[[lay$id_column]],
                                   fillColor = lay$data$FILL_COLOR,
                                   group = lay$group,
                                   color = color_outline,
                                   label = label_function(lay$data, params = label_params),
                                   weight = lay$weight,
                                   stroke = lay$stroke,
                                   fillOpacity = lay$data$FILL_OPACITY)

          } else if(lay$geom == "GlPoints"){

            map <- map %>%
              leaflet::clearGroup(lay$group) %>%
              leafgl::addGlPoints(data = lay$data,
                                  layerId = lay$data[[lay$id_column]],
                                  fillColor = lay$data$FILL_COLOR,
                                  fillOpacity = lay$data$FILL_OPACITY,
                                  radius = lay$radius,
                                  group = lay$group)

          }

        } else {
          map <- map %>%
            leaflet::clearGroup(lay$group)
        }


        if(isTRUE(lay$legend$toggle) && lay$toggle){

          if(is.numeric(lay$data[[lay$color_column]])){   # !color_col %in% force_factor &
            brk <- attributes(p_color_fun)$colorArgs$bins

            labs <- paste(brk[1:(length(brk)-1)], brk[2:length(brk)], sep = " - ")
            brk <- brk[-length(brk)]  # last one is the max, not a bin
          } else {
            brk <- levels(as.factor(lay$data[[lay$color_column]]))
            labs <- brk
          }

          legend_cols <- p_color_fun(brk)

          map <- map %>% leaflet::removeControl(paste0(lay$group,"_color_fill_legend")) %>%
            leaflet::addLegend(colors = legend_cols,
                      labels = labs,
                      title = lay$legend$title,
                      layerId = paste0(lay$group,"_color_fill_legend"),
                      #group = lay$group,
                      opacity = lay$legend$opacity,
                      position = lay$legend$position
            )

        } else {
          map <- map %>% leaflet::removeControl(paste0(lay$group,"_color_fill_legend"))
        }

      }

      map
    })

  })

  clicked_id <- shiny::reactiveVal()

  shiny::observeEvent(input$map_marker_click, {
    clicked_id(input$map_marker_click)
  })

  shiny::observeEvent(input$map_shape_click, {
    clicked_id(input$map_shape_click)
  })

  return(clicked_id)
}
