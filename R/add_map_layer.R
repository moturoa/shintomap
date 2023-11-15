
#- internal function used in shintoMapModule to add an actual polygon/point layer to a leaflet map

add_map_layer <- function(map, lay, color_default, color_outline, label_function){

  lay <- validate_map_layer(lay)

  # label_function can be a global arg (used for all layers),
  # or a layer-specific arg (overrides global, if defined)
  if(!is.null(lay$label_function)){
    label_function <- lay$label_function
  }


  if(is.reactive(lay$data)){
    stop("reactive passed inside config for shintomap::add_map_layer")
  }

  if(is.null(lay) || is.null(lay$data) || nrow(lay$data) == 0){

    if(is.null(lay$group))lay$group <- "none"

    map <- map %>%
      leaflet::clearGroup(lay$group) %>%
      leaflet::removeControl(paste0(lay$group,"_color_fill_legend"))

  } else {


    # No function to compute colors given; the column contains colors.
    if(is.null(lay$color_function) | is.null(lay$color_column)){

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

      # The provided column is not present in the data (fallback colors + warning)
      if(!hasName(lay$data, lay$color_column)){

        warning(paste("Color column", lay$color_column, "not found in data, using default color"))
        lay$data$FILL_COLOR <- color_default
        lay$data$FILL_OPACITY <- lay$opacity

      # otherwise compute the colors, using the provided function name (usually: shinto_auto_color)
      } else {

        opt$vals <- lay$data[[lay$color_column]]
        opt$force_factor <- any(lay$color_column %in% lay$force_factor)  # T/F, sent to shinto_auto_color

        # Send all args to the function
        p_color_fun <- do.call(base::get(col_fun_name), opt)

        lay$data$FILL_COLOR <- p_color_fun(lay$data[[lay$color_column]])

        lay$data$FILL_OPACITY <- opt$opacity
      }


    }

    if(is.null(lay$toggle) || lay$toggle){

      if(is.null(lay$geo_column)){
        map_data <- lay$data
      } else {
        map_data <- lay$data[[lay$geo_column]]
      }


      if(lay$geom == "CircleMarkers"){

        if(is.null(lay$clustering)){
          clus <- NULL
        } else {

          if(is.logical(lay$clustering)){
            if(isTRUE(lay$clustering)){
              clus <- markerClusterOptions()
            } else {
              clus <- NULL
            }

          } else {
            clus <- do.call(markerClusterOptions, lay$clustering)
          }

        }

        if(isTRUE(lay$label_fixed)){
          lab_options <-  labelOptions(noHide = TRUE, textsize = 10, textOnly = TRUE)
        } else {
          lab_options <- NULL
        }


        map <- map %>%
          leaflet::clearGroup(lay$group) %>%
          leaflet::addCircleMarkers(data = map_data,
                                    layerId = lay$data[[lay$id_column]],
                                    color = color_outline,
                                    fillColor = lay$data[["FILL_COLOR"]],
                                    radius = lay$radius,
                                    group = lay$group,
                                    stroke = lay$stroke,
                                    weight = lay$weight,
                                    clusterOptions = clus,
                                    label = label_function(lay$data, params = label_params),
                                    labelOptions = lab_options,
                                    opacity = lay$opacity,
                                    fillOpacity = lay$data[["FILL_OPACITY"]])

      } else if(lay$geom == "Polygons"){

        map <- map %>%
          leaflet::clearGroup(lay$group) %>%
          leaflet::addPolygons(data = map_data,
                               layerId = lay$data[[lay$id_column]],
                               fillColor = lay$data[["FILL_COLOR"]],
                               group = lay$group,
                               stroke = lay$stroke,
                               color = color_outline,
                               label = label_function(lay$data, params = label_params),
                               weight = lay$weight,
                               highlightOptions = lay$highlightOptions,
                               fillOpacity = lay$data[["FILL_OPACITY"]])

      } else if(lay$geom == "Polylines"){

        map <- map %>%
          leaflet::clearGroup(lay$group) %>%
          leaflet::addPolylines(data = map_data,
                                layerId = lay$data[[lay$id_column]],
                                stroke = lay$stroke,
                                weight = lay$weight,
                                color = lay$data[["FILL_COLOR"]],
                                group = lay$group,
                                label = label_function(lay$data, params = label_params))

      } else if(lay$geom == "GlPolygons"){

        map <- map %>%
          leaflet::clearGroup(lay$group) %>%
          leafgl::addGlPolygons(data = map_data,
                                layerId = lay$data[[lay$id_column]],
                                fillColor = lay$data[["FILL_COLOR"]],
                                group = lay$group,
                                color = color_outline,
                                label = label_function(lay$data, params = label_params),
                                weight = lay$weight,
                                stroke = lay$stroke,
                                fillOpacity = lay$data[["FILL_OPACITY"]])

      } else if(lay$geom == "GlPoints"){

        map <- map %>%
          leaflet::clearGroup(lay$group) %>%
          leafgl::addGlPoints(data = map_data,
                              layerId = lay$data[[lay$id_column]],
                              fillColor = lay$data[["FILL_COLOR"]],
                              fillOpacity = lay$data[["FILL_OPACITY"]],
                              radius = lay$radius,
                              group = lay$group)

      }

    } else {
      map <- map %>%
        leaflet::clearGroup(lay$group)
    }


    if(!is.null(lay$color_column)){

      val_for_color <- lay$data[[lay$color_column]]

      if(isTRUE(lay$legend$toggle) && lay$toggle && !all(is.na(val_for_color))){

        if(isTRUE(lay$color_function$method == "predefined")){

          legend_cols <- lay$color_function$colors
          labs <- lay$color_function$labels

          # Color that is used for levels that could not be matched (e.g. "Gemengd")
          # This does not apply to numeric because they are forced inside the bins anyway
          if(!is.numeric(val_for_color) && !is.null(lay$color_function$color_other)){

            mixlab <- lay$color_function$label_other
            if(is.null(mixlab))mixlab <- "label_other"
            labs <- c(labs, mixlab)
            legend_cols <- c(legend_cols, lay$color_function$color_other)

          }

        } else if(is.numeric(val_for_color) && !lay$color_col %in% lay$force_factor){

            brk <- attributes(p_color_fun)$colorArgs$bins

            labs <- paste(brk[1:(length(brk)-1)], brk[2:length(brk)], sep = " - ")
            brk <- brk[-length(brk)]  # last one is the max, not a bin
            legend_cols <- p_color_fun(brk)

        } else {

          brk <- levels(as.factor(val_for_color))
          labs <- brk
          legend_cols <- p_color_fun(brk)
        }

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


  }

  map
}


