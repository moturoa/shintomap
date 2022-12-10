
#- internal function used in shintoMapModule to add an actual polygon/point layer to a leaflet map

add_map_layer <- function(map, lay, color_default, color_outline, label_function){

  lay <- validate_map_layer(lay)

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
      opt$vals <- lay$data[[lay$color_column]]
      p_color_fun <- do.call(base::get(col_fun_name), opt)

      lay$data$FILL_COLOR <- p_color_fun(lay$data[[lay$color_column]])
      lay$data$FILL_OPACITY <- opt$opacity

    }

    if(!is.null(lay$toggle) && lay$toggle){

      if(is.null(lay$geo_column)){
        map_data <- lay$data
      } else {
        map_data <- lay$data[[lay$geo_column]]
      }


      if(lay$geom == "CircleMarkers"){

        map <- map %>%
          leaflet::clearGroup(lay$group) %>%
          leaflet::addCircleMarkers(data = map_data,
                                    layerId = lay$data[[lay$id_column]],
                                    color = lay$data[["FILL_COLOR"]],
                                    radius = lay$radius,
                                    group = lay$group,
                                    stroke = lay$stroke,
                                    weight = lay$weight,
                                    label = label_function(lay$data, params = label_params),
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

        if(is.numeric(val_for_color)){   # !color_col %in% force_factor &
          brk <- attributes(p_color_fun)$colorArgs$bins

          labs <- paste(brk[1:(length(brk)-1)], brk[2:length(brk)], sep = " - ")
          brk <- brk[-length(brk)]  # last one is the max, not a bin
        } else {
          brk <- levels(as.factor(val_for_color))
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


  }

  map
}


