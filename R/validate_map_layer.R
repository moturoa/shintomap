
# validates, fixes, and sets default values for a layer

validate_map_layer <- function(layer){


    keys <- names(layer)
    have <- function(x)x %in% keys

    # Reset group to avoid bug with duplicate group name.
    # if(have("group")){
    #   message("No need to set 'group' in shintomap layer definition")
    # }
    #
    # layer$group <- random_id()

    # layerId not always needed, but I think it crashes without it
    if(!have("id_column")){
      message("Set id_column in shintomap to process map clicks")
      layer$data$shintomap_fake_id <- seq_len(nrow(lay$data))
      layer$id_column <- "shintomap_fake_id"
    }

    if(!have("geom")){
      stop(paste("Must specify 'geom' in shintomap layer, options:",
                 paste(shintomap_allowed_geoms, collapse = ", ")))
    }
    if(!assert_map_geom(layer$geom)){
      stop(paste("Unsupported 'geom' in shintomap layer, options:",
                 paste(shintomap_allowed_geoms, collapse = ", ")))
    }

    never_allowed <- c("fillColor","color")
    if(any(have(never_allowed))){
      message(paste("The following settings are always ignore in shintomap layer: ",
                    paste(never_allowed, collapse = ", ")))
    }

    if(have("popup"))message("'popup' ignored in shintomap (use reactive click and a modal instead)")
    if(have("color_function") & have("opacity")){
      message("'opacity' ignored in shintomap layer when 'color_function' provided")
    }
    if(!have("color_function") & !have("opacity")){
      layer$opacity <- 0.8
    }


    # geom-specific settings
    if(layer$geom == "CircleMarkers"){

        if(!have("radius"))layer$radius <- 5
        if(!have("weight"))layer$weight <- 1
        if(!have("stroke"))layer$stroke <- TRUE
    }


    if(layer$geom == "Polygons"){

      if(!have("weight"))layer$weight <- 1
      if(!have("stroke"))layer$stroke <- TRUE
    }

    if(layer$geom == "Polylines"){

      if(!have("weight"))layer$weight <- 2
      if(!have("stroke"))layer$stroke <- TRUE

    }

    if(layer$geom == "GlPoints"){

      if(!have("radius"))layer$radius <- 5
    }


layer
}




