#' Makes a leaflet map for use in shintoMapModule
#' @export
shintoBaseMap <- function(set_view = NULL,
                          fullscreen_option = FALSE,
                          layers_button_position = "topright",
                          ...){

  map <- leaflet::leaflet() %>%
    addShintoMapLayers(..., position = layers_button_position)


  if(fullscreen_option){
    map <- map %>%
      addFullscreenControl(pseudoFullscreen=TRUE)
  }

  # Set the initial location
  # --> if set_view used, map does not rescale when border() changes
  # maybe not ideal
  if(!is.null(set_view)){

    map <- map %>%
      leaflet::setView(
        lng = set_view$lng,
        lat = set_view$lat,
        zoom = set_view$zoom
      )

  }

  map
}
