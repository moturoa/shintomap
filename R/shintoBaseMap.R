#' Makes a leaflet map for use in shintoMapModule
#' @param set_view List with components `lng`, `lat` and `zoom` for initial view of the map
#' @param fullscreen_option If TRUE, adds a fullscreen button (from leaflet.extras), slightly buggy sometimes
#' @param layers_button_position Where to place the button for the layer options (default topright)
#' @param ... Further arguments passed to [addShintoMapLayers()]
#' @export
#' @importFrom leaflet.extras addFullscreenControl
shintoBaseMap <- function(set_view = NULL,
                          fullscreen_option = FALSE,
                          layers_button_position = "topright",
                          ...){

  map <- leaflet::leaflet() %>%
    addShintoMapLayers(..., position = layers_button_position)


  if(fullscreen_option){
    map <- map %>%
      leaflet.extras::addFullscreenControl(pseudoFullscreen=TRUE)
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
