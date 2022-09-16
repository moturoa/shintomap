#' Makes a leaflet map for use in shintoMapModule
#' @export
shintoBaseMap <- function(set_view = NULL, ...){

  map <- leaflet::leaflet() %>%
    addShintoMapLayers(...)

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
