

shintomap_allowed_geoms <- c("Polygons","Polylines","GlPolygons","GlPoints","CircleMarkers")

assert_map_geom <- function(x){

  x %in% shintomap_allowed_geoms

}
