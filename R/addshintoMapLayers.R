#' Add map layers
#' @export
#' @importFrom leaflet addProviderTiles addTiles addWMSTiles WMSTileOptions addLayersControl
addShintoMapLayers <- function(map, default = "Esri.WorldGrayCanvas", position = "topright"){

  tiles <- c(
    "Kaart - grijs (ESRI)" = "Esri.WorldGrayCanvas",
    "OpenStreetMap" = "OpenStreetMap.Mapnik",
    "Satelliet (ESRI)" = "Esri.WorldImagery"
  )

  tiles <- c(
    tiles[match(default,tiles)],
    tiles[-match(default,tiles)]
  )

  for(i in seq_along(tiles)){
    map <- map %>%
      leaflet::addProviderTiles(unname(tiles[i]), group = names(tiles[i]))
  }

  map <- map %>%
    leaflet::addTiles(urlTemplate = "https://service.pdok.nl/brt/achtergrondkaart/wmts/v2_0/standaard/EPSG:3857/{z}/{x}/{y}.png",
             group = "Standaard NL (PDOK BRT)") %>%

    leaflet::addWMSTiles(
      "https://service.pdok.nl/hwh/luchtfotorgb/wms/v1_0",
      layers = "Actueel_orthoHR",
      group = "Luchtfoto Actueel HighRes (PDOK)",
      options = WMSTileOptions(maxNativeZoom = 22, maxZoom = 24)
    ) %>%

    leaflet::addWMSTiles(
      "https://service.pdok.nl/hwh/luchtfotocir/wms/v1_0",
      layers = "Actueel_ortho25IR",
      group = "Luchtfoto Actueel IR (PDOK)",
      options = WMSTileOptions(maxNativeZoom = 22, maxZoom = 24)
    ) %>%

    leaflet::addWMSTiles(
      "https://geodata.nationaalgeoregister.nl/kadastralekaart/wms/v4_0",
      layers = "kadastralekaart",
      group = "Kadastrale kaart",
      options = WMSTileOptions(maxNativeZoom = 18, maxZoom = 22))

  map <- map %>%
    leaflet::addLayersControl(baseGroups = c(names(tiles),
                                    "Standaard NL (PDOK BRT)",
                                    "Luchtfoto Actueel HighRes (PDOK)",
                                    "Luchtfoto Actueel IR (PDOK)",
                                    "Kadastrale kaart"),
                              position = position)

  map
}
