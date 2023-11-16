#' Add map tiles to a leaflet map, shinto style
#' @description Standardized map tiles for Shinto applications. Standaard NL, ESRI gray, OpenStreetMap,
#' Kadaster map (overlayed over standaard NL with transparency), PDOK arial photos (RGB, IR)
#' @param map A Leaflet map object
#' @param default Default selected layer
#' @param position Position of the legend, 'topright', 'bottomleft', etc.
#' @param openstreetmap_extend_zoom If TRUE, extends zoom in OpenStreetMap layer beyond native zoom,
#' so we can zoom in further (with lower resolution). Useful for drawing on map.
#' @param kadaster_opacity Opacity of the kadaster map overlayed over the Standaard NL map. If you don't want to
#' see the standaard NL map, set opacity to 1.0.
#' @export
#' @importFrom leaflet addProviderTiles addTiles addWMSTiles WMSTileOptions addLayersControl providerTileOptions
addShintoMapLayers <- function(map, default = "Esri.WorldGrayCanvas",
                               position = "topright",
                               openstreetmap_extend_zoom = FALSE,
                               kadaster_opacity = 0.5
                               ){


  urls <- list(
    pdok_standaard_nl = "https://service.pdok.nl/brt/achtergrondkaart/wmts/v2_0/standaard/EPSG:3857/{z}/{x}/{y}.png",
    pdok_luchtfoto_rgb = "https://service.pdok.nl/hwh/luchtfotorgb/wms/v1_0",
    pdok_luchtfoto_ir = "https://service.pdok.nl/hwh/luchtfotocir/wms/v1_0",
    geodata_kadaster = "https://geodata.nationaalgeoregister.nl/kadastralekaart/wms/v4_0"
  )

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

    # Max zoom extenden (voor intekenen gebied in WBM zodat je de kaart verder dan de native max zoom kan inzoomen)
    # set maxZoom hoger om nog verder in te kunnen zoomen (met steeds lagere resolutie)
    if(names(tiles[i]) == "OpenStreetMap" & openstreetmap_extend_zoom){
      map <- map %>%
        leaflet::addProviderTiles(unname(tiles[i]), group = names(tiles[i]),
                                  options = leaflet::providerTileOptions(maxNativeZoom = 18, maxZoom = 22)) #
    } else {
      map <- map %>%
        leaflet::addProviderTiles(unname(tiles[i]), group = names(tiles[i]))
    }

  }



  map <- map %>%
    leaflet::addTiles(urlTemplate = urls$pdok_standaard_nl,
             group = "Standaard NL (PDOK BRT)") %>%

    leaflet::addWMSTiles(
      urls$pdok_luchtfoto_rgb,
      layers = "Actueel_orthoHR",
      group = "Luchtfoto Actueel HighRes (PDOK)",
      options = WMSTileOptions(maxNativeZoom = 22, maxZoom = 24)
    ) %>%

    leaflet::addWMSTiles(
      urls$pdok_luchtfoto_ir,
      layers = "Actueel_ortho25IR",
      group = "Luchtfoto Actueel IR (PDOK)",
      options = WMSTileOptions(maxNativeZoom = 22, maxZoom = 24)
    ) %>%

    # Kadaster is semi-transparent over standaard-nl
    leaflet::addTiles(urlTemplate = urls$pdok_standaard_nl,
                      group = "Kadastrale kaart") %>%
    leaflet::addWMSTiles(
      urls$geodata_kadaster,
      layers = "kadastralekaart",
      group = "Kadastrale kaart",
      options = WMSTileOptions(maxNativeZoom = 18, maxZoom = 22, opacity =  kadaster_opacity))

  map <- map %>%
    leaflet::addLayersControl(baseGroups = c(names(tiles),
                                    "Standaard NL (PDOK BRT)",
                                    "Luchtfoto Actueel HighRes (PDOK)",
                                    "Luchtfoto Actueel IR (PDOK)",
                                    "Kadastrale kaart"),
                              position = position)

  map
}
