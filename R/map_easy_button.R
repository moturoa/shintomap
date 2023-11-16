#' Add buttons on map (topleft corner)
#' @description See example apps for usage.
#' @param id Id for the button (use session$ns("id") server-side)
#' @param icon_file Name of the icon, including the extension (.png or whatever), see inst/icons for list of available icons
#' @param label Label on hover (optional)
#' @export
#' @importFrom htmltools HTML
#' @importFrom leaflet JS
#' @importFrom glue glue
map_easy_button <- function(id, icon_file, label){

  icon_path <- shiny::resourcePaths()["mapicons"]

  if(!file.exists(file.path(icon_path, icon_file))){
    stop(paste("icon_file must be one of:",
               paste(dir(icon_path),collapse=", "))
    )
  }

  icon_file <- file.path("mapicons", icon_file)

  leaflet::easyButton(
    icon = htmltools::HTML(glue::glue("<img src='{icon_file}' height=18 width=18>")),
    title = label,
    onClick = leaflet::JS(glue::glue("function(btn, map){ Shiny.setInputValue('<<<id>>>', Math.random());}",
                      .open = "<<<", .close = ">>>")))
}
