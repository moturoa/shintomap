

# Example app with map on tab1, filters on tab2.
# this has caused bugs in the past (I believe)
# this works fine though
# Another 'bug' happens with two layers, one on (toggled), one off, and accidentally
# given the same 'group' name to both layers!
# group shouldn't have to be input name?

library(shiny)
library(softui)
library(leaflet)
library(shinyWidgets)

devtools::load_all()

geo <- readRDS("geo_Eindhoven.rds")
buurt_locaties <- sf::st_centroid(geo$buurten)

mymapui <- function(id){
  ns <- NS(id)

  tags$div(
     materialSwitch(ns("tog1"), "Toggle Area", value = TRUE),
     materialSwitch(ns("tog2"), "Toggle Points", value = FALSE),
     shintoMapUI(ns("map"), height = 800)
  )

}

mymapserver <- function(input, output, session, map_data = reactive(NULL), ...){



  my_base_map <- shintoBaseMap(set_view = list(
    lng = 5.463155,
    lat = 51.4497,
    zoom = 12
  )) %>%
    addEasyButtonBar(
      map_easy_button(session$ns("btn_help"), icon_file = "question-circle-fill.svg",
                      label = "Click here for help"),
      map_easy_button(session$ns("btn_fill_color"), icon_file = "palette-fill.svg",
                      label = "Kleuren instellingen")
    )

  callModule(shintoMapModule, "map",
             base_map = my_base_map,
             render_not_visible = TRUE, # for use in modal!
             border = reactive(geo$grens),
             label_function = function(data,...)data$bu_naam,
             ...,

             layers = list(

               # Layer: gebouwen
               reactive(

                 list(
                   data = map_data(),
                   toggle = input$tog1,
                   group = "area_layer",
                   geom = "Polygons",
                   id_column = "bu_code",
                   color_column = "a_inw",
                   stroke = FALSE,

                   color_function = list(
                     palfunction = "shinto_auto_color",
                     palette_function = "parula",
                     reverse = FALSE,
                     opacity = 0.8
                   ),
                   legend = list(
                     toggle = TRUE,
                     title = "Aantal inwoners",
                     position = "bottomright",
                     opacity = 0.9
                   )

                 )

               ),

               reactive(

                 list(
                   data = buurt_locaties,
                   toggle = input$tog2,
                   group = "point_layer",
                   geom = "CircleMarkers",
                   id_column = "bu_code"

                 )

               )

             )
  )

}


ui <- softui::simple_page(
  softui::tab_box(width = 6,
    softui::tab_panel(
      title = "shintomap", icon = bsicon("geo-alt-fill"),
      mymapui("map")
    ),
    softui::tab_panel(
      title = "filters", icon = bsicon("filter"),
      softui::virtual_select_input("sel_buurt", "Buurten", multiple = TRUE, choices= geo$buurten$bu_naam)
    )

  )

)

server <- function(input, output, session) {


  buurten_data <- reactive({
    if(is.null(input$sel_buurt)){
      geo$buurten
    } else {
      dplyr::filter(geo$buurten, bu_naam %in% !!input$sel_buurt)
    }
  })


  callModule(mymapserver, "map", map_data = buurten_data)


}

shinyApp(ui, server)

