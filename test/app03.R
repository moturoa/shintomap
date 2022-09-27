


library(shiny)
library(softui)
library(leaflet)

devtools::load_all()


geo <- readRDS("geo_Eindhoven.rds")


mymapui <- function(id){
  ns <- NS(id)

  shintoMapUI(ns("map"), height = 800)
}

mymapserver <- function(input, output, session){

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
             render_not_visible = FALSE, # for use in modal!
             border = reactive(geo$grens),
             label_function = function(data,...)data$bu_naam,

             layers = list(

               # Layer: gebouwen
               reactive(

                 list(
                   data = geo$buurten,
                   toggle = TRUE,
                   group = "area_layer",
                   geom = "Polygons",
                   id_column = "bu_code",
                   color_column = "a_inw",

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

               )

             )
  )

}





ui <- softui::simple_page(
  softui::box(title = "shintomap", icon = bsicon("geo-alt-fill"), width = 6,

              softui::action_button("btn_show_map", "Toon kaart", status = "info")
  )
)

server <- function(input, output, session) {

    softui::modalize(trigger_open = reactive(input$btn_show_map),ui_module = mymapui,
                     server_module = mymapserver, size = "xl")



}

shinyApp(ui, server)

