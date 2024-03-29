
# shintomap example app 03

# Example app with map in a modal, using softui::modalize,
# and the code for the map (ui+server) in a shiny module.
# Bug avoidance: see ca. line 114. Without this adjustment, the map loads
# the first time the modal is opened, but not the second time.
# Map has to be reloaded every time the modal is triggered...

# "Most of the time" a better solution is example app 03b. It has the same functionality
# as this one, but puts the map in a 'ui modal', which means the map does not get reloaded
# every time we click the button.

# Dependencies
library(shiny)
library(softui)
library(leaflet)

# Load this package (or do library(shintomap))
devtools::load_all()

# Data (in git)
geo <- readRDS("geo_Eindhoven.rds")

# Module UI, to test operation inside a module (inside a module (inside a module))
mymapui <- function(id){
  ns <- NS(id)

  tags$div(
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
             render_not_visible = FALSE, # for use in modal!
             border = reactive(geo$grens),
             label_function = reactive(function(data,...)data$bu_naam),
             ...,

             layers = list(

               # Layer: areas
               reactive(

                 list(
                   data = map_data(),
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




# Main shiny app
ui <- softui::simple_page(
  softui::box(title = "shintomap", icon = bsicon("geo-alt-fill"), width = 6,

              softui::action_button("btn_show_map", "Toon kaart", status = "info"),
              softui::virtual_select_input("sel_buurt", "Buurten", multiple = TRUE, choices= geo$buurten$bu_naam)
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


    softui::modalize(trigger_open = reactive(input$btn_show_map),
                     ui_module = mymapui,
                     server_module = mymapserver, size = "l",
                     server_pars = list(map_data = buurten_data,
                                        toggle_reload = reactive(input$btn_show_map)))  # BUG avoid!!



}

shinyApp(ui, server)

