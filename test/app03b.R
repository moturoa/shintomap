



# fix of example 03:
# ui_modal ftw


library(shiny)
library(softui)
library(leaflet)

devtools::load_all()


geo <- readRDS("geo_Eindhoven.rds")


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
             render_not_visible = TRUE, # for use in modal!
             border = reactive(geo$grens),
             label_function = reactive(function(data,...)data$bu_naam),
             ...,

             layers = list(

               # Layer: gebouwen
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





ui <- softui::simple_page(
  softui::box(title = "shintomap", icon = bsicon("geo-alt-fill"), width = 6,

              softui::modal_action_button("btn_show_map", modalId = "modal_kaart",
                                          "Toon kaart", status = "info"),
              softui::virtual_select_input("sel_buurt", "Buurten", multiple = TRUE, choices= geo$buurten$bu_naam),


              softui::ui_modal(
                id = "modal_kaart",
                title = "Buurten select",
                size = "xl",
                mymapui("mappy")
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

  callModule(mymapserver, "mappy", map_data = buurten_data)




}

shinyApp(ui, server)

