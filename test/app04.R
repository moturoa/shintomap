
# shintomap example app 04

# - Multiple layers on 1 map, switch on and off with toggles
# - Filters on another tab, that filter the areas shown on the map
# - Buttons on the map, open a modal, make some map setting
# - Use of selectinput below the map to adjust another setting

# Dependencies
library(shiny)
library(softui)
library(leaflet)
library(shinyWidgets)

devtools::load_all()

# Prepare the data
geo <- readRDS("geo_Eindhoven.rds")
geo$buurten$p_man <- geo$buurten$a_man / geo$buurten$a_inw

# some points for plotting
buurt_locaties <- sf::st_centroid(geo$buurten)

# Shiny UI module
mymapui <- function(id){
  ns <- NS(id)

  softui::fluid_page(

    softui::fluid_row(
      column(6,
             materialSwitch(ns("tog1"), "Toggle Area", value = TRUE),
             materialSwitch(ns("tog2"), "Toggle Points", value = FALSE)
             ),
      column(6,
             selectInput(ns("sel_palette"), "Palette",
                         choices = c("parula","kovesi.rainbow","viridis")), # functions in the pals package
             )
    ),
    softui::fluid_row(
      shintoMapUI(ns("map"), height = 800)
    ),

     verbatimTextOutput(ns("txt_out"))
  )

}

# Module server
mymapserver <- function(input, output, session, map_data = reactive(NULL), ...){


  output$txt_out <- renderPrint({
    reactiveValuesToList(input)
  })


  my_base_map <- shintoBaseMap(set_view = list(
    lng = 5.463155,
    lat = 51.4497,
    zoom = 12
  )) %>%
    addEasyButtonBar(
      map_easy_button(session$ns("btn_color_column"), icon_file = "layers-fill.svg",
                      label = "Kies kenmerk om kaart te kleuren"),
      map_easy_button(session$ns("btn_help"), icon_file = "question-circle-fill.svg",
                      label = "Click here for help")
    )


  color_column <- softui::modalize(
    ui_module = function(id){
      ns <- NS(id)
      softui::virtual_select_input(ns("sel_var"), "Kenmerk",
                                   choices = c("a_inw","p_man", "a_opp_ha", "bev_dich"),
                                   selected = "a_inw")
    },
    server_module = function(input, output, session){
      reactive(input$sel_var)
    },
    trigger_open = reactive(input$btn_color_column),
    default_output = "a_inw",
    title = "Kies kenmerk om kaart te kleuren"
  )


  callModule(shintoMapModule, "map",
             base_map = my_base_map,
             render_not_visible = TRUE, # for use in modal!
             border = reactive(geo$grens),
             #toggle_reload = reactive(input$`map-map_visible`),
             label_function = reactive(function(data,...)data$bu_naam),
             ...,

             layers = list(

               # Layer: areas
               reactive(

                 list(
                   data = map_data(),
                   toggle = input$tog1,
                   group = "area_layer",
                   geom = "Polygons",
                   id_column = "bu_code",
                   color_column = color_column(),
                   stroke = FALSE,

                   color_function = list(
                     palfunction = "shinto_auto_color",
                     palette_function = input$sel_palette,
                     reverse = FALSE,
                     n = 10,
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

               # Layer: points
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
      title = "shintomap",
      icon = bsicon("geo-alt-fill"),
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

