library(shiny)
library(dplyr)

source("calidad/utils.R")

datos <- feather::read_feather("calidad/data/shiny/enusc_2018.feather")

need_warning(tipo_calc = "Media",datos = datos,var = "rph_edad")
need_warning(tipo_calc = "Proporción",datos = datos,var = "rph_edad")


need_warning(tipo_calc = "Media",datos = datos,var = "vp_dc")
need_warning(tipo_calc = "Suma variable continua",datos = datos,var = "vp_dc")

es_prop <- datos %>%
  dplyr::mutate(es_prop_var = dplyr::if_else("vp_dc" == 1 | "vp_dc" == 0 | is.na("vp_dc"),1,0))

even <- sum(es_prop$es_prop_var) == nrow(es_prop)


need_warning <-function(tipo_calc,datos,var){

  ## media. suma var continua, deben ser continuas
  if(tipo_calc %in% c("Media","Suma variable continua")) {

    es_prop <- datos %>%
      dplyr::mutate(es_prop_var = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0 | is.na(!!rlang::parse_expr(var)),1,0))

    even <- sum(es_prop$es_prop_var) == nrow(es_prop)

    even

    ## proporción y conteo de casos, deben ser dummy
  }else if(tipo_calc %in% c("Proporción","Conteo casos")){

    es_prop <- datos %>%
      dplyr::mutate(es_prop_var = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0 | is.na(!!rlang::parse_expr(var)),1,0))

    even <- sum(es_prop$es_prop_var) != nrow(es_prop)

    even
  }


}


ui <- fluidPage(
  selectInput("varINTERES","var interes",choices = names(datos)),
  radioButtons("tipoCALCULO", "¿Qué tipo de cálculo deseas realizar?",
               choices = list("Media","Proporción","Suma variable continua","Conteo casos"), inline = F ),
  textOutput("tabla")

)

server <- function(input, output, session) {


observeEvent(list(input$tipoCALCULO,input$varINTERES),{
  print(datos[input$varINTERES])

  print(need_warning(tipo_calc = input$tipoCALCULO ,datos = datos,var = input$varINTERES))

  shinyFeedback::feedbackWarning("varINTERES",show =  need_warning(tipo_calc = input$tipoCALCULO ,datos = datos,var = input$varINTERES),text =  "¡La variable no es continua!")
})

output$tabla <- renderText({

  mean(datos[input$varINTERES],na.rm = T)

})


}

shinyApp(ui, server)
