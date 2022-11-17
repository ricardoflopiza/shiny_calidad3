
renderUI_origen_datos <- function(req){

  if(req == "Bases INE"){

    tagList(
      ## input archivo página del INE
      selectInput("base_web_ine", label = h4("Utilizar una base de datos del INE"),
                  choices = nom_arch_ine,
                  multiple = F),

      actionButton("base_ine", label = "Cargar base desde el INE")
    )

  }else if(req == "Base externa"){
      ## input de archivo local -----
      fileInput(inputId = "file", label = HTML("<h4>Carga una base de datos desde tu computador</h4> <h6>- El peso máximo aceptado es <strong>400 mb</strong> <br>
                                               - Los archivos <strong>.csv</strong> deben estar separados por comas</h6>"),
                buttonLabel = "Buscar" , placeholder = ".sav .rds .dta .sas .xlsx .csv .feather")
  }
}


### modal definicion indicadores ####

modal_indicadores <- function(){
  showModal(modalDialog(
    title = "Definición de indicadores",

    HTML("

<h5><strong>stat:</strong> estimación variable de interés</h5>

<strong><h4>Insumos a evaluar:</h4></strong>

<h5><strong>Ambos estándares</strong></h5>

<h5><strong>se:</strong> error estandar        <br>
<strong>df:</strong> grados de libertad        <br>
<strong>n:</strong> casos muestrales           <br>
<strong>cv:</strong> coeficiente de variación  </h5><br>

<h5><strong>Estandar CEPAL</strong></h5>

<h5><strong>deff:</strong> efecto diseño <br>
<strong>unweighted:</strong> conteo de casos no ponderado <br>
<strong>log_cv:</strong> coeficiente de variación logarítmico <br>
<strong>ess:</strong> tamaño de muestra efectiva <br>
</h5> <br>


<strong><h4>Resultados de la evaluación: </h4></strong>   \n

<h5><strong>Estandar INE Chile</strong></h5>

<h5><strong>eval_n:</strong> evaluación de casos muestrales        <br>
<strong>eval_df:</strong> evaluación de grados de libertad        <br>
<strong>eval_cv:</strong> evaluación del coeficiente de variación        <br>
<strong>calidad:</strong> evaluación final de la celda, puede ser: <p style=\"font-style: italic;\"> Fiable, Poco Fiable o No fiable </p> </h5><br>

<h5><strong>Estandar INE Chile solo en la evaluación de proporciones</strong></h5>

<h5><strong>prop_est:</strong> se refiere si la proporción evaluada es mayor o menor a 0.5, esto determina si se evalua por <strong>se</strong> o por <strong>cv</strong>   <br>
<strong>tipo_eval:</strong> 'Eval SE' o 'Eval CV', la proporción es evaluada por <strong>se</strong> o por <strong>cv</strong> <br>
<strong>cuadrativa:</strong> valor devuelto por la función cuadrática        <br>
 <strong>eval_se:</strong> evaluación del <strong>se</strong> <br> </h5><br>


<h5><strong>Estandar CEPAL</strong></h5>

<h5><strong>eval_n:</strong> evaluación de casos muestrales <br>
<strong>eval_ess:</strong> evaluación tamaño de muestra efectivo <br>
<strong>eval_df:</strong> evaluación grados de libertad <br>
<strong>eval_cv:</strong> evaluación del Coeficiente de variación <br>
<strong>eval_unweighted: </strong> evaluación del conteo de casos no ponderado <br>
<strong>eval_log_cv: </strong> evaluación del coeficiente de variación logarítmico <br>

<strong>calidad:</strong> evaluación final de la celda, puede ser: <p style=\"font-style: italic;\"> Publicar, Revisar o Suprimir </p> </h5>"
    ), easyClose = T, footer = actionButton(inputId = "cerrar_modal", "Cerrar"),
  ))
}

modal_estandar <- function(){

  showModal(
    modalDialog(
    title = "Esquemas de evaluación de calidad",
    HTML("<h5 style='text-align: justify;'>Este aplicativo implementa los estándares de calidad del <strong>Instituto Nacional de Estadísticas de Chile</strong> y de  <strong>CEPAL</strong>.
         Ambos tienen el objetivo de evaluar la calidad de las estimaciones provenientes de encuestas de hogares y ambos utilizan una
         clasificación basada en tres categorías de fiabilidad. La principal diferencia guarda
         relación con la cantidad de indicadores utilizados para llevar a cabo la evaluación de la calidad.
         En el caso del estándar del INE, se consideran los grados de libertad,
         el tamaño de muestra y el coeficiente de variación (o error estándar). Por su parte,
         el estándar de CEPAL considera todo lo anterior y, además, el conteo de casos no ponderado, el tamaño
         de muestra efectivo y el coeficiente de variación logarítmico.</h5>

<em><h6>Para conocer mas de estos estándares puede revisar los documentos en los siguientes links:</h6></em>
<a href= 'https://www.ine.cl/docs/default-source/buenas-practicas/estandares/estandar/documento/estandar-evaluacion-de-calidad-de-estimaciones.pdf' target='_blank'>INE Chile</a> y
<a href= 'https://www.cepal.org/es/publicaciones/45681-criterios-calidad-la-estimacion-indicadores-partir-encuestas-hogares-aplicacion'  target='_blank'>CEPAL</a>
<br>"),
    easyClose = TRUE,
    footer = NULL
    #easyClose = T, footer = actionButton(inputId = "cerrar_modal", "Cerrar"),

    )
  )
}


modal_reset <- function(){

  shinyalert::shinyalert(inputId = "confirm_reset",type = "warning",title =  HTML("<h3><strong>¿Desea eliminar el tabulado actual?</strong></h3>"),#,text = HTML("<h3><strong>¿Desea eliminar el tabulado actual?</strong></h3>"),
                         html = T,showCancelButton = T,cancelButtonText = "Cancelar",
                         confirmButtonText = "Continuar")

  # showModal(
  #   modalDialog(
  #     title = "¡Atención!",
  #     HTML("<h3><strong>¿Desea eliminar el tabulado actual?</strong></h3>"),
  #     easyClose = FALSE, footer = tagList(actionButton(inputId = "cerrar_modal", "Cancelar"),
  #     actionButton(inputId = "confirm_reset", "Confirmar")),
  #
  #   )
  # )
}



#  ess  Effective sample size
#  rm.na  Remove NA if it is required
#  deff  Design effect
#  rel_error  Relative error
#  unweighted  Add non weighted count if it is required "

### render UI main panel ####


renderUI_main_panel <- function(){

tagList(
        div(id="panel_central",class="titu-ine",
              h2("Resultado evaluación de calidad"),
                div(div(style="width:80%; display:inline-block; vertical-align: middle;",
                 #actionButton("show", "Definición de indicadores")),
                 downloadButton("tabla", label = "Descargar tabulado")),
                div(style="display:inline-block; vertical-align: middle;",
                    #downloadButton("tabla", label = "Descargar tabulado"))),
                    shinyBS::bsButton("reset", "refrescar",style = "danger", icon = icon("sync")))),
                div(style="display:inline-block; horizontal-align: right;",
                    actionButton("show", "Definición de indicadores")),
                    #shinyBS::bsButton("reset", "refrescar",style = "danger", icon = icon("sync"))),
                 uiOutput("PRUEBAS2"),
        ### render gráfico de resumen
           div(style='width:100%;overflow-x: scroll;',
            div(plotOutput('grafico'),
                align = "center",
                style = "height:200px"),
            ### render tabulado
            tags$div(
              class="my_table", # set to custom class
              htmlOutput("tabulado") %>% shinycssloaders::withSpinner(color="white"))
        )
     )
  )

}





