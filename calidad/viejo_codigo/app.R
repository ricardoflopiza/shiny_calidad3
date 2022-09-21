# https://stackoverflow.com/questions/42422310/assigning-custom-css-classes-to-designed-object-via-external-css-file

## Pendientes ####

# La base de la ENUT se descarga

### carga de paquetes ####
library(shinyFeedback)
library(calidad)
library(shiny)
library(haven)
library(labelled)
library(dplyr)
library(openxlsx)
library(sjmisc)
library(readxl)
library(survey)
library(shinyWidgets)
library(rlang)
library(kableExtra)
library(shinycssloaders)
library(readr)
library(shinybusy)
library(shinyalert)
library(writexl)
library(shinyjs)
library(tibble)
library(plotly)

# Cargar funciones ####
#source("download_data.R")
source("create_tabulado.R")
source("create_plot.R")
#source("utils.R")

#archivos_ine <- list.files("calidad/data/")
archivos_ine <- list.files("data/",pattern = ".feather") #c("enusc_2018.feather","enusc_2019.feather","enusc_2020.feather","epf_hogares.feather","epf_personas_ing.feather","esi_2020.feather")

nom_arch_ine <- archivos_ine %>% stringr::str_remove_all(".feather")

# posibilidad nombres de variables DC"
fact_exp = c("Fact_pers","Fact_Pers","fe","fact_cal", "fact_pers","fact_pers","fe","fact_cal", "fact_cal_esi","FACT_PERS","FACT_PERS","FE","FACT_CAL","FACT_CAL_ESI","wgt1","EXP","exp","Exp")
conglomerados = c("Conglomerado", "id_directorio","varunit", "conglomerado","id_directorio","varunit","CONGLOMERADO","ID_DIRECTORIO","VARUNIT","VarUnit")
estratos = c("VarStrat", "estrato", "varstrat","varstrat", "estrato",  "varstrat","VARSTRAT", "ESTRATO",  "VARSTRAT","VarStrat")

### DEBUG ####
debug = F
show_wrn = T

# UI ----

ui <- div(shinyFeedback::useShinyFeedback(),
          shinyjs::useShinyjs(),
          # tags$head(
          #   tags$link(rel = "stylesheet", type = "text/css", href = "maqueta.css")
          # ),
          includeCSS("www/maqueta.css"),

          div(class="top-ine",
              fluidPage(
                div(class="container",
                    HTML('<div class="menu-ine">
                <img class="logo-ine" src="ine_blanco.svg" alt="INE">
            </div>
            <div class="pull-right">
                <a class="btn btn-xs btn-primary" href="https://www.ine.cl" target="_blank">Volver al home INE</a>
            </div>'),
                )
              )
          ),
          div(class="conten-ine",

              ### fluid page de texto de descripción
              fluidPage(
                #   div(class="container-fluid",
                div(class="container",
                    HTML('<div class="row">
                <div class="col-md-12">
                    <h3 class="titu-ine">Evaluación de Calidad de Estimaciones en Encuestas de Hogares</h3>
                    <p class="text-ine">
Esta aplicación permite acercar a las personas usuarias la implementación del estándar de calidad para la evaluación de estimaciones en encuestas de hogares del INE. A través de ella, las personas usuarias pueden conocer la precisión que tienen las estimaciones generadas a partir de encuestas producidas por el INE u otras encuestas que utilicen muestreo probabilístico estratificado y en 2 etapas. Con esto se busca poner a disposición de la comunidad una herramienta interactiva para la cual no se requiere contar con conocimientos de programación, promoviendo el uso adecuado de la información publicada. Esta aplicación permite evaluar la calidad de la estimación de medias, totales, medianas y proporciones.                    </p>
                </div>
            </div>')
                )
              )
          ),
          # Agregar el logo del INE

          div(class="dash-ine",
              fluidPage(
                div(class="container",
                    sidebarLayout(
                      ## Sidebar ####
                      sidebarPanel(width = 3,
                                   ## UI INPUT ####
                                   radioGroupButtons(
                                     inputId = "Id004",
                                     label = h4("Selecciona desde donde cargar base de datos"),
                                     choices = c("Cargar datos propios", "Trabajar con datos INE"),
                                     status = "primary",
                                     justified = TRUE
                                   ),
                                   h5("En esta Sección puedes seleccionar la opción de cargar una base de datos desde tu computador, o cargar una base de datos del INE"),
                                   uiOutput("datos_locales"),
                                   uiOutput("DescargaINE"),
                                   ## render selección de variables de interes, y de cruce
                                   # uiOutput("seleccion1"),
                                   selectInput("varINTERES", label = h5("Variable de interés"),choices = "",  multiple = F),
                                   #textOutput("wrn_var_int"),

                                   uiOutput("denominador"),

                                   radioGroupButtons(
                                     inputId = "SCHEME",
                                     label = h5("Selecciona el esquema de evaluación, INE o CEPAL"),
                                     choices = c("chile", "cepal"),
                                     status = "primary",
                                     justified = TRUE
                                   ),

                                   radioButtons("tipoCALCULO", "¿Qué tipo de cálculo deseas realizar?",
                                                choices = list("Media","Proporción","Suma variable Continua","Conteo casos", "Mediana"), inline = F ),
                                   selectInput("varCRUCE", label = h5("Desagregación"), choices = "", selected = NULL, multiple = T),
                                   checkboxInput("IC", "¿Deseas agregar intervalos de confianza?",value = F),
                                   #checkboxInput("ajuste_ene", "¿Deseas agregar los ajuste del MM ENE?",value = F),
                                   uiOutput("etiqueta"),
                                   selectInput("varSUBPOB", label = h5("Subpoblación"), choices = "", selected = "", multiple = F),
                                   selectInput("varFACT1", label = h5("Variable para factor de expansión"), choices = "",selected ="", multiple = F),
                                   selectInput("varCONGLOM", label = h5("Variable para conglomerados"), choices = "", selected = "", multiple = F),
                                   selectInput("varESTRATOS",label = h5("Variable para estratos"), choices = "", selected = "", multiple = F),
                                   disabled(downloadButton("tabla", label = "Descargar tabulado")),
                                   actionButton("actionTAB", label = "Generar tabulado"),
                                   ## render selección variables DC
                                   uiOutput("seleccion2"),
                                   ## botón generación tabulado
                                   uiOutput("botonTAB")
                      ),
                      ## Main PANEL ----
                      mainPanel(width = 9,
                                verbatimTextOutput("tipoCalText"),
                                #### render titulo tabulado
                                uiOutput("tituloTAB")
                      )
                    )
                )
              )
          ),
          div(class="footer",
              fluidPage(
                div(class="container",
                    HTML('<div class="row">
                <div class="col-md-4">
                    <h4>INE en redes sociales</h4>
                    <a href="https://www.facebook.com/ChileINE/" target="_blank"><img class="facebook" src="facebook.svg"></a>
                    <a href="https://twitter.com/ine_chile?lang=es" target="_blank"><img class="twitter" src="twitter.svg"></a>
                    <a href="https://www.youtube.com/user/inechile" target="_blank"><img class="youtube" src="youtube.svg"></a>
                    <a href="https://www.instagram.com/chile.ine/" target="_blank"><img class="instagram" src="instagram.svg"></a>
                    <h4>Consultas</h4>
                    <p><a href="https://www.portaltransparencia.cl/PortalPdT/ingreso-sai-v2?idOrg=1003" target="_blank">Solicitud de acceso a la información pública</a></p>
                    <p><a href="https://atencionciudadana.ine.cl/" target="_blank">Atención ciudadana</a></p>
                </div>
                <div class="col-md-4">
                    <h4>Contacto</h4>
                    <p>
                        Dirección nacional: Morandé N°801, piso 22, Santiago, Chile<br>
                        RUT: 60.703.000-6<br>
                        Código postal: 8340148<br>
                    </p>
                </div>
                <div class="col-md-4">
                    <h4>SIAC / OIRS</h4>
                    <p>
                        Horario de atención:<br>
                        Lunes a viernes 9:00 a 17:00 horas<br>
                        Fono : <a>232461010</a> - <a>232461018</a><br>
                        Correo: ine@ine.cl<br>
                    </p>
                </div>
            </div>')
                )
              )
          ),
          div(class="pie-ine",
              fluidPage(
                div(class="container",
                    HTML('
        <div class="text-right">
            Instituto Nacional de Estadísticas
       </div>')
                )
              )
          )
)

# SERVER ----
server <- function(input, output, session) {
  ### trakear error ####
  ## options(shiny.trace = TRUE)

  ## parametro máximo de archivos subidos ###
  options(shiny.maxRequestSize=30*1024^2, scipen=999) # to the top of server.R would increase the limit to 30MB.

  output$datos_locales = renderUI({
    req(input$Id004 == "Cargar datos propios")
    tagList(
      ## input de archivo local -----
      fileInput(inputId = "file", label = h4("Carga una base de datos desde tu computador"),buttonLabel = "Buscar" , placeholder = ".sav .rds .dta .sas .xlsx .csv .feather")
    )
  })


  output$DescargaINE = renderUI({
    req(input$Id004 == "Trabajar con datos INE")
    tagList(
      ## input archivo página del INE
      selectInput("base_web_ine", label = h4("Utilizar una base de datos del INE"),
                  choices = nom_arch_ine,
                  multiple = F),

      actionButton("base_ine", label = "Cargar base desde el INE"),

    )
  })


  ### + I N P U T S + ####

  ### CARGA: base local ####
  # MODAL carga archivo formato no soportado ####

  ## formatos soportados
  soportados <- c(".sav",".rds",".dta", ".sas", ".xlsx", ".csv",".feather")

  observeEvent(input$file ,{

    req(!any(grepl(paste(soportados,collapse = "|"), tolower(input$file$datapath))))
    # tiene que haber warning para que se muestre
    shinyalert("¡Error!", "¡Formato de archivo no soportado!", type = "error")
  })


  data_input <- reactive({
    req(any(grepl(paste(soportados,collapse = "|"), input$file$datapath)))

    if(grepl(".sav", input$file$datapath)){

      data <- haven::read_sav(input$file$datapath)

    } else if(grepl(".rds", tolower(input$file$datapath))){

      data <-  readRDS(input$file$datapath)
    } else if(grepl(".dta", tolower(input$file$datapath))){

      data <- haven::read_dta(input$file$datapath)
    } else if(grepl(".sas", tolower(input$file$datapath))){

      data <- haven::read_sas(input$file$datapath)
    }  else if(grepl(".xlsx", tolower(input$file$datapath))){

      data <- readxl::read_excel(input$file$datapath)
    } else if(grepl(".csv", tolower(input$file$datapath))){

      data <-  read_delim(input$file$datapath, delim = ';')
    }else if(grepl(".feather", tolower(input$file$datapath))){

      data <-  feather::read_feather(input$file$datapath)
    }

    names(data) <- tolower(names(data))

    data
  })


  # DESCARGA: DE DATOS PÁGINA INE ----
  descarga =  eventReactive(input$base_ine, {

    # Modal para descarga
    show_modal_spinner() # show the modal window
    #
    #   #Descargar la base de datos en archivo temporal
    #  # temp <- tempfile()
    # #  datos <- download_data(input$base_web_ine)
    datos <- feather::read_feather(paste0("data/",input$base_web_ine,".feather"))
    #   # Modal para descarga
    remove_modal_spinner() # remove it when done

    datos
  })

  # SWITCH: DESCARGA DATOS WEB INE | COMPUTADOR LOCAL ----

  datos <- reactiveVal(NULL)

  observeEvent(input$file, {
    new <- data_input()
    datos(new)
  })

  observeEvent(input$base_ine, {
    datos(descarga())
  })

  ### EXTRACT: names variables input datos ####
  variables_int <- reactive({
    if (!is.null(input$file)) {
      names(data_input())
    } else if (!is.null(descarga())) {
      names(descarga())
    }
  })

  ### find var fact exp ####

  var_selec_fact <- reactive({names(datos())[grep(paste0("^",fact_exp,"$",collapse = "|"),names(datos()))]})

  ### find var conglom ####

  var_select_conglom  <- reactive({names(datos())[grep(paste0("^",conglomerados,"$",collapse = "|"),names(datos()))]})

  ### find var strat ####

  var_select_estrat <- reactive({names(datos())[grep(paste0("^",estratos,"$",collapse = "|"),names(datos()))]})

  observeEvent(datos(),{
    updateSelectInput(session, "varINTERES",
                      choices = variables_int(),
                      selected = "" )})

  observeEvent(any(is.null(datos()),input$tipoCALCULO == "Proporción"),{
    updateSelectInput(session, "varDENOM",
                      choices = variables_int(),
                      selected = "")
  })

  observeEvent(datos(),{
    updateSelectInput(session, "varCRUCE",
                      choices = c("",variables_int())
    )})

  ### update inputs ####

  observeEvent(datos(),{
    updateSelectInput(session, "varSUBPOB",
                      choices = c("",variables_int()),
                      selected = ""
    )})

  observeEvent(datos(),{
    updateSelectInput(session, "varFACT1",
                      choices = variables_int(),
                      selected = var_selec_fact()
    )})
  observeEvent(datos(),{
    updateSelectInput(session, "varCONGLOM",
                      choices = variables_int(),
                      selected = var_select_conglom()
    )})
  observeEvent(datos(),{
    updateSelectInput(session, "varESTRATOS",
                      choices = variables_int(),
                      selected = var_select_estrat()
    )})


  ### + R E N D E R - U I + ####

  # ### RENDER: IN SIDE BAR  ####
  output$etiqueta <- renderUI({
    req(input$varCRUCE >= 1)
    req(labelled::is.labelled(datos()[[input$varCRUCE[1]]]))
    checkboxInput("ETIQUETAS", "Sus datos poseen etiquetas, ¿Desea agregarlas?",value = F)

  })

  output$denominador <- renderUI({
    req(input$tipoCALCULO == "Proporción")
    selectInput("varDENOM", label = h5("Denominador - Opcional"),choices = variables_int(), selected = NULL, multiple = T)
    # selectInput("varINTERES", label = h5("Variable de interés"),choices = "",  multiple = F),

  })



  ### RENDER: IN MAIN PANEL -----
  ### Render título tabulado
  output$tituloTAB <- renderUI({
    req(!warning_resum(), input$varINTERES)
    req(input$actionTAB, tabuladoOK())

    tagList(
      div(class="titu-ine",
          h2("Resultado evaluación de calidad")),
      actionButton("show", "Definición de indicadores"),
      verbatimTextOutput("PRUEBAS2"),
      ### render gráfico de resumen
      div(style='width:100%;overflow-x: scroll;',
          div(plotOutput('grafico'),
              align = "center",
              style = "height:200px"),
          ### render tabulado
          tags$div(
            class="my_table", # set to custom class
            htmlOutput("tabulado") %>% withSpinner(color="#0dc5c1"))
      )
    )
  })



  #### + O U T P U T S + ####

  ### CREATE: tabulados  ----

  tabulado0 =  eventReactive(input$actionTAB,{

    # para generarse necesita que no hayan warnings
    req(!warning_resum(), input$varINTERES,input$varCONGLOM, input$varESTRATOS, input$varFACT1)

    tabulado = create_tabulado(base = datos(),
                               v_interes =  input$varINTERES,
                               denominador = input$varDENOM,
                               v_cruce = input$varCRUCE,
                               v_subpob =  input$varSUBPOB,
                               v_fexp1 = input$varFACT1,
                               v_conglom = input$varCONGLOM,
                               v_estratos = input$varESTRATOS,
                               tipoCALCULO = input$tipoCALCULO,
                               ci = input$IC,
                               scheme = input$SCHEME,
                               ajuste_ene = FALSE)

    #### opción de etiquetas #####

    if(input$ETIQUETAS != FALSE && !is.null(input$varCRUCE) && labelled::is.labelled(datos()[[input$varCRUCE[1]]])){ #

      paste_labels = function(tabla, base, var_cruce){

        dt = data.frame(valor = labelled::val_labels(base[[var_cruce]]))
        dt = tibble::rownames_to_column(dt)

        tabla[[var_cruce]] =  unlist(lapply(tabla[[var_cruce]] ,function(x) as.character(dt$rowname[dt$valor == x])))
        tabla
      }

      ####  al hacer filtros se eliminan categorias, necesitamos sacar etiquetas de base filtrada

      if(input$varSUBPOB != ""){
        datos2 = datos()[datos()[[input$varSUBPOB]] == 1,]
      }else{
        datos2 = datos()
      }
      for(i in input$varCRUCE){
        tabulado = paste_labels(tabla = tabulado, base = datos2, var_cruce = i)
      }
    }

    tabulado
  })

  tabuladoOK <- isolate({tabulado0})

  ### RENDER: Tabulado ####
  output$tabulado  <- renderText({
    tabla_html_shiny(tabuladoOK(),scheme = input$SCHEME) # %>% rename(es = se, cv = coef_var, media = contains("mean"))
  })

  ### RENDER: GRÁFICO DE BARRAS CON PORCENTAJE DE CELDAS POR CATEGORÍA ####

  output$grafico  <- renderPlot({
    create_plot(tabuladoOK(),scheme = input$SCHEME)
  }, height = 170, width = 800)

  ### MODAL definiciones ####

  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Definición de indicadores",

      HTML("<strong><h2>Insumos a evaluar:</h2></strong>      <br>
<h4><strong>ES:</strong> Error Estandar.        <br>
<strong>Coef_var:</strong> Coeficiente de Variación.       <br>
<strong>GL:</strong> Grados de Libertad.        <br>
<strong>n:</strong> Casos muestrales.     </h4>     <br>
<strong><h2>Resultados de la evaluación: </h2></strong>         <br>
<h4><strong>eval_n:</strong> Evaluación de casos muestrales        <br>
<strong>eval_gl:</strong> Evaluación de grados de libertad.        <br>
<strong>tipo_eval:</strong> Tipo de evaluación utilizada: Puede ser por Error Estandar o por Coeficiente de variación.        <br>
<strong>Cuadrática:</strong> Resultado de evaluación por función cuadrática.        <br>
<strong>eval_se:</strong> Resultado de la evaluación del Error Estandar.        <br>
<strong>eval_cv:</strong> Resultado de la evaluación del Coeficiente de variación.        <br>
<strong>calidad:</strong> Evaluación final de la celda, puede ser: <p style=\"font-style: italic;\"> Fiable, Poco Fiable o No fiable </p> </h4>      <br>"
      ), easyClose = T
    ))
  })




  # DESCARGA: DE TABULADO GENERADO ----

  # Habilitar botón de descarga
  observeEvent(tabuladoOK(),{
    req(!warning_resum(), input$varINTERES)
    req(input$actionTAB, tabuladoOK())

    enable("tabla")
  })

  # observeEvent(is.null(wrn_var_int()), {
  #   enable("actionTAB")
  # })
  #


  output$tabla <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(tabuladoOK(), file)
    }
  )


  ### Alertas warnings #####
  # input$varINTERES
  # input$varDENOM
  # input$varCRUCE
  # input$varSUBPOB
  # input$varFACT1
  # input$varCONGLOM
  # input$varESTRATOS
  # input$tipoCALCULO
  # input$IC
  # input$ajuste_en

  # "tipoCALCULO", "¿Qué tipo de cálculo deseas realizar?",
  # choices = list("Media","Proporción","Suma variable Continua","Conteo casos", "Mediana")

  #### warning alert var interes ####
  wrn_var_int <- reactive({

    if(show_wrn == F){

      even = FALSE

    }else{

      var <- input$varINTERES
      even <- FALSE

      if(var != "") {

        if(input$tipoCALCULO %in% c("Media","Mediana","Suma variable Continua")) {
          es_prop <- datos() %>%
            dplyr::mutate(es_prop_var = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0 | is.na(!!rlang::parse_expr(var)),1,0))

          even <- sum(es_prop$es_prop_var) == nrow(es_prop)
          shinyFeedback::feedbackWarning("varINTERES", even, "¡La variable no es continua!")

          even

        }else if(input$tipoCALCULO %in% c("Proporción","Conteo casos")){

          es_prop <- datos() %>%
            dplyr::mutate(es_prop_var = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0 | is.na(!!rlang::parse_expr(var)),1,0))

          even <- sum(es_prop$es_prop_var) != nrow(es_prop)
          shinyFeedback::feedbackWarning("varINTERES", even, "¡La variable no es de proporcion!")

          even
        }
      }else{even}
    }
  })

  ### warning alert var denom ####

  wrn_var_denom <- reactive({
    #  req(input$tipoCALCULO == "Proporción", input$varDENOM)
    if(show_wrn == F | is.null(input$varDENOM)){

      even = FALSE

    }else{

      var <- input$varDENOM
      even <- FALSE

      if(var != ""){

        es_prop <- datos() %>%
          dplyr::mutate(es_prop_var = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0 | is.na(!!rlang::parse_expr(var)),1,0))

        even <- sum(es_prop$es_prop_var) != nrow(es_prop)
        shinyFeedback::feedbackWarning("varDENOM", even, "¡La variable no es de proporcion!")

        even
      }else{even}
    }
  })


  #### warning alert var subpop ####
  wrn_var_subpop <- reactive({

    if(show_wrn == F){

      even = FALSE

    }else{

      subpop <- input$varSUBPOB
      even <- FALSE

      if(subpop != "") {

        es_prop_subpop <- datos() %>% dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop) == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

        ## Primero se inspecciona si tiene NAs
        if(sum(is.na(es_prop_subpop[[subpop]] > 0)) > 0){

          even <- sum(is.na(es_prop_subpop[[subpop]] > 0)) > 0

          shinyFeedback::feedbackWarning("varSUBPOB", even, "subpop contiene NAs!")

          even

          ### si no tiene NA se inspecciona si es continua
        }else if(sum(is.na(es_prop_subpop[[subpop]] > 0)) == 0){

          even <- sum(es_prop_subpop$es_prop_subpop) != nrow(es_prop_subpop)

          shinyFeedback::feedbackWarning("varSUBPOB",even,"subpop debe ser una variable dummy!")

          even

        }

        # sum(es_prop_subpop$es_prop_subpop) == nrow(es_prop_subpop)

      }else{even}
    }
  })

  #### warning alert var fact_exp ####
  wrn_var_factexp <- reactive({
    req(input$varFACT1)

    if(show_wrn == F){

      even = FALSE

    }else{

      var <- input$varFACT1
      even <- FALSE

      if(var != "") {

        even <- sum(is.na(datos()[[var]] > 0)) > 0

        shinyFeedback::feedbackWarning("varFACT1", even, "La variable contiene NAs!")

        even

        # sum(es_prop_subpop$es_prop_subpop) == nrow(es_prop_subpop)

      }else{even}
    }
  })

  #### warning alert var conglom ####
  wrn_var_conglom <- reactive({

    if(show_wrn == F){

      even = FALSE

    }else{

      var <- input$varCONGLOM
      even <- FALSE

      if(var != "") {

        even <- sum(is.na(datos()[[var]] > 0)) > 0

        shinyFeedback::feedbackWarning("varCONGLOM", even, "La variable contiene NAs!")

        even

        # sum(es_prop_subpop$es_prop_subpop) == nrow(es_prop_subpop)

      }else{even}
    }
  })

  #### warning alert var strata ####
  wrn_var_strata <- reactive({

    if(show_wrn == F){

      even = FALSE

    }else{

      var <- input$varESTRATOS
      even <- FALSE

      if(var != "") {

        even <- sum(is.na(datos()[[var]] > 0)) > 0

        shinyFeedback::feedbackWarning("varESTRATOS", even, "La variable contiene NAs!")

        even

        # sum(es_prop_subpop$es_prop_subpop) == nrow(es_prop_subpop)

      }else{even}
    }
  })

  #### resume warnings ###----

  warning_resum = reactive({
    any(wrn_var_int(),wrn_var_denom(),
        wrn_var_subpop(),wrn_var_factexp(),wrn_var_conglom(),wrn_var_strata())
  })

  #    output$wrn_var_subpop <- renderText(wrn_var_subpop())

  #### MODAL warnings cálculos ####
  observeEvent(input$actionTAB,{
    # tiene que haber warning para que se muestre
    req(warning_resum())
    shinyalert("¡Error!", "¡Considere las advertencias!", type = "error")
  })

  observeEvent(input$actionTAB,{
    # tiene que haber warning para que se muestre
    req(input$varINTERES == "")
    shinyalert("¡Error!", "¡Debe seleccionar una variable de interés!", type = "error")
  })

  observeEvent(input$actionTAB,{
    # tiene que haber warning para que se muestre
    req(input$varCONGLOM == "" || input$varESTRATOS == "" || input$varFACT1 == "")
    shinyalert("¡Error!", "¡Faltan variables del diseño complejo!", type = "error")
  })

  # r <- reactiveValues(NULL)

  # observe({
  #   req(warning_resum(), input$actionTAB)
  #   # Updates goButton's label and icon
  #
  #    input$actionTAB <- r
  #
  # })}

  ##### * Pruebas de outputs * ####

  output$tipoCalText <- renderPrint({
    req(debug == T)
    list(paste("Tipo de calculo:",input$tipoCALCULO),
         paste("acctionButton =",is.null(input$actionTAV)),
         paste("war_varINTERES =",wrn_var_int()),
         paste("war_varDENOM =",wrn_var_denom()),
         paste("varINTERES =",input$varINTERES),
         paste("warn_var_subpob =",wrn_var_subpop()),
         paste("warn_var_factEXP =",wrn_var_factexp()),
         paste("varDENOM =",input$varDENOM),
         paste("varSUBPOP =",input$varSUBPOB),
         paste("IC =",input$IC),
         paste("varFACTEXP =",input$varFACT1),
         paste("WARN_resume =",warning_resum()),
         paste("datos =",dim(datos()))
    )
  })

}
# Run the application
shinyApp(ui = ui, server = server)
