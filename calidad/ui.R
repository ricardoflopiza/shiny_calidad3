#addResourcePath(prefix = "www", directoryPath = "")
# UI ----
library(shiny)


shinyUI(shiny::div(shinyFeedback::useShinyFeedback(),
          shinyjs::useShinyjs(),
          # tags$head(
          #   tags$link(rel = "stylesheet", type = "text/css", href = "maqueta.css")
          # ),
          shiny::includeCSS("www/maqueta.css"),

          shiny::div(class="top-ine",
              shiny::fluidPage(
                shiny::div(class="container",
                    shiny::HTML('<div class="menu-ine">
                <a href="https://www.ine.cl" target="_blank" title="Ir al Instituto Nacional de Estadísticas">
                    <img class="logo-ine" src="ine_blanco.svg" alt="INE">
                </a>            </div>
            <div class="pull-right">
                   <h3 class="ti-top-ine">Evaluación de Calidad de Estimaciones en Encuestas de Hogares</h3>
            </div>'),
                )
              )
          ),
          shiny::div(class="conten-ine",

              ### fluid page de texto de descripción
              shiny::fluidPage(
                #   shiny::div(class="container-fluid",
                shiny::div(class="container",
                    shiny::HTML('<div class="row info-aplicativo">
                <div class="col-md-3">
                    <ul class="nav nav-tabs" role="tablist">
                        <li role="presentation" class="active"><a href="#home" aria-controls="home" role="tab" data-toggle="tab">¿Qué es y a quién aplica?</a></li>
                        <li role="presentation"><a href="#profile" aria-controls="profile" role="tab" data-toggle="tab">Uso del Aplicativo</a></li>
                    </ul>
                </div>
                <div class="col-md-9">
                    <div class="tab-content">
                        <div role="tabpanel" class="tab-pane active" id="home">
                            <h4 class="titu-ine">¿Qué es y a quién aplica?</h4>
                            <p>
                                Esta aplicación permite acercar a las personas usuarias la implementación del estándar de calidad para la evaluación
                                de estimaciones en encuestas de hogares del INE. A través de ella, las personas usuarias pueden conocer la precisión
                                que tienen las estimaciones generadas a partir de encuestas producidas por el INE u otras encuestas que utilicen
                                muestreo probabilístico estratificado y en 2 etapas. Con esto se busca poner a disposición de la comunidad una
                                herramienta interactiva para la cual no se requiere contar con conocimientos de programación, promoviendo el uso
                                adecuado de la información publicada. Esta aplicación permite evaluar la calidad de la estimación de medias, totales
                                y proporciones.<br><br>
                            </p>
                            <div class="alert alert-info" style="margin:0;" role="alert">
                                <small>
                                <img width="50" class="info" src="icon-info.svg" alt="icono-información">
                                Para un uso adecuado de esta herramienta se recomienda que la persona usuaria conozca las bases de datos que está
                                utilizando y sus libros de código, así como también se espera un conocimiento mínimo sobre diseños estadísticos complejos.
                                </small>
                            </div>
                        </div>
                        <div role="tabpanel" class="tab-pane" id="profile">
                            <h4 class="titu-ine">Uso del aplicativo</h4>
                            <div class="embed-responsive embed-responsive-16by9">
                                <iframe class="embed-responsive-item" src="https://www.youtube.com/embed/JWnnadpvEkA" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
                            </div>
                            <p>
                                <br>
                                Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla fringilla nec augue et imperdiet. Vivamus ac mi ut arcu
                                fermentum efficitur pharetra in est. Sed sit amet ultrices urna, vitae ultrices lorem.
                            </p>
                        </div>
                    </div>
                </div>
            </div>
        </div>')
                )
              )
          ),
          # Agregar el logo del INE

          shiny::div(class="dash-ine",
              shiny::fluidPage(
                shiny::div(class="container",
                    sidebarLayout(
                      ## Sidebar ####
                      sidebarPanel(width = 3,
                                   ## UI INPUT ####
                                   shinyWidgets::radioGroupButtons(
                                     inputId = "Id004",
                                     label = HTML("<h4>Definir base de datos de análisis</h4> <h5> En esta sección puedes escoger la opción de cargar una base de datos desde tu computador, o cargar una base de datos del INE</h5> "),
                                     choices = c("Bases INE","Base externa"),
                                     status = "primary",
                                     justified = TRUE
                                   ),
                                #   h5("En esta sección puedes escoger la opción de cargar una base de datos desde tu computador, o cargar una base de datos del INE"),
                                   uiOutput("datos_locales"),
                                   uiOutput("DescargaINE"),
                                   #### Edición datos
                                   #checkboxInput("data_edit", "¿Desea editar sus datos?",value = F),
                                  div(
                                  div(style="width:80%; display:inline-block; vertical-align: middle;",
                                      shinyWidgets::radioGroupButtons(
                                     inputId = "SCHEME",
                                     label = h5("Selecciona el esquema de evaluación, INE o CEPAL"),
                                     choices = c("chile", "cepal"),
                                     status = "primary",
                                     justified = TRUE
                                   )),
                                  div(style = "display:inline-block; vertical-align: bottom;",
                                 shinyBS::bsButton("info_estandar", "", icon = icon("question"), size = "extra-small"),
                                 shinyBS::bsPopover(id = "info_estandar",title= NULL,
                                            content = "¿Qué es un estándar de calidad?",
                                            placement = "right",
                                            trigger = "hover",
                                            options = list(container = "body")
                                  )
                                )
                              ),
                                   ## render selección de variables de interes, y de cruce
                                   # uiOutput("seleccion1"),
                                   #selectInput("varINTERES", label = h5("Variable de interés"),choices = "",  multiple = F),
                                   selectizeInput("varINTERES", label = h5("Variable de interés"),choices = "",  multiple = F,
                                                  options = list(placeholder = "Seleccione la variable")),
                                   #textOutput("wrn_var_int"),

                                   uiOutput("denominador"),

                                   radioButtons("tipoCALCULO", "¿Qué tipo de cálculo deseas realizar?",
                                                choices = list("Media","Proporción","Suma variable continua","Conteo casos"), inline = F ),
                                  selectizeInput("varCRUCE", label = h5("Desagregación"), choices = "", selected = NULL, multiple = T,
                                                 options = list(placeholder = "Seleccione la variable")),
                                   checkboxInput("IC", "¿Deseas agregar intervalos de confianza?",value = F),
                                   #checkboxInput("ajuste_ene", "¿Deseas agregar los ajuste del MM ENE?",value = F),
                                   uiOutput("etiqueta"),
                                   selectizeInput("varSUBPOB", label = h5("Subpoblación"), choices = "", selected = NULL, multiple = F,
                                                  options = list(placeholder = "Seleccione la variable")),
                                  selectizeInput("varFACT1", label = h5("Variable para factor de expansión"), choices = "",selected ="", multiple = F,
                                                 options = list(placeholder = "Seleccione la variable")),
                                   selectizeInput("varCONGLOM", label = h5("Variable para conglomerados"), choices = "", selected = "", multiple = F,
                                                  options = list(placeholder = "Seleccione la variable")),
                              selectizeInput("varESTRATOS",label = h5("Variable para estratos"), choices = "", selected = "", multiple = F,
                                             options = list(placeholder = "Seleccione la variable")),
                                   shinyjs::disabled(downloadButton("tabla", label = "Descargar tabulado")),
                                   actionButton("actionTAB", label = "Generar tabulado"),
                                   ## render selección variables DC
                                   uiOutput("seleccion2"),
                                   ## botón generación tabulado
                                   uiOutput("botonTAB")

                      ),
                      ## Main PANEL ----
                      mainPanel(width = 9,
                                #### render titulo tabulado
                                uiOutput("tituloTAB"),
                                uiOutput("edicion_datos")

                      )
                    )
                )
              )
          ),
          shiny::div(class="footer",
              shiny::fluidPage(
                shiny::div(class="container",
                    shiny::HTML('<div class="row">
                <div class="col-md-2">
                    <a href="https://www.ine.cl" target="_blank" title="Ir al Instituto Nacional de Estadísticas">
                        <img width="80%" style="padding:0 1rem;" class="cepal" src="logo_ine.svg">
                    </a>
                </div>
                <div class="col-md-2">
                    <a href="https://www.cepal.org/es" target="_blank" title="Ir a cepal">
                        <img width="79%" style="padding:0 2.7rem;" class="cepal" src="logotipo-cepal-blanco.svg">
                    </a>
                </div>
                <div class="col-md-3">
                    <h4>INE en redes sociales</h4>
                    <a href="https://www.facebook.com/ChileINE/" target="_blank"><img class="facebook" src="facebook.svg"></a>
                    <a href="https://twitter.com/ine_chile?lang=es" target="_blank"><img class="twitter" src="twitter.svg"></a>
                    <a href="https://www.youtube.com/user/inechile" target="_blank"><img class="youtube" src="youtube.svg"></a>
                    <a href="https://www.instagram.com/chile.ine/" target="_blank"><img class="instagram" src="instagram.svg"></a>
                    <br><br><br>
                    <h4>Consultas</h4>
                    <p><a href="https://www.portaltransparencia.cl/PortalPdT/ingreso-sai-v2?idOrg=1003" target="_blank">Solicitud de acceso a la información pública</a></p>
                    <p><a href="https://atencionciudadana.ine.cl/" target="_blank">Atención ciudadana</a></p>
                </div>
                <div class="col-md-5">
                    <h4>Contacto</h4>
                    <p>
                        Dirección nacional: Morandé N°801, piso 22, Santiago, Chile<br>
                        RUT: 60.703.000-6<br>
                        Código postal: 8340148<br>
                    </p>
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
          shiny::div(class="pie-ine",
              shiny::fluidPage(
                shiny::div(class="container",
                    shiny::HTML('
        <div class="text-right">
            Instituto Nacional de Estadísticas
       </div>')
                )
              )
          )
    )
)

