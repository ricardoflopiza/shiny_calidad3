
#list.files("calidad/data/shiny")

#archivos_ine <- list.files("calidad/data/")
archivos_ine <- list.files("data/shiny/",pattern = ".rda") #c("enusc_2018.feather","enusc_2019.feather","enusc_2020.feather","epf_hogares.feather","epf_personas_ing.feather","esi_2020.feather")

nom_arch_ine <- archivos_ine %>% stringr::str_remove_all(".rda")

# posibilidad nombres de variables DC"
fact_exp = c("Fact_pers","Fact_Pers","fe","fact_cal", "fact_pers","fact_pers","fe","fact_cal", "fact_cal_esi","FACT_PERS","FACT_PERS","FE","FACT_CAL","FACT_CAL_ESI","wgt1","EXP","exp","Exp","expr","EXPR")
conglomerados = c("Conglomerado", "id_directorio","varunit", "conglomerado","id_directorio","varunit","CONGLOMERADO","ID_DIRECTORIO","VARUNIT","VarUnit")
estratos = c("VarStrat", "estrato", "varstrat","varstrat", "estrato",  "varstrat","VARSTRAT", "ESTRATO",  "VARSTRAT","VarStrat")

### load data from user #####

load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}

### carga de datos locales ####

carga_datos_locales <- function(path){

if(grepl(".sav", path)){

  data <- haven::read_sav(path)

} else if(grepl(".rds", tolower(path))){

  data <-  readRDS(path)
} else if(grepl(".dta", tolower(path))){

  data <- haven::read_dta(path)
} else if(grepl(".sas", tolower(path))){

  data <- haven::read_sas(path)
}  else if(grepl(".xlsx", tolower(path))){

  data <- readxl::read_excel(path)
} else if(grepl(".csv", tolower(path))){

#  data <-  read_delim(path, delim = ',')
  data <-  readr::read_csv(path)
}else if(grepl(".feather", tolower(path))){

  data <-  feather::read_feather(path)
}

names(data) <- tolower(names(data))

data

}

### crear tabulado html ####

tabla_html_shiny <- function(tabla,input) {

  # Esto se hace en el caso de que la etiqueta diga calidad
  if (input$SCHEME == "chile") {
    d <- tabla %>%
      dplyr::mutate_if(is.numeric, ~round(.x, 2)) %>%
      dplyr::mutate(
        calidad = kableExtra::cell_spec(.data$calidad, background  = dplyr::case_when(
          .data$calidad == "fiable" ~ "green",
          .data$calidad == "poco fiable" ~ "yellow",
          .data$calidad == "no fiable" ~ "red"
        ),
        color = "black")) %>%
      dplyr::mutate(
        n = kableExtra::cell_spec(.data$n, color= dplyr::case_when(
          .data$n < 60  ~ "red",
          .data$n >= 60 ~ "black"
        )),
        df = kableExtra::cell_spec(.data$df, color = dplyr::case_when(
          .data$df < 9  ~ "red",
          .data$df >= 9 ~ "black"
        ))) %>%
      kableExtra::kable(format.args = list(decimal.mark = ',', big.mark = "."),
                        format = "html",
                        escape = FALSE,
                        align = "c",
                        table.attr = "style = \"color: black;\"")  %>%
      kableExtra::kable_styling("striped",
                                full_width = FALSE,
                                html_font = "arial") %>%
      kableExtra::kable_paper("hover") %>%
      kableExtra::row_spec(0, bold = TRUE, color = "black")

  } else {
  d <- tabla %>%
      dplyr::mutate_if(is.numeric, ~round(.x, 2)) %>%
      dplyr::mutate(
        calidad = kableExtra::cell_spec(.data$calidad, background  = dplyr::case_when(
          .data$calidad == "Publicar" ~ "green",
          .data$calidad == "Revisar" ~ "yellow",
          .data$calidad == "Suprimir" ~ "red"
        ),
        color = "black")) %>%
      dplyr::mutate(
        n = kableExtra::cell_spec(.data$n, color= dplyr::case_when(
          .data$n < 60  ~ "red",
          .data$n >= 60 ~ "black"
        )),
        df = kableExtra::cell_spec(.data$df, color = dplyr::case_when(
          .data$df < 9  ~ "red",
          .data$df >= 9 ~ "black"
        ))) %>%
      kableExtra::kable(format.args = list(decimal.mark = ',', big.mark = "."),
                        format = "html",
                        escape = FALSE,
                        align = "c",
                        table.attr = "style = \"color: black;\"")  %>%
      kableExtra::kable_styling("striped",
                                full_width = FALSE,
                                html_font = "arial") %>%
      kableExtra::kable_paper("hover") %>%
      kableExtra::row_spec(0, bold = TRUE, color = "black")

  }

#  wt$close()

  return(d)
}

### check if var is dummy or not ####
need_warning <-function(tipo_calc,datos,var){

  ## media. suma var continua, deben ser continuas
  if(tipo_calc %in% c("Media","Suma variable Continua")) {

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



dumming_by_labels <- function(dc,var,domains = NULL ,exclud_lab = NULL){

  data <- dc$variables
  #plan0 = as.data.frame(matrix(NA,0,7))
  #names(plan0) <-c("N°","Cuadro","Desagregación","Nombre.variable","Factor.de.expansión","filtro","detalle")

  etiquetas <- unique(data[[var]])[!unique(data[[var]]) %in% exclud_lab]

  nombres_vars <-map_chr(etiquetas, ~ paste0(var,"_",.x,"_DUM"))

  dc$variables<- bind_cols(data,map(etiquetas, function(x){

    nomvar <- paste0(var,"_",x,"_DUM")

    data %>%
      mutate(!!nomvar := ifelse(!!rlang::parse_expr(var) == x,1,0)) %>%
      select(all_of(nomvar))

    #ifelse(data[[var]] == x,1,0)
    # print(nomvar)
  }
  ))

  map2_df(nombres_vars,etiquetas,~ evaluate(create_size(var = .x,domains = domains,design = dc)) %>%
            mutate(!!var := .y) %>% select(!!var,everything())
  )
}

is_categoric <- function(data,var){
  length(unique(data[[var]]))
}
