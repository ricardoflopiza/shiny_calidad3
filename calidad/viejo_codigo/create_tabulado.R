
create_tabulado = function(base, v_interes, v_cruce,  v_subpob, v_fexp1, v_conglom,  v_estratos, tipoCALCULO, ci, ajuste_ene,denominador,scheme ,server = T){
  #create_Tabulado = function(base){
#  print(ci)
# base = datos()
# v_interes =  input$varINTERES
# v_cruce = input$varCRUCE
# v_subpob =  input$varSUBPOB
# v_fexp1 = input$varFACT1
# v_conglom = input$varCONGLOM
# v_estratos = input$varESTRATOS
# tipoCALCULO = input$tipoCALCULO


  if(length(v_cruce)>1){
    v_cruce1 = paste0(v_cruce, collapse  = "+")
  }else{
    v_cruce1 = v_cruce
  }

  base[[v_interes]] = as.numeric(base[[v_interes]])
  base$unit =  as.numeric(base[[v_conglom]])
  base$varstrat =  as.numeric(base[[v_estratos]])
  base$fe =  as.numeric(base[[v_fexp1]])


  ### Diseño complejo
  dc <- svydesign(ids = ~unit, strata = ~varstrat,
                  data =  base, weights = ~fe)

  options(survey.lonely.psu = "certainty")


  ### listas de funciones CALIDAD
  log_cv = F
  if(scheme == "chile"){

    ess = F
    deff = F
    unweighted = F

  funciones_cal = list(calidad::create_mean, calidad::create_prop,
                       calidad::create_size, calidad::create_tot, calidad::create_median)

  funciones_eval = list(calidad::evaluate_mean, calidad::evaluate_prop,
                        calidad::evaluate_size, calidad::evaluate_tot, calidad::evaluate_median)

  }else if(scheme == "cepal"){

  ess = T
  deff = T
  unweighted = T

  funciones_cal = list(calidad::create_mean, calidad::create_prop,
                       calidad::create_tot_con, calidad::create_tot, calidad::create_median)

  funciones_eval = list(calidad::evaluate_mean, calidad::evaluate_prop,
                        calidad::evaluate_tot_con, calidad::evaluate_tot, calidad::evaluate_median)

  }


  if(tipoCALCULO %in% "Media") {
    num = 1
  }else if(tipoCALCULO %in% "Proporción"){
    num = 2

    if(scheme == "cepal"){
      log_cv = T
    }

  }else if(tipoCALCULO %in% "Suma variable Continua"){
    num = 3
  }else if(tipoCALCULO %in% "Conteo casos"){
    num = 4
  }else if(tipoCALCULO %in% "Mediana"){
    num = 5
  }

#print(log_cv)
if(log_cv == T){

  ### normal sin denominador ###
  if(is.null(denominador)){

    if(is.null(v_cruce) && v_subpob == "") {
      insumos = funciones_cal[[num]](var = v_interes, disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

    } else if (v_subpob == ""){
      insumos = funciones_cal[[num]](var = v_interes,dominios = v_cruce1 ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

    } else if (is.null(v_cruce)){
      base[[v_subpob]] = as.numeric(base[[v_subpob]])
      insumos = funciones_cal[[num]](var = v_interes,subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

    } else {
      base[[v_subpob]] = as.numeric(base[[v_subpob]])
      insumos = funciones_cal[[num]](var = v_interes,dominios = v_cruce1 ,subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
    }
  }


  if(!is.null(denominador)){
    if(is.null(v_cruce) && v_subpob == "") {
      insumos = funciones_cal[[num]](var = v_interes, denominador = denominador, disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

    } else if (v_subpob == ""){
      insumos = funciones_cal[[num]](var = v_interes,denominador = denominador, dominios = v_cruce1 ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

    } else if (is.null(v_cruce)){
      base[[v_subpob]] = as.numeric(base[[v_subpob]])
      insumos = funciones_cal[[num]](var = v_interes ,denominador = denominador, subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

    } else {
      base[[v_subpob]] = as.numeric(base[[v_subpob]])
      insumos = funciones_cal[[num]](var = v_interes, dominios = v_cruce1, denominador = denominador, subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
    }
  }

  if(scheme == "cepal"){
    evaluados <- evaluados %>%
      mutate(tag = dplyr::case_when(tag == "publish" ~ "Publicar",
                                    tag == "review" ~ "Revisar",
                                    tag == "supress" ~ "Suprimir")) %>%
      rename(calidad = tag)
  }

}else{

  ### normal sin denominador ###
  if(is.null(denominador)){

  if(is.null(v_cruce) && v_subpob == "") {
    insumos = funciones_cal[[num]](var = v_interes, disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

  } else if (v_subpob == ""){
    insumos = funciones_cal[[num]](var = v_interes,dominios = v_cruce1 ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

  } else if (is.null(v_cruce)){
    base[[v_subpob]] = as.numeric(base[[v_subpob]])
    insumos = funciones_cal[[num]](var = v_interes,subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

  } else {
    base[[v_subpob]] = as.numeric(base[[v_subpob]])
    insumos = funciones_cal[[num]](var = v_interes,dominios = v_cruce1 ,subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
  }
  }


  if(!is.null(denominador)){
    if(is.null(v_cruce) && v_subpob == "") {
      insumos = funciones_cal[[num]](var = v_interes, denominador = denominador, disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

    } else if (v_subpob == ""){
      insumos = funciones_cal[[num]](var = v_interes,denominador = denominador, dominios = v_cruce1 ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

    } else if (is.null(v_cruce)){
      base[[v_subpob]] = as.numeric(base[[v_subpob]])
      insumos = funciones_cal[[num]](var = v_interes ,denominador = denominador, subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)

    } else {
      base[[v_subpob]] = as.numeric(base[[v_subpob]])
      insumos = funciones_cal[[num]](var = v_interes, dominios = v_cruce1, denominador = denominador, subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
    }
  }

  if(scheme == "cepal"){
  evaluados <- evaluados %>%
    mutate(tag = dplyr::case_when(tag == "publish" ~ "Publicar",
                                  tag == "review" ~ "Revisar",
                                  tag == "supress" ~ "Suprimir")) %>%
    rename(calidad = tag)
  }

  evaluados
}


}

### crear tabulado html

tabla_html_shiny <- function(tabla, scheme) {

  # Esto se hace en el caso de que la etiqueta diga calidad
  if (scheme == "chile") {
    tabla %>%
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
        gl = kableExtra::cell_spec(.data$gl, color = dplyr::case_when(
          .data$gl < 9  ~ "red",
          .data$gl >= 9 ~ "black"
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
    tabla %>%
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
        gl = kableExtra::cell_spec(.data$gl, color = dplyr::case_when(
          .data$gl < 9  ~ "red",
          .data$gl >= 9 ~ "black"
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

}
