#' income_constant_prices
#'
#' @description This function allows you to calculate the household income constant prices
#' @param data data frame with ECH microdata
#' @param base_month baseline month
#' @param base_year baseline year
#' @param mes month
#' @param ht11 ht11 income
#' @param ht13 ht13 rental value
#' @param ht19 ht19 number of individuals in the household
#' @param ipc  General ("G") or Regional ("R")
#' @importFrom dplyr mutate left_join
#' @importFrom magrittr %<>% %>%
#' @importFrom haven zap_labels
#' @importFrom rlang .data
#' @export
#' @return data.frame
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- income_constant_prices(data = ech::toy_ech_2018)
#' }

income_constant_prices <- function(data = ech::toy_ech_2018,
                                   base_month = 6,
                                   base_year = 2018,
                                   ipc = "G",
                                   mes = "mes",
                                   ht11 = "ht11",
                                   ht13 = "ht13",
                                   ht19 = "ht19"){

  if (ipc == "G") {

    deflate <- ech::deflate(base_month = base_month,
                            base_year = base_year,
                            df_year = max(data$anio))

    data <- data %>% dplyr::mutate(aux = as.integer(haven::zap_labels(.data[[mes]]))) %>%
      dplyr::left_join(deflate, by = c("aux" = "mes"), keep = F)

    data %<>% dplyr::mutate(y_pc_d = .data[[ht11]] / .data[[ht19]] * deflate, # income per capita deflated
                            rv_d = .data[[ht13]] * deflate, # rental value deflated
                            y_wrv_d = (.data[[ht11]] - .data[[ht13]]) * deflate, # income without rental value deflated
                            y_wrv_pc_d = (.data[[ht11]] - .data[[ht13]]) / .data[[ht19]] * deflate # income without rental value per capita deflated
    )
  }

  if (ipc == "R") {

    deflactor_i <-  deflate(base_month = base_month, base_year = base_year, ipc = "I", df_year = max(data$anio))
    deflactor_m <-  deflate(base_month = base_month, base_year = base_year, ipc = "M", df_year = max(data$anio))

    data <- data %>%
      dplyr::mutate(aux = as.integer(haven::zap_labels(data$mes))) %>%
      dplyr::left_join(deflactor_i, by = c("aux" = "mes"), keep = F) %>%
      dplyr::rename(deflactor_i = deflate) %>%
      dplyr::left_join(deflactor_m, by = c("aux" = "mes"), keep = F) %>%
      dplyr::rename(deflactor_m = deflate)

    data <- data %>%  dplyr::mutate(deflactor_r = ifelse(dpto == 1, deflactor_m, deflactor_i),
                                    y_wrv_pc_d_r = (ht11 - ht13) / ht19 * deflactor_r)  # income without rental value per capita deflated (regional)

  }

 return(data)

  # message(glue::glue("Se ha creado la variable {colname} en la base"))
}

#' income_quantiles
#'
#' @description This function allows you to calculate the Household Income Quantiles
#'
#' @param data data.frame
#' @param quantile quintil (5) or decil (10)
#' @param weights ponderation variable
#' @param income name of the income constant price variable. Default: "y_pc_d"
#' @importFrom statar xtile
#' @importFrom dplyr mutate pull
#' @importFrom magrittr %<>%
#' @export
#' @return data.frame
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- income_constant_prices(data = ech::toy_ech_2018)
#' toy_ech_2018 <- income_quantiles(data = toy_ech_2018)
#' }

income_quantiles <- function(data = ech::toy_ech_2018,
                             quantile = 5,
                             weights = "pesoano",
                             income = "y_pc_d") {

  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(weights %in% names(data))
  assertthat::assert_that(quantile %in% c(5, 10))
  assertthat::assert_that(income  %in% names(data), msg = "Sorry... :( \n Income parameter is not calculated, please use income_constant_prices() to obtain the variable.")

  weights = pull(data[,weights])

  if (quantile == 5) {
    ## quintiles
    data %<>% dplyr::mutate(quintil = statar::xtile(.data[[income]], n = 5, wt = weights))
  }  else {
    ## deciles
    data %<>% dplyr::mutate(decil = statar::xtile(.data[[income]], n = 10, wt = weights))
  }
  # message(glue::glue("Se ha creado la variable {colname} en la base"))
}



#' labor_income_per_capita
#'
#' @param data data frame
#' @param numero household id
#' @param pobpcoac definition of population by activity status
#' @param g126_1 sueldo o jornales liquidos
#' @param g126_2 comisiones, incentivos, horas extras, habilitaciones
#' @param g126_3 viaticos no sujetos a rendicion
#' @param g126_4 propinas
#' @param g126_5 aguinaldo
#' @param g126_6 salario vacacional
#' @param g126_7 pagos atrasados
#' @param g126_8 boletos de transporte
#' @param g127_3 recibio alimentos o bebidas
#' @param g128_1 recibio tickets alimentacion
#' @param g129_2 recibio vivienda o alojamiento
#' @param g130_1 recibio otro tipo de retribucion
#' @param g131_1 recibio otro tipo de complemento pagado por el/la empleador/a
#' @param g133_1 derecho a cultivo para consumo propio
#' @param g133_2 derecho a cultivo para consumo propio (monto percibido por la venta)
#' @param g134_1 sueldo o jornales liquidos
#' @param g134_2 comisiones, incentivos, horas extras, habilitaciones
#' @param g134_3 viaticos no sujetos a rendicion
#' @param g134_4 propinas
#' @param g134_5 aguinaldo
#' @param g134_6 salario vacacional
#' @param g134_7 pagos atrasados
#' @param g134_8 boletos de transporte
#' @param g135_3 recibio alimentos o bebidas
#' @param g136_1 recibio tickets alimentacion
#' @param g137_2 recibio vivienda o alojamiento
#' @param g138_1 recibio otro tipo de retribucion
#' @param g139_1 recibio otro tipo de complemento pagado por el/la empleador/a
#' @param g141_1 derecho a cultivo para consumo propio
#' @param g141_2 derecho a cultivo para consumo propio (monto percibido por la venta)
#' @param g142 retiros para gastos del hogar de negocios que tiene o tenia
#' @param g144_1 retiro de productos para consumo propio (trabajador/a no agropecuario/a)
#' @param g144_2_1 retiro de productos para consumo propio (trabajador/a no agropecuario/a)
#' @param g144_2_3 retiro de productos para consumo propio (trabajador/a no agropecuario/a)
#' @param g144_2_4 retiro de productos para consumo propio (trabajador/a no agropecuario/a)
#' @param g144_2_5 retiro de productos para consumo propio (trabajador/a no agropecuario/a)
#' @export
#' @return data.frame
#' @importFrom dplyr mutate case_when
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- labor_income_per_capita(data = ech::toy_ech_2018)
#' }
#'
labor_income_per_capita <- function(data = ech::toy_ech_2018,
                                    numero = "numero",
                                    pobpcoac = "pobpcoac",
                                    g126_1 = "g126_1",
                                    g126_2 = "g126_2",
                                    g126_3 = "g126_3",
                                    g126_4 = "g126_4",
                                    g126_5 = "g126_5",
                                    g126_6 = "g126_6",
                                    g126_7 = "g126_7",
                                    g126_8 = "g126_8",
                                    g127_3 = "g127_3",
                                    g128_1 = "g128_1",
                                    g129_2 = "g129_2",
                                    g130_1 = "g130_1",
                                    g131_1 = "g131_1",
                                    g133_1 = "g133_1",
                                    g133_2 = "g133_2",
                                    g134_1 = "g134_1",
                                    g134_2 = "g134_2",
                                    g134_3 = "g134_3",
                                    g134_4 = "g134_4",
                                    g134_5 = "g134_5",
                                    g134_6 = "g134_6",
                                    g134_7 = "g134_7",
                                    g134_8 = "g134_8",
                                    g135_3 = "g135_3",
                                    g136_1 = "g136_1",
                                    g137_2 = "g137_2",
                                    g138_1 = "g138_1",
                                    g139_1 = "g139_1",
                                    g141_1 = "g141_1",
                                    g141_2 = "g141_2",
                                    g142 = "g142",
                                    g144_1 = "g144_1",
                                    g144_2_1 = "g144_2_1",
                                    g144_2_3 = "g144_2_3",
                                    g144_2_4 = "g144_2_4",
                                    g144_2_5 = "g144_2_5"){

  data <- data %>%
    dplyr::mutate(
      main_work = ifelse(pobpcoac %in% 2:5, g126_1 + g126_2 + g126_3 + g126_4 + g126_5 + g126_6 + g126_7 + g126_8 + g127_3 + g128_1 + g129_2 + g130_1 + g131_1 + g133_1 + g133_2/12, NA),
      second_work = ifelse(pobpcoac %in% 2:5, g134_1 + g134_2 + g134_3 + g134_4 + g134_5 + g134_6 + g134_7 + g134_8 + g135_3 + g136_1 + g137_2 + g138_1 + g139_1 + g141_1 + g141_2/12, NA),
      self_employment = ifelse(pobpcoac %in% 2:5, g142 + g144_1 + g144_2_1 + g144_2_3 + g144_2_4 + g144_2_5, 4),
      labor_income = main_work + second_work + self_employment
     ) %>%
    dplyr::group_by(numero) %>%
    dplyr::mutate(labor_income_h = sum(labor_income, na.rm = TRUE),
            labor_income_h_percapita = labor_income_h /sum(!is.na(labor_income_h))) %>%
   dplyr::ungroup()

}


#' labor_income_per_hour
#'
#' @param data data frame
#' @param numero household id
#' @param f85 hours worked per week
#' @param pobpcoac definition of population by activity status
#' @param pt4 total employment income
#' @param base_month baseline month
#' @param base_year baseline year
#' @param mes month
#'
#' @return data.frame
#'
#' @details Disclaimer: El script no es un producto oficial de INE.
#' @export
#'
#' @examples
#' \donttest{
#' df <- ech::toy_ech_2018
#' toy_ech_2018 <- labor_income_per_hour(data = df, base_month = "06", base_year = "2018")
#' }
#'
labor_income_per_hour <- function(data = ech::toy_ech_2018,
                                  numero = "numero",
                                  f85 = "f85",
                                  pobpcoac = "pobpcoac",
                                  pt4 = "pt4",
                                  base_month = NULL,
                                  base_year = NULL,
                                  mes = "mes"){

  deflate_mdeo <- ech::deflate(base_month = base_month, base_year = base_year, ipc = "M", df_year = max(data$anio))
  names(deflate_mdeo)[1] <- "deflate_mdeo"

  deflate_int <- ech::deflate(base_month = base_month, base_year = base_year, ipc = "I", df_year = max(data$anio))
  names(deflate_int)[1] <- "deflate_int"

  data <- data %>% dplyr::mutate(aux = as.integer(haven::zap_labels(mes))) %>%
    dplyr::left_join(deflate_mdeo, by = c("aux" = "mes"), keep = F) %>%
    dplyr::left_join(deflate_int, by = c("aux" = "mes"), keep = F) %>%
    dplyr::mutate(deflate = dplyr::case_when(dpto == 1 ~ deflate_mdeo,
                                             TRUE ~ deflate_int))

  data <- data %>%
    dplyr::mutate(
      hours_per_month = f85 * 4.2, # Cantidad de horas trabajadas en un mes en ocupacion principal
      total_income_per_hour = ifelse(pobpcoac == 2 & pt4 != 0, (pt4/deflate)*100/hours_per_month, NA)) # Total de ingresos por trabajo por hora

}
