#' This function allows you to calculate the household income constant prices
#' @family income
#' @param data data.frame with ECH microdata
#' @param base_month baseline month
#' @param base_year baseline year
#' @param mes month
#' @param ht11 Variable name of income. Default: ht11
#' @param ht13 Variable name of rental value. Default: ht13
#' @param ht19 Variable name of number of individuals in the household. Default: ht19
#' @param index IPC or IPAB
#' @param level  General ("G") or Regional ("R")
#'
#' @importFrom dplyr mutate left_join %>%
#' @importFrom haven zap_labels
#' @importFrom rlang .data
#' @export
#' @return data.frame
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' \donttest{
#' toy_ech_2018 <- income_constant_prices(data = ech::toy_ech_2018)
#' }

income_constant_prices <- function(data = ech::toy_ech_2018,
                                   base_month = 6,
                                   base_year = 2018,
                                   index = "IPC",
                                   level = "G",
                                   mes = "mes",
                                   ht11 = "ht11",
                                   ht13 = "ht13",
                                   ht19 = "ht19"){

  # checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(dplyr::between(as.numeric(base_month),1,12), msg =  glue::glue("Sorry... :( \n base_month is not between 1 and 12"))
  assertthat::assert_that(index  %in% c("IPC", "IPAB"), msg =  glue::glue("Sorry... :( \n index is not IPC or IPAB"))
  assertthat::assert_that(level  %in% c("G", "R"), msg =  glue::glue("Sorry... :( \n level is not G or R"))
  assertthat::assert_that(mes  %in% names(data), msg =  glue::glue("Sorry... :( \n {mes} is not in data"))
  assertthat::assert_that(ht11  %in% names(data), msg =  glue::glue("Sorry... :( \n {ht11} is not in data"))
  assertthat::assert_that(ht13  %in% names(data), msg =  glue::glue("Sorry... :( \n {ht13} is not in data"))
  assertthat::assert_that(ht19  %in% names(data), msg =  glue::glue("Sorry... :( \n {ht19} is not in data"))

  if(max(data$anio) %in% 2013:2015){
    data <- organize_ht11(data = data, year = max(data$anio))
  }

  if (level == "G") {
    if(index == "IPC"){
      deflator <- deflate(base_month = base_month,
                              base_year = base_year,
                              index = "IPC",
                              level = "G",
                              df_year = max(data$anio))
    } else{
      deflator <- deflate(base_month = base_month,
                              base_year = base_year,
                              index = "IPAB",
                              level = "G",
                              df_year = max(data$anio))
    }

    data <- data %>%
      dplyr::mutate(aux = as.integer(haven::zap_labels(.data[[mes]]))) %>%
      dplyr::left_join(deflator, by = c("aux" = "mes"), keep = F)

    data <- data %>%
      dplyr::mutate(y_pc = .data[[ht11]] / .data[[ht19]], # income per capita
                    y_pc_d = y_pc * deflator, # income per capita deflated
                    rv_d = .data[[ht13]] * deflator, # rental value deflated
                    y_wrv_d = (.data[[ht11]] - .data[[ht13]]) * deflator, # income without rental value deflated
                    y_wrv_pc_d = ((.data[[ht11]] - .data[[ht13]]) / .data[[ht19]]) * deflator) %>% # income without rental value per capita deflated
     dplyr::select(-aux, -deflator)
    message("Variables have been created: \n \t y_pc (income per capita current prices / ingreso per capita a precios corrientes);
    y_pc_d  (income per capita deflated / ingreso per capita deflactado);
         rv_d (rental value deflated / valor locativo deflactado);
         y_wrv_d (income without rental value deflated / ingreso sin valor locativo deflactado) &
         y_wrv_pc_d (income without rental value per capita deflated / ingreso sin valor locativo per capita deflactado)")
  }

  if (level == "R") {
    if(index == "IPC"){
      deflator_i <-  deflate(base_month = base_month, base_year = base_year, index = "IPC", level = "I", df_year = max(data$anio))
      deflator_m <-  deflate(base_month = base_month, base_year = base_year, index = "IPC", level = "M", df_year = max(data$anio))
    } else{
      deflator_i <-  deflate(base_month = base_month, base_year = base_year, index = "IPAB", level = "I", df_year = max(data$anio))
      deflator_m <-  deflate(base_month = base_month, base_year = base_year, index = "IPAB", level = "M", df_year = max(data$anio))
    }

    data <- data %>%
      dplyr::mutate(aux = as.integer(haven::zap_labels(data$mes))) %>%
      dplyr::left_join(deflator_i, by = c("aux" = "mes"), keep = F) %>%
      dplyr::rename(deflator_i = deflator) %>%
      dplyr::left_join(deflator_m, by = c("aux" = "mes"), keep = F) %>%
      dplyr::rename(deflator_m = deflator)

    data <- data %>%
      dplyr::mutate(deflator_r = ifelse(dpto == 1, deflator_m, deflator_i),
                    y_pc = .data[[ht11]] / .data[[ht19]], # income per capita
                    y_pc_d_r = y_pc * deflator_r, # income per capita deflated
                    rv_d_r = .data[[ht13]] * deflator_r, # rental value deflated
                    y_wrv_d_r = (.data[[ht11]] - .data[[ht13]]) * deflator_r, # income without rental value deflated
                    y_wrv_pc_d_r = ((.data[[ht11]] - .data[[ht13]]) / .data[[ht19]]) * deflator_r) %>% # income without rental value per capita deflated
      dplyr::select(-aux, -deflator_i, -deflator_m, -deflator_r)
    message("Variables have been created: \n \t y_pc (income per capita current prices / ingreso per capita a precios corrientes)
                y_pc_d_r (income per capita deflated / ingreso per capita deflactado);
                rv_d_r (rental value deflated / valor locativo deflactado);
                y_wrv_d_r (income without rental value deflated / ingreso sin valor locativo deflactado) &
                y_wrv_pc_d_r (income without rental value per capita deflated / ingreso sin valor locativo per capita deflactado)")
  }

 return(data)

}

#' This function allows you to calculate the Household Income Quantiles
#' @family income
#' @param data data.frame
#' @param quantile Variable name of quintil (5) or decil (10). Default: 5
#' @param weights Variable name of ponderation variable. Default: "pesoano"
#' @param income Variable name of income constant price. Default: "y_pc_d"
#' @importFrom statar xtile
#' @importFrom dplyr mutate pull
#' @export
#' @return data.frame
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
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
  assertthat::assert_that(income %in% names(data), msg = "Sorry... :( \n Income parameter is not calculated, please use income_constant_prices() to obtain the variable.")

  weights = pull(data[,weights])

  if (quantile == 5) {
    ## quintiles
    data <- data %>% dplyr::mutate(quintil = statar::xtile(.data[[income]], n = 5, wt = weights))
    message("A variable has been created: \n \t quintil (quintil de ingresos)")
  }  else {
    ## deciles
    data <- data %>% dplyr::mutate(decil = statar::xtile(.data[[income]], n = 10, wt = weights))
    message("A variable has been created: \n \t decil (decil de ingresos)")
  }

  return(data)
}



#' This function allows you to calculate the labor income per capita
#' @family income
#' @param data data frame
#' @param numero Variable name of household id
#' @param pobpcoac Variable name of definition of population by activity status
#' @param g126_1 Variable name of net salary
#' @param g126_2 Variable name of commissions, incentives, overtime payment, fringe benefits
#' @param g126_3 Variable name of non-surrendering expenses
#' @param g126_4 Variable name of tips
#' @param g126_5 Variable name of annual complementary salary
#' @param g126_6 Variable name of vacation pay
#' @param g126_7 Variable name of delayed payments
#' @param g126_8 Variable name of transportation tickets
#' @param g127_3 Variable name of received food or drink
#' @param g128_1 Variable name of received food tickets
#' @param g129_2 Variable name of received housing or accommodation
#' @param g130_1 Variable name of another type of compensation
#' @param g131_1 Variable name of received another type of supplement paid by the employer
#' @param g133_1 Variable name of the right to cultivate goods for own-consumption
#' @param g133_2 Variable name of the right to cultivate goods for own-consumption (amount received from the sale)
#' @param g134_1 Variable name of net salary
#' @param g134_2 Variable name of commissions, incentives, overtime payment, fringe benefits
#' @param g134_3 Variable name of non-surrendering expenses
#' @param g134_4 Variable name of tips
#' @param g134_5 Variable name of annual complementary salary
#' @param g134_6 Variable name of vacation pay
#' @param g134_7 Variable name of delayed payments
#' @param g134_8 Variable name of transportation tickets
#' @param g135_3 Variable name of received food or drink
#' @param g136_1 Variable name of received food tickets
#' @param g137_2 Variable name of received housing or accommodation
#' @param g138_1 Variable name of another type of compensation
#' @param g139_1 Variable name of received another type of supplement paid by the employer
#' @param g141_1 Variable name of the right to cultivate goods for own-consumption
#' @param g141_2 Variable name of the right to cultivate goods for own-consumption (amount received from the sale)
#' @param g142 Variable name of withdrawals for business household expenses you have or had
#' @param g144_1 Variable name of collected products for own consumption (non-agricultural worker)
#' @param g144_2_1 Variable name of collected products for own consumption (non-agricultural worker)
#' @param g144_2_3 Variable name of collected products for own consumption (non-agricultural worker)
#' @param g144_2_4 Variable name of collected products for own consumption (non-agricultural worker)
#' @param g144_2_5 Variable name of collected products for own consumption (non-agricultural worker)
#' @export
#' @return data.frame
#' @importFrom dplyr mutate case_when
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' \donttest{
#' toy_ech_2018 <- labor_income_per_capita(data = ech::toy_ech_2018)
#' }

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

  # checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(numero  %in% names(data), msg =  glue:glue("Sorry... :( \n {numero} is not in data"))
  assertthat::assert_that(pobpcoac  %in% names(data), msg =  glue:glue("Sorry... :( \n {pobpcoac} is not in data"))
  assertthat::assert_that(g126_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g126_1} is not in data"))
  assertthat::assert_that(g126_2  %in% names(data), msg =  glue:glue("Sorry... :( \n {g126_2} is not in data"))
  assertthat::assert_that(g126_3  %in% names(data), msg =  glue:glue("Sorry... :( \n {g126_3} is not in data"))
  assertthat::assert_that(g126_4  %in% names(data), msg =  glue:glue("Sorry... :( \n {g126_4} is not in data"))
  assertthat::assert_that(g126_5  %in% names(data), msg =  glue:glue("Sorry... :( \n {g126_5} is not in data"))
  assertthat::assert_that(g126_6  %in% names(data), msg =  glue:glue("Sorry... :( \n {g126_6} is not in data"))
  assertthat::assert_that(g126_7  %in% names(data), msg =  glue:glue("Sorry... :( \n {g126_7} is not in data"))
  assertthat::assert_that(g126_8  %in% names(data), msg =  glue:glue("Sorry... :( \n {g126_8} is not in data"))
  assertthat::assert_that(g127_3  %in% names(data), msg =  glue:glue("Sorry... :( \n {g127_3} is not in data"))
  assertthat::assert_that(g128_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g128_1} is not in data"))
  assertthat::assert_that(g129_2  %in% names(data), msg =  glue:glue("Sorry... :( \n {g129_2} is not in data"))
  assertthat::assert_that(g130_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g130_1} is not in data"))
  assertthat::assert_that(g131_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g131_1} is not in data"))
  assertthat::assert_that(g133_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g133_1} is not in data"))
  assertthat::assert_that(g134_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g134_1} is not in data"))
  assertthat::assert_that(g134_2  %in% names(data), msg =  glue:glue("Sorry... :( \n {g134_2} is not in data"))
  assertthat::assert_that(g134_3  %in% names(data), msg =  glue:glue("Sorry... :( \n {g134_3} is not in data"))
  assertthat::assert_that(g134_4  %in% names(data), msg =  glue:glue("Sorry... :( \n {g134_4} is not in data"))
  assertthat::assert_that(g134_5  %in% names(data), msg =  glue:glue("Sorry... :( \n {g134_5} is not in data"))
  assertthat::assert_that(g134_6  %in% names(data), msg =  glue:glue("Sorry... :( \n {g134_6} is not in data"))
  assertthat::assert_that(g134_7  %in% names(data), msg =  glue:glue("Sorry... :( \n {g134_7} is not in data"))
  assertthat::assert_that(g134_8  %in% names(data), msg =  glue:glue("Sorry... :( \n {g134_8} is not in data"))
  assertthat::assert_that(g135_3  %in% names(data), msg =  glue:glue("Sorry... :( \n {g135_3} is not in data"))
  assertthat::assert_that(g136_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g136_1} is not in data"))
  assertthat::assert_that(g137_2  %in% names(data), msg =  glue:glue("Sorry... :( \n {g137_2} is not in data"))
  assertthat::assert_that(g138_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g138_1} is not in data"))
  assertthat::assert_that(g139_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g139_1} is not in data"))
  assertthat::assert_that(g141_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g141_1} is not in data"))
  assertthat::assert_that(g141_2  %in% names(data), msg =  glue:glue("Sorry... :( \n {g141_2} is not in data"))
  assertthat::assert_that(g142  %in% names(data), msg =  glue:glue("Sorry... :( \n {g142} is not in data"))
  assertthat::assert_that(g144_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g144_1} is not in data"))
  assertthat::assert_that(g144_2_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {g144_2_1} is not in data"))
  assertthat::assert_that(g144_2_3  %in% names(data), msg =  glue:glue("Sorry... :( \n {g144_2_3} is not in data"))
  assertthat::assert_that(g144_2_4  %in% names(data), msg =  glue:glue("Sorry... :( \n {g144_2_4} is not in data"))
  assertthat::assert_that(g144_2_5  %in% names(data), msg =  glue:glue("Sorry... :( \n {g144_2_5} is not in data"))


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

  message("Variables have been created: \n \t labor_income (Ingresos laborales) &
            labor_income_h (Ingresos laborales del hogar) &
            labor_income_h_percapita (Ingresos laborales per capita)")
  return(data)
}


#' This function allows you to calculate the labor income per hour
#' @family income
#' @param data data frame
#' @param numero Variable name of household id
#' @param f85 Variable name of hours worked per week
#' @param pobpcoac Variable name of definition of population by activity status
#' @param pt4 Variable name of total employment income
#' @param base_month baseline month
#' @param base_year baseline year
#' @param mes month
#' @return data.frame
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#' @export
#' @examples
#' \donttest{
#' toy_ech_2018 <- ech::toy_ech_2018
#' toy_ech_2018 <- labor_income_per_hour(data = toy_ech_2018, base_month = "06", base_year = "2018")
#' }

labor_income_per_hour <- function(data = ech::toy_ech_2018,
                                  numero = "numero",
                                  f85 = "f85",
                                  pobpcoac = "pobpcoac",
                                  pt4 = "pt4",
                                  base_month = 6,
                                  base_year = 2018,
                                  mes = "mes"){

  # checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(dplyr::between(base_month,1,12), msg =  glue::glue("Sorry... :( \n base_month is not between 1 and 12"))
  assertthat::assert_that(mes  %in% names(data), msg =  glue::glue("Sorry... :( \n {mes} is not in data"))
  assertthat::assert_that(numero  %in% names(data), msg =  glue::glue("Sorry... :( \n {numero} is not in data"))
  assertthat::assert_that(pobpcoac  %in% names(data), msg =  glue::glue("Sorry... :( \n {pobpcoac} is not in data"))
  assertthat::assert_that(pt4  %in% names(data), msg =  glue::glue("Sorry... :( \n {pt4} is not in data"))
  assertthat::assert_that(f85  %in% names(data), msg =  glue::glue("Sorry... :( \n {f85} is not in data"))

  deflator_mdeo <- deflate(base_month = base_month, base_year = base_year, index = "IPC", level = "M", df_year = max(data$anio))
  names(deflator_mdeo)[1] <- "deflator_mdeo"

  deflator_int <- deflate(base_month = base_month, base_year = base_year, index = "IPC", level = "I", df_year = max(data$anio))
  names(deflator_int)[1] <- "deflator_int"

  data <- data %>% dplyr::mutate(aux = as.integer(haven::zap_labels(mes))) %>%
    dplyr::left_join(deflator_mdeo, by = c("aux" = "mes"), keep = F) %>%
    dplyr::left_join(deflator_int, by = c("aux" = "mes"), keep = F) %>%
    dplyr::mutate(deflator = dplyr::case_when(dpto == 1 ~ deflator_mdeo,
                                             TRUE ~ deflator_int)) %>%
    dplyr::select(-aux, -deflator_int, -deflator_mdeo)

  data <- data %>%
    dplyr::mutate(
      hours_per_month = f85 * 4.2,
      total_income_per_hour = ifelse(pobpcoac == 2 & pt4 != 0, (pt4 / deflator) * 100 / hours_per_month, NA))

  message("Variables have been created: \n \t hours_per_month (Cantidad de horas trabajadas al mes en ocupacion principal) &
            total_income_per_hour (Total de ingresos por trabajo por hora)")
  return(data)
}
