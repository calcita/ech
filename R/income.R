#' income_constant_prices
#'
#' @description This function allows you to calculate the household income constant prices
#' @param data data.frame with ECH microdata
#' @param base_month baseline month
#' @param base_year baseline year
#' @param mes month
#' @param ht11 Variable name of income. Default: ht11
#' @param ht13 Variable name of rental value. Default: ht13
#' @param ht19 Variable name of number of individuals in the household. Default: ht19
#' @param ipc  General ("G") or Regional ("R")
#'
#' @importFrom dplyr mutate left_join
#' @importFrom magrittr %<>% %>%
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
                                   ipc = "G",
                                   mes = "mes",
                                   ht11 = "ht11",
                                   ht13 = "ht13",
                                   ht19 = "ht19"){

  # checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(dplyr::between(base_month,1,12), msg =  glue::glue("Sorry... :( \n base_month is not between 1 and 12"))
  assertthat::assert_that(ipc  %in% c("G", "R"), msg =  glue::glue("Sorry... :( \n ipc is not G or R"))
  assertthat::assert_that(mes  %in% names(data), msg =  glue::glue("Sorry... :( \n {mes} is not in data"))
  assertthat::assert_that(ht11  %in% names(data), msg =  glue::glue("Sorry... :( \n {ht11} is not in data"))
  assertthat::assert_that(ht13  %in% names(data), msg =  glue::glue("Sorry... :( \n {ht13} is not in data"))
  assertthat::assert_that(ht19  %in% names(data), msg =  glue::glue("Sorry... :( \n {ht19} is not in data"))

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
    message("Variables have been created in the base: \n \t y_pc_d  (income per capita deflated);
         rv_d (rental value deflated);
         y_wrv_d (income without rental value deflated) &
         y_wrv_pc_d (income without rental value per capita deflated)")
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
    message("Variables have been created in the base: \n \t deflactor_r (Deflactor regional) &
            y_wrv_pc_d_r (income without rental value per capita deflated (regional))")
  }

 return(data)

  # message(glue::glue("Se ha creado la variable {colname} en la base"))
}

#' income_quantiles
#'
#' @description This function allows you to calculate the Household Income Quantiles
#'
#' @param data data.frame
#' @param quantile Variable name of quintil (5) or decil (10). Default: 5
#' @param weights Variable name of ponderation variable. Default: "pesoano"
#' @param income Variable name of income constant price. Default: "y_pc_d"
#' @importFrom statar xtile
#' @importFrom dplyr mutate pull
#' @importFrom magrittr %<>%
#' @export
#' @return data.frame
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
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
    data %<>% dplyr::mutate(quintil = statar::xtile(.data[[income]], n = 5, wt = weights))
    message("A variable has been created in the base: \n \t quintil (quintil de ingresos)")
  }  else {
    ## deciles
    data %<>% dplyr::mutate(decil = statar::xtile(.data[[income]], n = 10, wt = weights))
    message("A variable has been created in the base: \n \t decil (decil de ingresos)")
  }

  data
}



#' labor_income_per_capita
#'
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
#' @param numero Variable name of household id
#' @param f85 Variable name of hours worked per week
#' @param pobpcoac Variable name of definition of population by activity status
#' @param pt4 Variable name of total employment income
#' @param base_month baseline month
#' @param base_year baseline year
#' @param mes month
#'
#' @return data.frame
#'
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
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
