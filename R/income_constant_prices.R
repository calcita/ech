#' income_constant_prices
#'
#' @description Household income constant prices
#' @param data data frame with ECH microdata
#' @param base.month mes base
#' @param base.year anio base
#' @param mes mes
#' @param ht11 ht11
#' @param ysvl ysvl
#' @param ht13 ht13
#' @param ht19 ht19
#' @importFrom dplyr mutate left_join
#' @importFrom magrittr %<>% %>%
#' @importFrom haven zap_labels
#' @importFrom rlang .data
#' @export
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- income_constant_prices(data = ech::toy_ech_2017_income)
#' }

income_constant_prices <- function(data = ech::toy_ech_2017_income,
                                   base.month = 6,
                                   base.year = 2017,
                                   mes = "mes",
                                   ht11 = "ht11",
                                   ysvl = "YSVL",
                                   ht13 = "ht13",
                                   ht19 = "ht19"){

  deflate <- deflate(base.month = base.month,
                     base.year = base.year)
  # Asigna deflactor
  data <- data %>% dplyr::mutate(aux = as.integer(haven::zap_labels(.data[[mes]]))) %>%
    dplyr::left_join(deflate, by = c("aux" = "mes"), keep = F)

  # Ingresos deflactados
  data %<>% dplyr::mutate(ht11_per_capita = .data[[ht11]] / .data[[ht19]],
                          ht11_deflate = .data[[ht11]] * deflate,
                          ht13_deflate = .data[[ht13]] * deflate,
                          ht11_svl_def = .data[[ysvl]] * deflate,
                          ht11_svl_per_capita_deflate = .data[[ysvl]] / .data[[ht19]] * deflate,
                          ht11_per_capita_deflate = .data[[ht11]] / .data[[ht19]] * deflate # Ingresos promedio per cápita a precios constantes de month.base year.base
  )
  # message(glue::glue("Se ha creado la variable {colname} en la base"))
}
