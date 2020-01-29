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
#' @importFrom dplyr mutate left_join enquo
#' @importFrom magrittr %<>% %>%
#' @importFrom haven zap_labels
#' @importFrom rlang sym quo_name
#' @export
#' @examples
#' \donttest{
#' income_constant_prices(data = ech::toy_ech_2017_income)
#' }
income_constant_prices <- function(data = ech::toy_ech_2017_income,
                                   base.month = 6,
                                   base.year = 2017,
                                   mes = "mes",
                                   ht11 = "ht11",
                                   ysvl = "YSVL",
                                   ht13 = "ht13",
                                   ht19 = "ht19"){
  mes = quo_name(enquo(mes))
  ht11 = quo_name(enquo(ht11))
  ysvl = quo_name(enquo(ysvl))
  ht13 = quo_name(enquo(ht13))
  ht19 = quo_name(enquo(ht19))
  #load("data/toy_ech_2017_income.rda")
  deflate <- deflate(base.month = base.month,
                     base.year = base.year)
  # Asigna deflactor
  data <- data %>% dplyr::mutate(aux = as.integer(haven::zap_labels(!!sym(mes))))

  data <- dplyr::left_join(data, deflate, by = c("aux" = "mes"))

  # Ingresos deflactados
  data %<>% dplyr::mutate(ht11_per_capita = !!sym(ht11) / !!sym(ht19),
                 ht11_deflate = !!sym(ht11) * deflate,
                 ht13_deflate = !!sym(ht13) * deflate,
                 ht11_svl_def = !!sym(ysvl) * deflate,
                 ht11_svl_per_capita_deflate = !!sym(ysvl) / !!sym(ht19) * deflate,
                 ht11_per_capita_deflate = !!sym(ht11) / !!sym(ht19) * deflate # Ingresos promedio per cápita a precios constantes de month.base year.base
  )

}
