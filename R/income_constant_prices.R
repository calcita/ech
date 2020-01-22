#' income_constant_prices
#'
#' @description Household income constant prices
#' @param data
#' @param base.month
#' @param base.year
#' @param mes
#' @param ht11
#' @param ysvl
#' @param ht13
#' @param ht19
#' @import dplyr
#' @importFrom magrittr %<>% %>%
#' @return
#' @export
#'
#' @examples
income_constant_prices <- function(data = df,
                                   base.month = 6,
                                   base.year = 2017,
                                   mes = mes,
                                   ht11 = ht11,
                                   ysvl = ysvl,
                                   ht13 = ht13,
                                   ht19 = ht19){

  #load("data/toy_ech_2017_income.rda")
  deflate <- deflate(base.month = base.month,
                     base.year = base.year)
  # Asigna deflactor
  df <- df %>% dplyr::mutate(aux = as.integer(haven::zap_labels(mes)))

  df <- dplyr::left_join(df, deflate, by = c("aux" = "mes"))

  # Ingresos deflactados
  df %<>% dplyr::mutate(ht11_per_capita = ht11 / ht19,
                 ht11_deflate = ht11 * deflate,
                 ht13_deflate = ht13 * deflate,
                 ht11_svl_def = YSVL * deflate,
                 ht11_svl_per_capita_deflate = YSVL / ht19 * deflate,
                 ht11_per_capita_deflate = ht11 / ht19 * deflate # Ingresos promedio per cápita a precios constantes de month.base year.base
  )

}
