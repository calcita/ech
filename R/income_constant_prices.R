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
#'
#' @importFrom magrittr %<>%
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
  load("R/sysdata.rda")
  mes_base <- ipc_base2010 %>%
    filter(fecha == paste0(base.year, "-",base.month, "-01")) %>%
    select(indice) %>% as.numeric

  rows1 <- which(ipc_base2010$fecha == paste0(base.year-1, "-",12, "-01"))
  rows2 <- which(ipc_base2010$fecha == paste0(base.year, "-",11, "-01"))

  # Calcula el deflactor
  deflate <- ipc_base2010 %>%
    slice(rows1:rows2) %>%
    mutate(deflate = mes_base/as.numeric(indice),
           mes = 1:12
    ) %>%
    select(deflate, mes)

  # Asigna deflactor
  df <- df %>% mutate(aux = as.integer(haven::zap_labels(mes)))

  df <- left_join(df, deflate, by = c("aux" = "mes"))

  # Ingresos deflactados
  df %<>% mutate(ht11_per_capita = ht11 / ht19,
                 ht11_deflate = ht11 * deflate,
                 ht13_deflate = ht13 * deflate,
                 ht11_svl_def = YSVL * deflate,
                 ht11_svl_per_capita_deflate = YSVL / ht19 * deflate,
                 ht11_per_capita_deflate = ht11 / ht19 * deflate # Ingresos promedio per cápita a precios constantes de month.base year.base
  )

}
