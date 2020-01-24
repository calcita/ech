#' gini
#'
#' @param data data frame with ECH microdata
#' @param weights ponderation variable
#' @param domain dominio de interes
#' @export
#'
#' @examples
#' gini(df, pesoano)
gini <- function(data = df,
                 weights = pesoano,
                 domain = NULL) {

  #data %<>% filter (nper == 1)
  # ipc montevideo

  # data <- income_constant_prices(data = data, base.month = 1, base.year = 2005)
  # data %<>% mutate(ypc_deflate_gini = ht11_svl_per_capita_deflate) %>%
  #   select(-ht11_per_capita:-ht11_per_capita_deflate) # ver como no sobreescribir

  # ipc interior

  # if (domain = NULL) {
  #   indice <- laeken::gini(inc = "ypcsvl_deflate_gini", weights = "pesoano", data = data)
  # } else {
  #   indice <- laeken::gini("ypcsvl_deflate_gini", weights = "pesoano", data = data, breakdown = domain)
  # }

}
