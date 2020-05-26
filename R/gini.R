#' gini
#'
#' @param data data frame with ECH microdata
#' @param weights ponderation variable
#' @param domain dominio de interes
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate select pull
#' @importFrom laeken gini
#' @export

gini <- function(data = ech::toy_ech_2018,
                 weights = "pesoano",
                 domain = NULL) {

  data %<>% filter(.data$nper == 1)
  # ipc montevideo

  data <- income_constant_prices(data = data, base.month = 1, base.year = 2005)
  data %<>% mutate(ypcsvl_deflate_gini = .data$ht11_svl_per_capita_deflate) %>%
    select(-.data$ht11_per_capita:-.data$ht11_per_capita_deflate) # ver como no sobreescribir

  # ipc interior

  if (is.null(domain)) {
    indice <- laeken::gini(inc = data$ypcsvl_deflate_gini, weights = pull(data[,weights]))
  } else {
    indice <- laeken::gini(data$ypcsvl_deflate_gini, weights = pull(data[,weights]), breakdown = pull(data[,domain]))
  }

  # ver como llenar los slots de indice...

  return(indice)
}
