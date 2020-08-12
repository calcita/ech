#' set_design
#' This function allows you to set the survey desing
#'
#' @param data data frame with ECH microdata
#' @param level is household ("h") or individual ("i")
#' @param ids variables specifying the unit primary sampling (it's not a public variable)
#' @param numero variables specifying  the householder ids
#' @param estred13 variable specifying strata
#' @param pesoano variable specifying weights
#'
#' @importFrom glue glue
#' @importFrom srvyr as_survey_design
#' @importFrom magrittr %>%
#' @keywords design
#' @return a list
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' \donttest{
#' set_design(data = ech::toy_ech_2018, level = "h")
#' }
#

set_design <- function(data = ech::toy_ech_2018,
                       level = "i",
                       ids = NULL,
                       numero = "numero",
                       estred13 = "estred13",
                       pesoano = "pesoano"){

   if (level == "h") {
    d <- data %>%
      dplyr::mutate(estred13 = as.character(estred13)) %>%
      dplyr::filter(duplicated(numero) == FALSE) %>%
      srvyr::as_survey_design(ids = 1, strata = estred13, weights = pesoano)
  } else {
    d <- data %>%
     dplyr::mutate(estred13 = as.character(estred13)) %>%
     srvyr::as_survey_design(ids = numero, strata = estred13, weights = pesoano)
  }

 return(d)

}
