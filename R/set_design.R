#' set_design
#' @description This function allows you to set the survey desing
#'
#' @param data data frame with ECH microdata
#' @param level is household ("h") or individual ("i")
#' @param ids variables specifying the unit primary sampling (it's not a public variable)
#' @param numero variables specifying  the householder ids
#' @param estrato variable specifying strata
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
                       numero = "numero",
                       ids = NULL,
                       estrato = NULL,
                       pesoano = "pesoano"){

  if(is.null(ids)){
    if (level == "h") {
      d <- data %>%
        dplyr::filter(duplicated(numero) == FALSE) %>%
        srvyr::as_survey_design(ids = 1, weights = pesoano)
    } else {
      d <- data %>%
        srvyr::as_survey_design(ids = numero, weights = pesoano)
    }
  } else {
    if (level == "h") {
      d <- data %>%
        dplyr::mutate(estrato = as.character(estrato)) %>%
        dplyr::filter(duplicated(numero) == FALSE) %>%
        srvyr::as_survey_design(ids = ids, strata = estrato, weights = pesoano)
    } else {
      d <- data %>%
        dplyr::mutate(estrato = as.character(estrato)) %>%
        srvyr::as_survey_design(ids = ids, strata = estrato, weights = pesoano)
    }
  }

 return(d)

}
