#' set_design
#' This function allows you to set the survey desing
#'
#' @param data data frame with ECH microdata
#' @param level is household ("h") or individual ("i")
#' @param ids variables specifying cluster ids from largest level to smallest level (leaving the argument empty, NULL, 1, or 0 indicate no clusters).
#' @param strata variable specifying strata.
#' @param weights variable specifying weights (inverse of probability).
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
                       ids = "numero",
                       strata = "estred13",
                       weights = "pesoano"){

   if (level == "h") {
    d <- data %>%
      filter(duplicated(ids) == FALSE) %>%
      srvyr::as_survey_design(ids, strata, weights)
  } else {
    d <- data %>%
     srvyr::as_survey_design(ids, strata, weights)
  }

 return(d)

}
