#' A function to set survey design
#'
#' This function allows you to set the survey desing
#' @param data data frame with ECH microdata
#' @param level is household ("h") or individual ("i")
#' @importFrom glue glue
#' @importFrom srvyr as_survey_design
#' @importFrom magrittr %>%
#' @keywords design
#' @export
#' @return d
#' @examples
#' \donttest{
#' set_design(data = ech::toy_ech_2018, level = "h")
#' }
#

set_design <- function(data = ech::toy_ech_2018, level = "i"){
  utils::globalVariables(c("numero", "estred13", "pesoano"))
  if (level == "h") {
    d <- data %>%
      filter(duplicated(numero) == FALSE) %>%
      srvyr::as_survey_design(ids = numero, strata = estred13, weights = pesoano)
  } else {
    d <- data %>%
     srvyr::as_survey_design(ids = numero, strata = estred13, weights = pesoano)
  }

  # return(d)
  # define package environment and package wide parameters
  # .ech.pkg.env <<- new.env(parent = emptyenv())
  # # # rm(list = ls(envir = .ech.pkg.env), envir = .ech.pkg.env)
  #
  # .ech.pkg.env$design <- d
}
