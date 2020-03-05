#' A function to set survey design
#'
#' This function allows you to set the survey desing
#' @param data data frame with ECH microdata
#' @importFrom glue glue
#' @importFrom srvyr as_survey_design
#' @importFrom magrittr %>%
#' @keywords design
#' @export
#' @return d
#' @examples
#' set_design(data = ech::toy_ech_2018)
#

set_design <- function(data = ech::toy_ech_2018){

  # d <- data %>%
  #   srvyr::as_survey_design(ids = 1, strata = estred13, weights = pesoano)
  # return(d)
  #define package environment and package wide parameters

  # .ech.pkg.env <<- new.env(parent = emptyenv())
  # # rm(list = ls(envir = .ech.pkg.env), envir = .gemrtables.pkg.env)
  #
  # .ech.pkg.env$design <- d

}
