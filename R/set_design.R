#' A function to set survey design
#'
#' This function allows you to set the survey desing
#' @param data data frame with ECH microdata
#' @param level is household or individual
#' @param ids survey phase
#' @param strata survey strata
#' @param weights ponderation variable
#' @importFrom glue glue
#' @importFrom srvyr as_survey_design
#' @importFrom magrittr %>%
#' @keywords design
#' @export
#' @return d
#' @examples
#' set_design(data = d)
#

set_design <- function(data = df, ids = 1, strata = "estred13", weights = "pesoano"){
  d <- data %>%
    srvyr::as_survey_design(ids = ids, strata = strata, weights = weights)
  return(d)
  #define package environment and package wide parameters

  .ech.pkg.env <<- new.env(parent = emptyenv())
  # rm(list = ls(envir = .ech.pkg.env), envir = .gemrtables.pkg.env)

  .ech.pkg.env$design <- d

}
