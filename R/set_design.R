#' A function to set survey design
#'
#' This function allows you to set the survey desing
#' @param data data frame with ECH microdata
#' @param level is household ("h") or individual ("i")
#' @param ids Variables specifying cluster ids from largest level to smallest level (leaving the argument empty, NULL, 1, or 0 indicate no clusters).
#' @param strata Variables specifying strata.
#' @param weights Variables specifying weights (inverse of probability).
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

set_design <- function(data = ech::toy_ech_2018, level = "i", ids = "numero", strata = "estred13", weights = "pesoano"){
   if (level == "h") {
    d <- data %>%
      filter(duplicated(ids) == FALSE) %>%
      srvyr::as_survey_design(ids, strata, weights)
  } else {
    d <- data %>%
     srvyr::as_survey_design(ids, strata, weights)
  }

  # return(d)
  # define package environment and package wide parameters
  # .ech.pkg.env <<- new.env(parent = emptyenv())
  # # # rm(list = ls(envir = .ech.pkg.env), envir = .ech.pkg.env)
  #
  # .ech.pkg.env$design <- d
}
