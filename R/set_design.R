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
#' @keywords design
#' @export
#' @import tidyverse
#' @return d
#' @examples
#' set_design(data = d)
#

set_design <- function(data = data, ids = 1, strata = estred13, weights = pesoano){
  d <- data %>%
    srvyr::as_survey_design(ids = ids, strata = strata, weights = weights)
  return(d)
}
