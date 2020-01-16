#' A function to estimate variables at universe level
#'
#' This function allows you to estimate variable.
#' @param data data frame with ECH microdata
#' @param variable data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference
#' @param level is household or individual
#' @param ids survey phase
#' @param strata survey strata
#' @param weights ponderation variable
#' @importFrom dplyr mutate, select, filter, group_by
#' @importFrom glue glue
#' @importFrom srvyr summarise
#' @keywords household_type
#' @export
#' @import tidyverse
#' @return table
#' @examples
#' get_estimation(data = d)
#

get_estimation <- function(data = df,
                           variable = NULL,
                           by.x = NULL,
                           by.y = NULL,
                           domain = NULL,
                           level = NULL,
                           design = d){

 # stopifnot(!is.null(data) | !is.null(variable) | is.null(design))
 #  message(glue:glue("Debe indicar una variable a estimar"))

  #if(is.null(by.x) & is.null(by.y)){
  estimation <- design %>%
        srvyr::summarise(colname = srvyr::survey_total(vartype = "ci"))
  # } else if(!is.null(by.x) & is.null(by.y)){
  #   est_total <- data_h %>%
  #     srvyr::as_survey_design(ids = numero, strata = strata, weights = weights) %>%
  #     srvyr::group_by(household_type, {{by.x}}) %>%
  #     srvyr::summarise(tipo_hogar = srvyr::survey_total(vartype = "ci"))
  # } else {
  #   est_total <- data_h %>%
  #     srvyr::as_survey_design(ids = numero, strata = strata, weights = weights) %>%
  #     srvyr::group_by(household_type, {{by.x}}, {{by.y}}) %>%
  #     srvyr::summarise(tipo_hogar = srvyr::survey_total(vartype = "ci"))
  # }

  # if (is.null(by.y) & !is.null(by.x)) {
  #   table <- d %>%
  #     srvyr::summarise(tasa_empleo = survey_ratio(po, pet, vartype = "ci"))
  # } else if (!is.null(by) & length(by) == 1 & geo.unit == "uy") {
  #   table <- design_p %>%
  #     srvyr::group_by({{by}}) %>%
  #     srvyr::summarise(tasa_empleo = survey_ratio(po, pet, vartype = "ci"))
  # } else if (is.null(by) & geo.unit != "uy") {
  #   table <- design_p %>%
  #     srvyr::group_by({{geo.unit}}) %>%
  #     srvyr::summarise(tasa_empleo = survey_ratio(po, pet, vartype = "ci"))
  # } else {
  #   table <- design_p %>%
  #     srvyr::group_by({{by}}, {{geo.unit}}) %>%
  #     srvyr::summarise(tasa_empleo = survey_ratio(po, pet, vartype = "ci"))
  # }
  # return(table)

  return(estimation)

}
