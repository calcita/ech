#' An employment Function
#'
#' This function allows you to..
#' @param
#' @keywords employment
#' @export
#' @import tidyverse srvyr magrittr
#' @examples
#' employment_rate()

employment_rate <- function(data = df,
                            geo.unit = "uy",
                            by = NULL,
                            pobcoac = "pobcoac",
                            weights = "pesoano"){
    if (exists("pea", data)) warning('pea pre-existing')

  # df %<>% mutate(pea = if_else(pobpcoac %in% 2:5, 1, 0),
  #                        pet = if_else(pobpcoac != 1, 1, 0),
  #                        po = if_else(pobpcoac == 2, 1, 0),
  #                        pd = if_else(pobpcoac %in% 3:5, 1, 0)
  #                       )
  # chequear si es numérica y si lo es convertir a character

  df %<>% mutate(pea = if_else(pobpcoac %in% c("Ocupados", "Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"), 1, 0),
                         pet = if_else(pobpcoac != "Menores de 14 años", 1, 0),
                         po = if_else(pobpcoac == "Ocupados", 1, 0),
                         pd = if_else(pobpcoac %in% c("Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"), 1, 0)
  )

  design_p <- survey.design.ech(df, nivel = "p", weights = weights)
  if (is.null(by) & geo.unit == "uy") {
    table <- design_p %>%
             srvyr::summarise(tasa_empleo = survey_ratio(po, pet, vartype = "ci"))
  } else if (!is.null(by) & length(by) == 1 & geo.unit == "uy") {
    table <- design_p %>%
            srvyr::group_by({{by}}) %>%
            srvyr::summarise(tasa_empleo = survey_ratio(po, pet, vartype = "ci"))
  } else if (is.null(by) & geo.unit != "uy") {
    table <- design_p %>%
             srvyr::group_by({{geo.unit}}) %>%
             srvyr::summarise(tasa_empleo = survey_ratio(po, pet, vartype = "ci"))
  } else {
    table <- design_p %>%
             srvyr::group_by({{by}}, {{geo.unit}}) %>%
             srvyr::summarise(tasa_empleo = survey_ratio(po, pet, vartype = "ci"))
  }
  return(table)
}
