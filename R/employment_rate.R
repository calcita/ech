#' An employment rate function
#'
#' This function allows you to..
#' @param data data frame with microdata
#' @param pobcoac Definition of categories: "Ocupados", "Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"
#' @keywords employment
#' @export
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate enquo
#' @importFrom rlang .data sym quo_name
#' @examples
#' \donttest{
#' employment_rate()
#' }

employment_rate <- function(data = ech::toy_ech_2018,
                            pobpcoac = "pobpcoac"){
  if (exists("pea", data)) warning('pea pre-existing')
  if (!exists(pobpcoac, data)) stop("pobpcoac variable name not in data")
  pobpcoac = quo_name(enquo(pobpcoac))
  data %<>% dplyr::mutate(pea = ifelse(!!sym(pobpcoac) %in% 2:5, 1, 0),
                          pet = ifelse(!!sym(pobpcoac) != 1, 1, 0),
                          po = ifelse(!!sym(pobpcoac) == 2, 1, 0),
                          pd = ifelse(!!sym(pobpcoac) %in% 3:5, 1, 0)
  )
  # chequear si es numérica y si lo es convertir a character

  # data %<>% mutate(pea = if_else(pobpcoac %in% c("Ocupados", "Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"), 1, 0),
  #                        pet = if_else(pobpcoac != "Menores de 14 años", 1, 0),
  #                        po = if_else(pobpcoac == "Ocupados", 1, 0),
  #                        pd = if_else(pobpcoac %in% c("Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"), 1, 0)
  # )

}
