#' An employment rate function
#'
#' This function allows you to..
#' @param data data frame with microdata
#' @param pobcoac Definition of categories: "Ocupados", "Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"
#' @keywords employment
#' @export
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @examples
#' \dontest{
#' employment_rate()
#' }

employment_rate <- function(data = ech::toy_ech_2018,
                            pobcoac = "pobcoac"){

  # data %<>% dplyr::mutate(pea = ifelse(pobpcoac %in% 2:5, 1, 0),
  #                         pet = ifelse(pobpcoac != 1, 1, 0),
  #                         po = ifelse(pobpcoac == 2, 1, 0),
  #                         pd = ifelse(pobpcoac %in% 3:5, 1, 0)
  # )
  # chequear si es numérica y si lo es convertir a character

  # data %<>% mutate(pea = if_else(pobpcoac %in% c("Ocupados", "Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"), 1, 0),
  #                        pet = if_else(pobpcoac != "Menores de 14 años", 1, 0),
  #                        po = if_else(pobpcoac == "Ocupados", 1, 0),
  #                        pd = if_else(pobpcoac %in% c("Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"), 1, 0)
  # )

}
