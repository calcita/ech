#' An employment rate function
#'
#' This function allows you to..
#' @param data data frame with microdata
#' @param pobpcoac Definition of categories: "Ocupados", "Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"
#' @keywords employment
#' @export
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' employment_rate()
#' }

employment_rate <- function(data = ech::toy_ech_2018,
                            pobpcoac = "pobpcoac"){
  if (exists("pea", data)) warning('pea pre-existing')
  if (!exists(pobpcoac, data)) stop("pobpcoac variable name not in data")
  data %<>% dplyr::mutate(pea = ifelse(.data[[pobpcoac]] %in% 2:5, 1, 0),
                          pet = ifelse(.data[[pobpcoac]] != 1, 1, 0),
                          po = ifelse(.data[[pobpcoac]] == 2, 1, 0),
                          pd = ifelse(.data[[pobpcoac]] %in% 3:5, 1, 0)
  )
  # chequear si es numérica y si lo es convertir a character

  # data %<>% mutate(pea = if_else(pobpcoac %in% c("Ocupados", "Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"), 1, 0),
  #                        pet = if_else(pobpcoac != "Menores de 14 años", 1, 0),
  #                        po = if_else(pobpcoac == "Ocupados", 1, 0),
  #                        pd = if_else(pobpcoac %in% c("Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"), 1, 0)
  # )

}
