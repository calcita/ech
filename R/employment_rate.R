#' An employment rate function
#'
#' This function allows you to..
#' @param data data frame with microdata
#' @keywords employment
#' @export
#' @import tidyverse srvyr magrittr
#' @examples
#' employment_rate()

employment_rate <- function(data = df,
                            pobcoac = "pobcoac"){

  data %<>% mutate(pea = if_else(pobpcoac %in% 2:5, 1, 0),
                   pet = if_else(pobpcoac != 1, 1, 0),
                   po = if_else(pobpcoac == 2, 1, 0),
                   pd = if_else(pobpcoac %in% 3:5, 1, 0)
                        )
  # chequear si es numérica y si lo es convertir a character

  # data %<>% mutate(pea = if_else(pobpcoac %in% c("Ocupados", "Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"), 1, 0),
  #                        pet = if_else(pobpcoac != "Menores de 14 años", 1, 0),
  #                        po = if_else(pobpcoac == "Ocupados", 1, 0),
  #                        pd = if_else(pobpcoac %in% c("Desocupados buscan trab. por 1a. vez", "Desocupados propiamente dichos", "Desocupados en seguro de paro"), 1, 0)
  # )

}
