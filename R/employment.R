#' An employment function
#'
#' This function allows you to calculate the variables: PEA, PET, PO, PD
#' @param data data frame with microdata
#' @param pobpcoac Definition of population by activity status
#' @keywords employment
#' @export
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- employment(data = ech::toy_ech_2018, pobpcoac = "pobpcoac")
#' }

employment <- function(data = ech::toy_ech_2018,
                       pobpcoac = "pobpcoac"){

# checks ---
   if (exists("pea", data)) warning('pea pre-existing')
   if (!exists(pobpcoac, data)) stop("pobpcoac variable name not in data")

# variables ---
    data %<>% dplyr::mutate(pea = ifelse(.data[[pobpcoac]] %in% 2:5, 1, 0),
                            pet = ifelse(.data[[pobpcoac]] != 1, 1, 0),
                            po = ifelse(.data[[pobpcoac]] == 2, 1, 0),
                            pd = ifelse(.data[[pobpcoac]] %in% 3:5, 1, 0)
  )
}
