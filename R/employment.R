#' employment
#' This function allows you to calculate the variables: PEA, PET, PO, PD
#'
#' @param data data.frame with microdata
#' @param pobpcoac Variable name of definition of population by activity status. Default: "pobpcoac"
#'
#' @keywords employment
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' \donttest{
#' toy_ech_2018 <- employment(data = ech::toy_ech_2018, pobpcoac = "pobpcoac")
#' }

employment <- function(data = ech::toy_ech_2018,
                       pobpcoac = "pobpcoac") {

# checks ---
  assertthat::assert_that(is.data.frame(data), msg = glue::glue("Sorry... :( \n  is not a data.frame"))
  assertthat::assert_that(pobpcoac %in% names(data), msg = glue::glue("Sorry... :( \n  {popbcoac} is not in data"))

# variables ---
  data <- data %>% dplyr::mutate(pea = ifelse({{pobpcoac}} %in% 2:5, 1, 0),
                          pet = ifelse({{pobpcoac}} != 1, 1, 0),
                          po  = ifelse({{pobpcoac}} == 2, 1, 0),
                          pd  = ifelse({{pobpcoac}} %in% 3:5, 1, 0)
  )

  message("Variables have been created: \n \t pea (Poblacion economicamente activa);
         pet (Poblacion en edad de trabajar);
         po (Poblacion ocupada) &
         pd (Poblacion desocupada)")
  return(data)
}


#' branch_ciuu
#'
#' @param data data.frame
#' @param f72_2 Variable name of ciiu code rev.4
#' @param group logical to define 12 or 18 categories, if FALSE code 18. Default: TRUE
#' @param disaggregated logical to define disaggregated branches or not. Default: FALSE
#'
#' @return data.frame
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' \donttest{
#' toy_ech_2018 <- branch_ciiu(data = ech::toy_ech_2018)
#' }

branch_ciiu <- function(data = ech::toy_ech_2018,
                        f72_2 = "f72_2",
                        group = TRUE,
                        disaggregated = FALSE){

    if(is.character(data$f72_2)) data$f72_2 <- as.numeric(data$f72_2)

    data <- data %>%
      dplyr::mutate(branch_ciiu = dplyr::case_when(f72_2 < 1000 ~ 1, #faltan evaluar los NA
                                                                f72_2 < 4000 ~ 2,
                                                                f72_2 < 4500 ~ 3,
                                                                f72_2 < 4900 ~ 4,
                                                                f72_2 < 5500 ~ 5,
                                                                f72_2 < 5800 ~ 6,
                                                                f72_2 < 6400 ~ 7,
                                                                f72_2 < 6800 ~ 8,
                                                                f72_2 < 6900 ~ 9,
                                                                f72_2 < 7500 ~ 10,
                                                                f72_2 < 8400 ~ 11,
                                                                f72_2 < 8500 ~ 12,
                                                                f72_2 < 8600 ~ 13,
                                                                f72_2 < 9000 ~ 14,
                                                                f72_2 < 9400 ~ 15,
                                                                f72_2 < 9700 ~ 16,
                                                                f72_2 < 9900 ~ 17,
                                                                TRUE ~ 18))

  if (group == TRUE){
    data <- data %>%
      dplyr::mutate(branch_group_ciiu = dplyr::case_when( #faltan evaluar los NA
        branch_ciiu == 1 ~ "Agropecuaria, pesca, caza y explotacion de minas o conteras",
        branch_ciiu == 2 ~ "Industria manufacturera, Suministro de electricidad, gas y agua",
        branch_ciiu == 3 ~  "Construccion",
        branch_ciiu == 4 ~ "Comercio por menor y por mayor; Alojamiento y servicio de comida",
        branch_ciiu == 5 ~ "Transporte y almacenamiento",
        branch_ciiu == 6 ~ "Comercio por menor y por mayor; Alojamiento y servicio de comida",
        branch_ciiu == 7 ~ "Informatica y Comunicacion",
        branch_ciiu == 8 ~ "Actividades financieras y de seguros",
        branch_ciiu == 9 ~ "Actividades inmobiliarias",
        branch_ciiu == 10 ~ "Actividades profesionales, cientificas y tecnicas",
        branch_ciiu == 11 ~ "Actividades administrativas y servicio de apoyo",
        branch_ciiu == 12 ~ "Administracion Publica; Defensa y Actividades de organizaciones y organos extraterritoriales",
        branch_ciiu == 18 ~ "Administracion Publica; Defensa y Actividades de organizaciones y organos extraterritoriales",
        branch_ciiu == 13 ~ "Ensenanza",
        branch_ciiu == 14 ~ "Servicios sociales y Salud",
        branch_ciiu == 15 ~ "Otras actividades de servicio; Arte, entretenimiento y recreacion",
        branch_ciiu == 16 ~ "Otras actividades de servicio; Arte, entretenimiento y recreacion",
        branch_ciiu == 17 ~ "Actividades de los hogares como empleadores",
        TRUE ~ ""))
    message("A variable has been created: \n \t branch_ciiu (Rama de actividad CIIU)
            branch_group_ciiu (Rama de actividad CIIU agrupadas)")
  }

    if (disaggregated == TRUE){
      data <- data %>%
        dplyr::mutate(branch_ciiu_disaggregated = dplyr::case_when( #faltan evaluar los NA
          f72_2 < 301 ~ "Agricultura, ganaderia, caza y silvicultura",
          f72_2 < 500 ~ "Pesca",
          f72_2 < 1000 ~ "Explotacion de minas y canteras",
          f72_2 < 3500 ~ "Industrias manufactureras",
          f72_2 < 4100 ~ "Suministro de electricidad, gas y agua",
          f72_2 < 4500 ~ "Construccion",
          f72_2 < 4900 ~ "Comercio",
          f72_2 < 5800 ~ "Hoteles y restaurantes",
          f72_2 < 5500 | (f72_2 >= 5800 & f72_2 <= 6400) ~ "Transporte, almacenamiento y comunicaciones",
          f72_2 < 6800 ~ "Intermediacion financiera",
          f72_2 < 8400 ~ "Actividades inmobiliarias, empresariales y de alquiler",
          f72_2 < 8500 | f72_2 == 9900 ~ "Administracion publica y defensa, planes de seguridad social",
          f72_2 < 8600 ~ "Ensenanza",
          f72_2 < 9000 ~ "Servicios sociales y de salud",
          f72_2 < 9700 ~ "Otros servicios sociales",
          f72_2 < 9900 ~ "Hogares privados con servicio domestico",
          TRUE ~ ""))
      message("A variable has been created: \n \t branch_ciiu (Rama de actividad CIIU)
              branch_ciiu_disaggregated (Rama de actividad CIIU desagregada)")
    }
    return(data)
}
