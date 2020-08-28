#' This function allows you to calculate the variables: PEA, PET, PO, PD
#' @family employment
#' @param data data.frame with microdata
#' @param pobpcoac Variable name of definition of population by activity status. Default: "pobpcoac"
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
  data <- data %>% dplyr::mutate(pea = ifelse(pobpcoac %in% 2:5, 1, 0),
                          pet = ifelse(pobpcoac != 1, 1, 0),
                          po  = ifelse(pobpcoac == 2, 1, 0),
                          pd  = ifelse(pobpcoac %in% 3:5, 1, 0),
                          pea = haven::labelled(pea, labels = c("Si" = 1, "No" = 0),
                                                label = "Poblacion economicamente activa"),
                          pet = haven::labelled(pet, labels = c("Si" = 1, "No" = 0),
                                                label = "Poblacion en edad de trabajar"),
                          po = haven::labelled(po, labels = c("Si" = 1, "No" = 0),
                                               label = "Poblacion ocupada"),
                          pd = haven::labelled(pd, labels = c("Si" = 1, "No" = 0),
                                               label = "Poblacion desocupada")
  )

  message("Variables have been created: \n \t pea (Poblacion economicamente activa);
         pet (Poblacion en edad de trabajar);
         po (Poblacion ocupada) &
         pd (Poblacion desocupada)")
  return(data)
}

#' This function allows you to identify underemployed people
#' @family employment
#' @param data data.frame
#' @param pobpcoac Variable name of definition of population by activity status. Default: "pobpcoac"
#' @param f85 Variable name of number of hours worked in the main job
#' @param f98 Variable name of Number of hours worked at the secondary job
#' @param f101 Variable name of reasons why you want another job
#' @param f102 Variable name of want to work more hours
#' @param f103 Variable name of are available to work more hours at this time
#' @param f104 Variable name of reasons why you dont work more hours
#'
#' @importFrom dplyr mutate case_when
#' @importFrom haven labelled
#' @return data.frame
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' toy_ech_2018 <- underemployment(data = ech::toy_ech_2018)

underemployment <- function(data = ech::toy_ech_2018,
                         pobpcoac = "pobpcoac",
                         f85 = "f85",
                         f98 = "f98",
                         f101 = "f101",
                         f102 = "f102",
                         f103 = "f103",
                         f104 = "f104"){

  # checks ---
  assertthat::assert_that(is.data.frame(data), msg = glue::glue("Sorry... :( \n  is not a data.frame"))
  assertthat::assert_that(pobpcoac %in% names(data), msg = glue::glue("Sorry... :( \n  {popbcoac} is not in data"))
  assertthat::assert_that(f85 %in% names(data), msg = glue::glue("Sorry... :( \n  {f85} is not in data"))
  assertthat::assert_that(f98 %in% names(data), msg = glue::glue("Sorry... :( \n  {f98} is not in data"))
  assertthat::assert_that(f101 %in% names(data), msg = glue::glue("Sorry... :( \n  {f101} is not in data"))
  assertthat::assert_that(f102 %in% names(data), msg = glue::glue("Sorry... :( \n  {f102} is not in data"))
  assertthat::assert_that(f103 %in% names(data), msg = glue::glue("Sorry... :( \n  {f103} is not in data"))
  assertthat::assert_that(f104 %in% names(data), msg = glue::glue("Sorry... :( \n  {f104} is not in data"))

  data <- data %>% dplyr::mutate(underemployment = dplyr::case_when(pobpcoac == 2 & (f85 +  f98 < 40) & (f101 == 1 | f102 == 1) & (f103 == 1) & (f104 == 5) ~ 1,
                                                                   TRUE ~ 0),
                                 underemployment = haven::labelled(underemployment, labels = c("Si" = 1, "No" = 0), label = "Poblacion subempleada")
      )

    message("A variable has been created: \n \t underemployment (Poblacion subempleada)")
    return(data)

}

#' This function allows you to identify workers with employment restrictions
#' @family employment
#' @param data data.frame
#' @param f82 Variable name of contribution to the pension fund
#' @param underemployment Variable name of underemployment
#' @return data.frame
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#' @examples
#' toy_ech_2018 <- underemployment(data = ech::toy_ech_2018)
#' toy_ech_2018 <- employment_restrictions(data = toy_ech_2018)

employment_restrictions  <- function(data = ech::toy_ech_2018,
                                     f82 = "f82",
                                     underemployment = "underemployment"){

  # checks ---
  assertthat::assert_that(is.data.frame(data), msg = glue::glue("Sorry... :( \n  is not a data.frame"))
  assertthat::assert_that(f82 %in% names(data), msg = glue::glue("Sorry... :( \n  {f82} is not in data"))
  assertthat::assert_that(underemployment %in% names(data), msg = glue::glue("Sorry... :( \n  {underemployment} is not in data"))

  data <- data %>% dplyr::mutate(employment_restrictions = dplyr::case_when(
    f82 == 1 & underemployment == 0 ~ 1,
    f82 == 2 & underemployment == 0 ~ 2,
    f82 == 1 & underemployment == 1 ~ 3,
    f82 == 2 & underemployment == 1 ~ 4),
    employment_restrictions =  haven::labelled(employment_restrictions,
                                               labels = c("Sin restricciones" = 1,  "Restriccion por no aporte" = 2, "Restriccion por subempleo" = 3, "Restriccion por subempleo y no aporte " = 4),
                                               label = "Restricciones al empleo"))

  message("A variable has been created: \n \t  employment_restrictions (restricciones al empleo)")
  return(data)
}

#' This function allows you to identify activity branches
#' @family employment
#' @param data data.frame
#' @param f72_2 Variable name of ciiu code rev.4
#' @param group logical to define 12 or 18 categories, if FALSE code 18. Default: TRUE
#' @param disaggregated logical to define disaggregated branches or not. Default: FALSE
#' @return data.frame
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- branch_ciiu(data = ech::toy_ech_2018)
#' }

branch_ciiu <- function(data = ech::toy_ech_2018,
                        f72_2 = "f72_2",
                        group = TRUE,
                        disaggregated = FALSE){

  # checks ---
  assertthat::assert_that(is.data.frame(data), msg = glue::glue("Sorry... :( \n  is not a data.frame"))
  assertthat::assert_that(f72_2 %in% names(data), msg = glue::glue("Sorry... :( \n  {f72_2} is not in data"))

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
