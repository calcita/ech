#' housing_deprivation
#'
#' This function allows you to calculate the housing status
#' @param data data frame
#' @param ht19 number of individuals in the household
#' @param d9 number of rooms
#' @param d10 number of rooms to sleep
#' @param d11 source of water
#' @param d12 llegada del agua a la vivienda
#' @param d13 servicio sanitario
#' @param d16 evacuacion del servicio sanitario
#' @param d18 energy source for lighting
#' @param d19 cooking space
#' @param c2 predominant material on external walls
#' @param c3 predominant roofing material
#' @param c4 predominant flooring material
#' @param quintil income quintil
#' @param region_4 region
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @return data.frame
#' @details Disclaimer: El script no es un producto oficial de INE.
#' @export
#'
#' @examples
#' \donttest{
#' toy_ech_2018 <- income_constant_prices(data = ech::toy_ech_2018)
#' toy_ech_2018 <- income_quantiles(data = toy_ech_2018)
#' toy_ech_2018 <- housing_deprivation(data = toy_ech_2018)
#' }
housing_deprivation <- function(data = ech::toy_ech_2018,
                         ht19 = "ht19",
                         d9 = "d9",
                         d10 = "d10",
                         d11 = "d11",
                         d12 = "d12",
                         d13 = "d13",
                         d16 = "d16",
                         d18 = "d18",
                         d19 = "d19",
                         c2 = "c2",
                         c3 = "c3",
                         c4 = "c4",
                         quintil = "quintil",
                         region_4 = "region_4"
                         ) {

  # checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(ht19  %in% names(data), msg =  glue:glue("Sorry... :( \n {ht19} is not in data"))
  assertthat::assert_that(d9  %in% names(data), msg =  glue:glue("Sorry... :( \n {d9} is not in data"))
  assertthat::assert_that(d10  %in% names(data), msg =  glue:glue("Sorry... :( \n {d10} is not in data"))
  assertthat::assert_that(d11  %in% names(data), msg =  glue:glue("Sorry... :( \n {d11} is not in data"))
  assertthat::assert_that(d12  %in% names(data), msg =  glue:glue("Sorry... :( \n {d12} is not in data"))
  assertthat::assert_that(d13  %in% names(data), msg =  glue:glue("Sorry... :( \n {d13} is not in data"))
  assertthat::assert_that(d16  %in% names(data), msg =  glue:glue("Sorry... :( \n {d16} is not in data"))
  assertthat::assert_that(d18  %in% names(data), msg =  glue:glue("Sorry... :( \n {d18} is not in data"))
  assertthat::assert_that(d19  %in% names(data), msg =  glue:glue("Sorry... :( \n {d19} is not in data"))
  assertthat::assert_that(c2  %in% names(data), msg =  glue:glue("Sorry... :( \n {c2} is not in data"))
  assertthat::assert_that(c3  %in% names(data), msg =  glue:glue("Sorry... :( \n {c3} is not in data"))
  assertthat::assert_that(c4  %in% names(data), msg =  glue:glue("Sorry... :( \n {c4} is not in data"))
  assertthat::assert_that(region_4  %in% names(data), msg =  glue:glue("Sorry... :( \n {region_4} is not in data"))
  assertthat::assert_that(quintil %in% names(data), msg = "Sorry... :( \n quintil parameter is not calculated, please use income_constant_prices() to obtain the variable.")


  data <- data %>%
    dplyr::mutate(
      overcrowding = ifelse((ht19 / d10) > 2, 1, 0), #Carencia: Hacinamiento
      bathroom = ifelse(d13 != 1, 1, 0), #Carencia: Baño (Mínimo un baño)
      rooms = ifelse((d19 == 3 | (d9 - d10) < 1 & ht19 > 1), 1, 0), #Carencia: Ambientes adecuados(cocina, comedor, estar diario)
      roof_materials = ifelse(c3 %in% c(4, 6) | (quintil < 5 & c3 == 5), 1, 0), #Carencia: Techo adecuado
      wall_materials = ifelse(c2 %in% c(4, 6) | (quintil < 5 & c2 == 5), 1, 0), #Carencia: Paredes adecuadas
      floor_materials = ifelse(c4 > 3, 1, 0), #Carencia: Pisos adecuados
      water = ifelse(d12 > 1, 1, 0), #Carencia: Agua
      running_water = ifelse((region_4 < 4 & d11 > 1) | (region_4 == 4 & d11 %in% c(2, 5:6)), 1, 0), #Carencia: Red general para el agua o pozo
      drainage = ifelse((d14 == 0) | (d16 > 2), 1, 0), #Carencia: Desague
      electricity = ifelse((region_4 < 4 & d18 > 1) | (region_4 == 4 & d18 > 2), 1, 0), #Carencia: Red eléctrica
      housing_deprivation_q = overcrowding + bathroom + rooms + roof_materials + wall_materials + floor_materials + water + running_water + drainage + electricity, #Cantidad de carencias de vivienda
      housing_deprivation = ifelse(housing_deprivation_q > 0, 1, 0)
  )

}



#' A function to calculate the housing situation
#'
#' @param data data.frame
#' @param c5_1 roof condensation
#' @param c5_2 roof drips
#' @param c5_3 walls cracks
#' @param c5_4 broken doors or windows
#' @param c5_5 floors cracks
#' @param c5_6 plaster drop on walls
#' @param c5_7 detached ceilings
#' @param c5_8 poor sunlight
#' @param c5_9 poor ventilation
#' @param c5_10 floods when it rains
#' @param c5_11 in danger of collapse
#' @param c5_12 dampness in the foundations
#'
#' @return data.frame
#' @export
#'
#' @examples
#' toy_ech_2018 <- housing_situation(data = ech::toy_ech_2018)
#'
housing_situation <- function(data = ech::toy_ech_2018,
                              c5_1 = "c5_1",
                              c5_2 = "c5_2",
                              c5_3 = "c5_3",
                              c5_4 = "c5_4",
                              c5_5 = "c5_5",
                              c5_6 = "c5_6",
                              c5_7 = "c5_7",
                              c5_8 = "c5_8",
                              c5_9 = "c5_9",
                              c5_10 = "c5_10",
                              c5_11 = "c5_11",
                              c5_12 = "c5_12"){

  data <- data %>%
    dplyr::mutate(
      housing_situation = ifelse(c5_1 == 2 & c5_2 == 2 &  c5_3 == 2 & c5_4 == 2 & c5_5 == 2 & c5_6 == 2 & c5_7 == 2 & c5_8 == 2 & c5_9 == 2 & c5_10 == 2 & c5_11 == 2 & c5_12 == 2, "Sin problemas",
                                 ifelse(c5_1 == 2 & c5_2 == 2 & c5_3 == 2 & c5_6 == 2  & c5_7 == 2  & c5_10 == 2 & c5_11 == 2 & c5_12 == 2  & (c5_4 == 1 | c5_5 == 1 | c5_8 == 1  | c5_9 == 1), "Problemas leves",
                                        ifelse(c5_3 == 2 & c5_10 == 2 & c5_11 == 2 & (c5_1 == 1 | c5_2 == 1 | c5_6 == 1 | c5_7 == 1 | c5_12 == 1), "Problemas moderados",
                                               ifelse(c5_3 == 1 | c5_10 == 1  | c5_11 == 1, "Problemas graves", ""
    )))))

}



#' A function to calculate the housing conditions
#'
#' @param data data.frame
#' @param c2 predominant material on external walls
#' @param c3 predominant roofing material
#' @param c4 predominant flooring material
#'
#' @return data.frame
#' @export
#'
#' @examples
#' toy_ech_2018 <- housing_conditions(data = ech::toy_ech_2018)
#'
housing_conditions <- function(data = ech::toy_ech_2018,
                               c2 = "c2",
                               c3 = "c3",
                               c4 = "c4"){

  data <- data %>%
    dplyr::mutate(
      housing_conditions = ifelse(c2 == 6 | (c3 == 6 & c2 %in% c(2, 4, 5)) | (c3 == 6 & c4 %in% 4:5 & c2 %in% c(1,3)) | (c3 == 5 & c4 == 5 & c2 %in% c(2, 4, 5)) | (c3 == 4 & c4 == 5 & c2 %in% c(2, 4)), "Precaria",
        ifelse((c3== 6 & c4 %in% 1:3 & c2 %in% c(1, 3)) | (c3 == 4 & c4 %in% 1:4 & c2 %in% 1:5) | (c3 == 4 & c4 == 5 & c2 %in% c(1, 3, 5)) | (c3 == 5 & c4 %in% 1:4 & c2 == 4) | (c3 == 5 &  c4 == 5 & c2 %in% c(1, 3)) | (c3 == 5 & c4 == 4 & c2 == 2) | (c3 %in% 1:3 & c4 == 5 & c2 %in% 1:5) | (c3 %in% 1:3 & c4 == 4 & c2 %in% c(2, 4, 5)) | (c3 %in% 1:3  & c4 %in% 1:3 & c2 == 4) | (c3 %in% 2:3 & c4 == 3 & c2 == 2), "Modesta",
        ifelse((c3 == 5 & c4 == 4 & c2 %in% c(1, 3, 5)) | (c3 == 5 & c4 %in% 1:3 & c2 %in% c(2, 5)) | (c3 %in% 1:3 & c4 == 4  & c2 %in% 1:3) | (c3 %in% 1:3 & c4 %in% 2:3 & c2 == 5) | (c3 %in% 1:2 & c4 == 1 & c2 == 5) | (c3 == 3 & c4 %in% 1:2 & c2 == 2) | (c3 == 2 & c4 %in% 2:3 & c2 == 3) | (c3 %in% 1:2 & c4 %in% 1:2 & c2 == 2) | (c3 == 2 & c4 %in% 2:3 & c2 == 1) | (c3 == 1 & c4 == 3 & c2 == 2), "Mediana",
        ifelse((c3 %in% c(1, 3, 5) & c4 %in% 1:3 & c2 %in% c(1, 3)) | (c3 == 3 & c4 == 1 & c2 == 5) | (c3 == 2 & c4 == 1 & c2 %in% c(1, 3)), "Buena", ""
        )))))
}

#' A function to calculate overcrowding in the household
#'
#' @param data data.frame
#' @param ht19 Number of individuals in the household
#' @param d10  Number of rooms to sleep
#'
#' @return data.frame
#' @export
#'
#' @examples
#' toy_ech_2018 <- overcrowding(data = ech::toy_ech_2018)
#'
overcrowding <- function(data = ech::toy_ech_2018,
                         ht19 = "ht19",
                         d10 = "d10"){

  data <- data %>%
    dplyr::mutate(overcrowding = dplyr::case_when(
     ht19 / d10 > 2 ~ 1,
     TRUE ~ 0
    )
  )

}


#' A function to calculate the housing tenure
#'
#' @param data data.frame
#' @param d8_1 housing_tenure (owner, renter, rent-free occupancy, etc.)
#'
#' @return data.frame
#' @export
#'
#' @examples
#' toy_ech_2018 <- housing_tenure(data = ech::toy_ech_2018)
#'
housing_tenure <- function(data = ech::toy_ech_2018,
                           d8_1 = "d8_1"){

  data <- data %>%
    dplyr::mutate(
      housing_tenure = ifelse(as.integer(d8_1) %in% c(1, 2, 10), "Propietaria-o",
        ifelse(as.integer(d8_1) == 5, "Inquilina-o",
        ifelse(as.integer(d8_1) %in% 6:8, "Ocupante gratuito",
        ifelse(as.integer(d8_1) == 9, "Ocupante sin permiso",
        ifelse(as.integer(d8_1) %in% 3:4, "Propietaria-o solo de la vivienda", "")))))
    )
}
