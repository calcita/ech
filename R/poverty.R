#' This function allows you to calculate de Unsatisfied Basic Needs
#' @family poverty
#' @param data data.frame
#' @param c2 Variable name of predominant material on external walls
#' @param c3 Variable name of predominant roofing material
#' @param c4 Variable name of predominant flooring material
#' @param d9 Variable name of number of rooms
#' @param d11 Variable name of principal source of potable water
#' @param d12 Variable name of water supply network / water access
#' @param d13 Variable name of sanitary facilities
#' @param d14 Variable name of bathroom presence
#' @param d15 Variable name of private bathroom use
#' @param d16 Variable name of sewerage facilities
#' @param d18 Variable name of energy source for lighting
#' @param d19 Variable name of cooking space
#' @param d21_1 Variable name of heater or termophon presence
#' @param d21_2 Variable name of instantaneous water heater presence
#' @param d21_3 Variable name of fridge presence
#' @param d260 Variable name of energy source for heating
#' @param ht19 Variable name of number of individuals in the household
#' @param pobre06 Variable name of poverty
#' @param e27 Variable name of age
#' @param school_enrollment Variable name of school_enrollment
#' @param years_schooling Variable name of years_schooling
#' @param e238 Variable name of attendance to initial education
#' @param anio Variable name of survey year
#' @return data.frame
#' @export
#' @importFrom dplyr mutate case_when
#' @details
#' Based on http://www.ine.gub.uy/documents/10181/34017/Atlas_fasciculo_1_NBI_versionrevisada.pdf/57ea17f9-3fd9-4306-b9ca-948abc7fab73
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_18 <- enrolled_school(data = ech::toy_ech_2018)
#' toy_ech_18 <- years_of_schooling(toy_ech_18)
#' toy_ech_18 <- unsatisfied_basic_needs(toy_ech_18)
#' }

unsatisfied_basic_needs <- function(data = ech::toy_ech_2018,
                                    c2 = "c2",
                                    c3 = "c3",
                                    c4 = "c4",
                                    d9 = "d9",
                                    d11 = "d11",
                                    d12 = "d12",
                                    d13 = "d13",
                                    d14 = "d14",
                                    d15 = "d15",
                                    d16 = "d16",
                                    d18 = "d18",
                                    d19 = "d19",
                                    d21_1 = "d21_1",
                                    d21_2 = "d21_2",
                                    d21_3 = "d21_3",
                                    d260 = "d260",
                                    ht19 = "ht19",
                                    pobre06 = "pobre06",
                                    e27 = "e27",
                                    school_enrollment = "school_enrollment",
                                    years_schooling = "years_schooling",
                                    e238 = "e238",
                                    anio = "anio"){

  # checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(c2 %in% names(data), msg =  glue::glue("Sorry... :( \n {c2} is not in data"))
  assertthat::assert_that(c3 %in% names(data), msg =  glue::glue("Sorry... :( \n {c3} is not in data"))
  assertthat::assert_that(c4 %in% names(data), msg =  glue::glue("Sorry... :( \n {c4} is not in data"))
  assertthat::assert_that(d9 %in% names(data), msg =  glue::glue("Sorry... :( \n {d9} is not in data"))
  assertthat::assert_that(d11 %in% names(data), msg =  glue::glue("Sorry... :( \n {d11} is not in data"))
  assertthat::assert_that(d12 %in% names(data), msg =  glue::glue("Sorry... :( \n {d12} is not in data"))
  assertthat::assert_that(d13 %in% names(data), msg =  glue::glue("Sorry... :( \n {d13} is not in data"))
  assertthat::assert_that(d14 %in% names(data), msg =  glue::glue("Sorry... :( \n {d14} is not in data"))
  assertthat::assert_that(d15 %in% names(data), msg =  glue::glue("Sorry... :( \n {d15} is not in data"))
  assertthat::assert_that(d16 %in% names(data), msg =  glue::glue("Sorry... :( \n {d16} is not in data"))
  assertthat::assert_that(d18 %in% names(data), msg =  glue::glue("Sorry... :( \n {d18} is not in data"))
  assertthat::assert_that(d19 %in% names(data), msg =  glue::glue("Sorry... :( \n {d19} is not in data"))
  assertthat::assert_that(d21_1 %in% names(data), msg =  glue::glue("Sorry... :( \n {d21_1} is not in data"))
  assertthat::assert_that(d21_2 %in% names(data), msg =  glue::glue("Sorry... :( \n {d21_2} is not in data"))
  assertthat::assert_that(d21_3 %in% names(data), msg =  glue::glue("Sorry... :( \n {d21_3} is not in data"))
  #assertthat::assert_that(d260 %in% names(data), msg =  glue::glue("Sorry... :( \n {d260} is not in data"))
  assertthat::assert_that(ht19 %in% names(data), msg =  glue::glue("Sorry... :( \n {ht19} is not in data"))
  assertthat::assert_that(pobre06 %in% names(data), msg =  glue::glue("Sorry... :( \n {pobre06} is not in data"))
  assertthat::assert_that(anio %in% names(data), msg =  glue::glue("Sorry... :( \n {anio} is not in data"))
  assertthat::assert_that(school_enrollment %in% names(data), msg = "Sorry... :( \n school_enrollment is not calculated, please run enrolled_school() to obtain the variable.")
  assertthat::assert_that(years_schooling %in% names(data), msg = "Sorry... :( \n years_schooling is not calculated, please run years_of_schooling() to obtain the variable.")

  yy <- max(as.numeric(data$anio))
  if (yy == 2011) {
    data <- data %>%
      dplyr::group_by(numero) %>%
      dplyr::mutate(
        UBN_housing = ifelse((c2 == 6 | c3 == 6 | c4 == 5) | (ht19 / d9) > 2 | d19 == 3, 1, 0),
        UBN_water = ifelse(d12 %in% 2:4 | d11 %in% 2:6, 1, 0),
        UBN_sewerage = ifelse(d13 == 3 | d14 == 0 | d15 == 2 | d16 %in% 3:4, 1, 0),
        UBN_electricity = ifelse(d18 > 2, 1, 0),
        UBN_confort = ifelse(d21_3 == 2 | (d21_1 == 2 & d21_2 == 2), 1, 0)) %>%
      dplyr::ungroup()
    data <- data %>%
      dplyr::mutate(UBN_education = ifelse(e27 %in% 4:17 & school_enrollment == 0 & years_schooling < 12, 1, 0)) %>%
      dplyr::group_by(numero) %>%
      dplyr::mutate(UBN_education = max(UBN_education),
                    UBN_q = UBN_housing + UBN_water + UBN_sewerage + UBN_electricity + UBN_confort + UBN_education,
                    UBN = dplyr::case_when(
                      UBN_q == 0 ~ 0,
                      UBN_q == 1 ~ 1,
                      UBN_q == 2 ~ 2,
                      UBN_q >= 3 ~ 3),
                    UBN = haven::labelled(UBN, labels = c("Sin NBI" = 0, "Con 1 NBI" = 1, "Con 2 NBI" = 2, "Con 3 o mas NBI" = 3), label = "NBI")
      ) %>%
      dplyr::ungroup()

    message(glue::glue("El objeto data es de 2011, en UBN_confort no se incluye fuente para calefaccionar (d260). En NBI educacion no se incluye educacion inicial (e238)."))
    message("Variables have been created: \n \t UBN_housing (NBI vivienda);
            UBN_water (NBI acceso al agua);
            UBN_sewerage (NBI saneamiento);
            UBN_electricity (NBI electricidad);
            UBN_confort (NBI confort);
            UBN_education (NBI educacion);
            UBN_q (suma de NBIs) &
            UBN (Categoria NBI)")
  } else if (yy >= 2012 & yy <= 2013) {
    data <- data %>%
      dplyr::group_by(numero) %>%
      dplyr::mutate(
        UBN_housing = ifelse((c2 == 6 | c3 == 6 | c4 == 5) | (ht19 / d9) > 2 | d19 == 3, 1, 0),
        UBN_water = ifelse(d12 %in% 2:4 | d11 %in% 2:6, 1, 0),
        UBN_sewerage = ifelse(d13 == 3 | d14 == 0 | d15 == 2 | d16 %in% 3:4, 1, 0),
        UBN_electricity = ifelse(d18 > 2, 1, 0),
        UBN_confort = ifelse(d21_3 == 2 | (d21_1 == 2 & d21_2 == 2), 1, 0)) %>%
      dplyr::ungroup()
    data <- data %>%
      dplyr::mutate(UBN_education = ifelse(e27 %in% 4:17 & (school_enrollment == 0 | e238 == 2) & years_schooling < 12, 1, 0)) %>%
      dplyr::group_by(numero) %>%
      dplyr::mutate(UBN_education = max(UBN_education),
        UBN_q = UBN_housing + UBN_water + UBN_sewerage + UBN_electricity + UBN_confort + UBN_education,
        UBN = dplyr::case_when(
          UBN_q == 0 ~ 0,
          UBN_q == 1 ~ 1,
          UBN_q == 2 ~ 2,
          UBN_q >= 3 ~ 3),
        UBN = haven::labelled(UBN, labels = c("Sin NBI" = 0, "Con 1 NBI" = 1, "Con 2 NBI" = 2, "Con 3 o mas NBI" = 3), label = "NBI")
      ) %>%
        dplyr::ungroup()

    message(glue::glue("El objeto data es previo a 2014 en UBN_confort no se incluye fuente para calefaccionar (d260)"))
    message("Variables have been created: \n \t UBN_housing (NBI vivienda);
            UBN_water (NBI acceso al agua);
            UBN_sewerage (NBI saneamiento);
            UBN_electricity (NBI electricidad);
            UBN_confort (NBI confort);
            UBN_education (NBI educacion);
            UBN_q (suma de NBIs) &
            UBN (Categoria NBI)")
  } else {
    data <- data %>%
      dplyr::group_by(numero) %>%
      dplyr::mutate(
        UBN_housing = ifelse((c2 == 6 | c3 == 6 | c4 == 5) | (ht19 / d9) > 2 | d19 == 3, 1, 0),
        UBN_water = ifelse(d12 %in% 2:4 | d11 %in% c(2, 4:6), 1, 0),
        UBN_sewerage = ifelse(d13 == 3 | d14 == 0 | d15 == 2 | d16 %in% 3:4, 1, 0),
        UBN_electricity = ifelse(d18 > 2, 1, 0),
        UBN_confort = ifelse(d260 == 6 | d21_3 == 2 | (d21_1 == 2 & d21_2 == 2), 1, 0)) %>%
        dplyr::ungroup()
    data <- data %>%
        dplyr::mutate(UBN_education = ifelse(e27 %in% 4:17 & (school_enrollment == 0 | e238 == 2) &
                                               years_schooling < 12, 1, 0)) %>%
        dplyr::group_by(numero) %>%
        dplyr::mutate(UBN_education = max(UBN_education),
                      UBN_q = UBN_housing + UBN_water + UBN_sewerage + UBN_electricity + UBN_confort + UBN_education,
                      UBN = dplyr::case_when(
                        UBN_q == 0 ~ 0,
                        UBN_q == 1 ~ 1,
                        UBN_q == 2 ~ 2,
                        UBN_q >= 3 ~ 3),
                      UBN = haven::labelled(UBN, labels = c("Sin NBI" = 0, "Con 1 NBI" = 1, "Con 2 NBI" = 2, "Con 3 o mas NBI" = 3), label = "NBI")
      ) %>%
      dplyr::ungroup()

    message("Variables have been created: \n \t UBN_housing (NBI vivienda);
            UBN_water (NBI acceso al agua);
            UBN_sewerage (NBI saneamiento);
            UBN_electricity (NBI electricidad);
            UBN_confort (NBI confort);
            UBN_education (NBI educacion);
            UBN_q (suma de NBIs) &
            UBN (Categoria NBI)")
  }

 return(data)
}

#' This function allows you to calculate poor and indigent people or household
#' @family poverty
#' @param data data.frame
#' @param scale equivalency scale
#' @param region_4 Variable name of region. Default: region_4
#' @param dpto Variable name of departamento. Default: dpto
#' @param ht11 Variable name of income. Default: ht11
#' @param ht19 Variable name of number of individuals in the household. Default: ht19
#' @param numero household id
#' @return data.frame
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#' @examples
#' toy_ech_2018 <- poverty(data = ech::toy_ech_2018)

poverty <- function(data = ech::toy_ech_2018,
                    scale = 0.8,
                    region_4 = "region_4",
                    dpto = "dpto",
                    ht11 = "ht11",
                    ht19 = "ht19",
                    numero = "numero"){

  # checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(region_4  %in% names(data), msg =  glue::glue("Sorry... :( \n {region_4} is not in data"))
  assertthat::assert_that(dpto  %in% names(data), msg =  glue::glue("Sorry... :( \n {dpto} is not in data"))
  assertthat::assert_that(ht11  %in% names(data), msg =  glue::glue("Sorry... :( \n {ht11} is not in data"))
  assertthat::assert_that(ht19  %in% names(data), msg =  glue::glue("Sorry... :( \n {ht19} is not in data"))
  assertthat::assert_that(numero  %in% names(data), msg =  glue::glue("Sorry... :( \n {numero} is not in data"))

  yy <- max(as.numeric(data$anio))
  m <- basket_goods(data = ech::cba_cbna_mdeo, year = yy) %>% dplyr::mutate(mm = 1:12) %>% dplyr::select(-fecha, -cbt_lp)
  i <- basket_goods(data = ech::cba_cbna_int, year = yy) %>% dplyr::mutate(mm = 1:12) %>% dplyr::select(-fecha, -cbt_lp)
  r <- basket_goods(data = ech::cba_cbna_rur, year = yy) %>% dplyr::mutate(mm = 1:12) %>% dplyr::select(-fecha, -cbt_lp)

  h <- data %>%
    dplyr::filter(duplicated(numero) == FALSE) %>%
    dplyr::select(numero, mes, dpto, region_4, ht11, ht19) %>%
    dplyr::mutate(mm = as.integer(haven::zap_labels(mes))) %>%
    dplyr::left_join(., m, by = c("mm")) %>%
    dplyr::rename(cba_m = cba_li, cbna_m = cbna) %>%
    dplyr::left_join(., i, by = c("mm")) %>%
    dplyr::rename(cba_i = cba_li, cbna_i = cbna) %>%
    dplyr::left_join(., r, by = c("mm")) %>%
    dplyr::rename(cba_r = cba_li, cbna_r = cbna) %>%
    dplyr::mutate(
      cba = dplyr::case_when(
        dpto == 1 ~ cba_m,
        dpto != 1 & region_4 != 4 ~ cba_i,
        region_4 == 4 ~ cba_r),
      cbna = dplyr::case_when(
        dpto == 1 ~ cbna_m,
        dpto != 1 & region_4 != 4 ~ cbna_i,
        region_4 == 4 ~ cbna_r))  %>%
    dplyr::select(-mm:-cbna_r) %>%
    dplyr::mutate(cba = round(cba),
                  cbna = round(cbna))

  h <- h %>% dplyr::mutate(
    indigency_line = cba * ht19,
    poverty_line =  indigency_line + cbna * (ht19 ^ scale),
    indigent = ifelse(ht11 <= indigency_line, 1, 0),
    indigent = haven::labelled(indigent, labels = c("Indigente" = 1, "No indigente" = 0),
                               label = "Indigente"),
    poor = ifelse(ht11 <= poverty_line, 1, 0),
    poor = haven::labelled(poor, labels = c("Pobre" = 1, "No pobre" = 0),
                           label = "Pobre")
  )

  data <- h %>% dplyr::select(numero, indigency_line, poverty_line, poor, indigent) %>%
    dplyr:: left_join(data, ., by = "numero")

  message("Variables have been created: \n \t poor (pobre) &
              indigent (indigente)")
  return(data)
}

#' This function allows you to calculate an integrated poverty measure
#' @family poverty
#' @param data data.frame
#' @param pobre06 Variable name of poverty
#' @param UBN_q Variable name of UBN
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \donttest{
#' toy_ech_18 <- enrolled_school(data = ech::toy_ech_2018)
#' toy_ech_18 <- years_of_schooling(toy_ech_18)
#' toy_ech_18 <- unsatisfied_basic_needs(toy_ech_18)
#' toy_ech_18 <- integrated_poverty_measure(data = toy_ech_18)
#' }

integrated_poverty_measure <- function(data = ech::toy_ech_2018,
                                       pobre06 = "pobre06",
                                       UBN_q = "UBN_q"){

  # checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(pobre06 %in% names(data), msg = glue:glue("Sorry... :( \n {pobre06} is not in data"))
  assertthat::assert_that(UBN_q %in% names(data), msg = glue:glue("Sorry... :( \n {UBN_q} is not calculated, please run unsatisfied_basic_needs() to obtain the variable."))

  data <- data %>%
    dplyr::mutate(integrated_poverty_measure = dplyr::case_when(
      pobre06 == 0 & UBN_q == 0 ~ 0,
      pobre06 == 1 & UBN_q == 0 ~ 1,
      pobre06 == 0 & UBN_q >= 1 ~ 2,
      pobre06 == 1 & UBN_q >= 1 ~ 3),
      integrated_poverty_measure = haven::labelled(integrated_poverty_measure,
                                                   labels = c("No pobreza" = 0, "Pobreza reciente" = 1,
                                                              "Pobreza inercial" = 2, "Pobreza cronica" = 3),
                                                   label = "Pobreza integrada")
    )
  message(" \t integrated_poverty_measure (Pobreza integrada)")
  return(data)
}
