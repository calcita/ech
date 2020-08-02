#' unsatisfied_basic_needs
#'
#' @param data data.frame
#' @param ipm Variable name of
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
#' @param enrollment Variable name of enrollment
#' @param years_schooling Variable name of years_schooling
#'
#' @return data.frame
#' @export
#' @importFrom dplyr mutate case_when
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' toy_ech_18 <- enrolled_school(data = ech::toy_ech_2018)
#' toy_ech_18 <- years_of_schooling(toy_ech_18)
#' toy_ech_18 <- unsatisfied_basic_needs(toy_ech_18)
#'
unsatisfied_basic_needs <- function(data = ech::toy_ech_2018,
                                    ipm = FALSE,
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
                                    enrollment = "enrollment",
                                    years_schooling = "years_schooling"){

  assertthat::assert_that(enrollment %in% names(data), msg = "Sorry... :( \n enrollment is not calculated, please run enrolled_school() to obtain the variable.")
  assertthat::assert_that(years_schooling %in% names(data), msg = "Sorry... :( \n years_schooling is not calculated, please run years_of_schooling() to obtain the variable.")

  data <- data %>%
    dplyr::mutate(
      UBN_housing = ifelse((c2 == 6 | c3 == 6 | c4 == 5) | (ht19 / d9) >2 | d19 == 3, 1, 0),
      UBN_water = ifelse(d12 %in% 2:4 | d11 %in% 2:6, 1, 0),
      UBN_sewerage = ifelse(d13 == 3 | d14 == 0 | d15 == 2 | d16 %in% 3:4, 1, 0),
      UBN_electricity = ifelse(d18 > 2, 1, 0),
      UBN_confort = ifelse(d260 == 6 | d21_3 == 2 | (d21_1 == 2 & d21_2 == 2), 1, 0),
      UBN_education = ifelse(.data$enrollment == 0 & .data$years_schooling < 12, 1, 0),
      UBN_q = sum(dplyr::c_across(UBN_housing:UBN_education)),
      UBN = dplyr::case_when(
        UBN_q == 0 ~ "Sin NBI",
        UBN_q == 1 ~ "Con 1 NBI",
        UBN_q == 2 ~ "Con 2 NBI",
        UBN_q >= 3 ~ "Con 3 o mas NBI")
    )

  if (ipm == T){
    data <- data %>%
      dplyr::mutate(integrated_poverty_measure = dplyr::case_when(
        pobre06 == 0 & UBN_q == 0 ~ "No pobreza",
        pobre06 == 1 & UBN_q == 0 ~ "Pobreza reciente",
        pobre06 == 0 & UBN_q >= 1 ~ "Pobreza inercial",
        pobre06 == 1 & UBN_q >= 1 ~ "Pobreza cronica"
      ))
  }

    return(data)
}

#' poverty
#'
#' @param data data.frame
#' @param scale equivalency scale
#' @param region_4 Variable name of region. Default: region_4
#' @param dpto Variable name of departamento. Default: dpto
#' @param ht11 Variable name of income. Default: ht11
#' @param ht19 Variable name of number of individuals in the household. Default: ht19
#'
#' @return data.frame
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' toy_ech_2018 <- poverty(data = ech::toy_ech_2018)
#'
poverty <- function(data = ech::toy_ech_2018,
                    scale = 0.8,
                    region_4 = "region_4",
                    dpto = "dpto",
                    ht11 = "ht11",
                    ht19 = "ht19"){

  yy <- max(as.numeric(data$anio))
  m <- basket_goods(data = ech::cba_cbna_mdeo, year = yy) %>% dplyr::mutate(mm = 1:12) %>% select(-fecha, -cbt_lp)
  i <- basket_goods(data = ech::cba_cbna_int, year = yy) %>% dplyr::mutate(mm = 1:12) %>% select(-fecha, -cbt_lp)
  r <- basket_goods(data = ech::cba_cbna_rur, year = yy) %>% dplyr::mutate(mm = 1:12) %>% select(-fecha, -cbt_lp)

  data <- data %>%
    dplyr::mutate(mm = as.integer(haven::zap_labels(data$mes))) %>%
    dplyr::left_join(., m, by = c("mm")) %>%
    rename(cba_m = cba_li, cbna_m = cbna) %>%
    dplyr::left_join(., i, by = c("mm")) %>%
    rename(cba_i = cba_li, cbna_i = cbna) %>%
    dplyr::left_join(., m, by = c("mm")) %>%
    rename(cba_r = cba_li, cbna_r = cbna) %>%
    dplyr::mutate(
      cba = dplyr::case_when(
        dpto == 1 ~ cba_m,
        dpto != 1 & region_4 != 4 ~ cba_i,
        region_4 == 4 ~ cba_r),
      cbna = dplyr::case_when(
        dpto == 1 ~ cbna_m,
        dpto != 1 & region_4 != 4 ~ cbna_i,
        region_4 == 4 ~ cbna_r),
      indigency_line = cba * ht19,
      poverty_line =  indigency_line + cbna * (ht19 ^ scale),
      indigent = ifelse(ht11 <= indigency_line, 1, 0),
      poor = ifelse(ht11 <= poverty_line, 1, 0)) %>%
      select(-mm:-cbna_r)
  }
