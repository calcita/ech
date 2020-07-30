#' unsatisfied_basic_needs
#'
#' @param data data.frame
#' @param ipm Variable name of
#' @param c2 Variable name of
#' @param c3 Variable name of
#' @param c4 Variable name of
#' @param d9 Variable name of
#' @param d11 Variable name of
#' @param d12 Variable name of
#' @param d13 Variable name of
#' @param d14 Variable name of
#' @param d15 Variable name of
#' @param d16 Variable name of
#' @param d18 Variable name of
#' @param d19 Variable name of
#' @param d21_1 Variable name of
#' @param d21_2 Variable name of
#' @param d21_3 Variable name of
#' @param d260 Variable name of
#' @param ht19 Variable name of
#' @param pobre06 Variable name of
#' @param enrollment Variable name of
#' @param years_schooling Variable name of
#'
#' @return data.frame
#' @export
#' @importFrom dplyr mutate case_when
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' toy_ech_2018 <- unsatisfied_basic_needs(data = ech::toy_ech_2018)
#'
unsatisfied_basic_needs <- function(data = ech::toy_ech_2018,
                                    ipm = FALSE,
                                    c2 = c2,
                                    c3 = c3,
                                    c4 = c4,
                                    d9 = d9,
                                    d11 = d11,
                                    d12 = d12,
                                    d13 = d13,
                                    d14 = d14,
                                    d15 = d15,
                                    d16 = d16,
                                    d18 = d18,
                                    d19 = d19,
                                    d21_1 = d21_1,
                                    d21_2 = d21_2,
                                    d21_3 = d21_3,
                                    d260 = d260,
                                    ht19 = ht19,
                                    pobre06 = pobre06,
                                    enrollment = enrollment,
                                    years_schooling = years_schooling){

  # assertthat::assert_that(enrollment %in% names(data), msg = "Sorry... :( \n enrollment is not calculated, please run enrolled_school() to obtain the variable.")
  # assertthat::assert_that(enrollment %in% names(data), msg = "Sorry... :( \n years_schooling is not calculated, please run years_of_schooling() to obtain the variable.")

  data <- data %>%
    dplyr::mutate(
      UBN_housing = ifelse((c2 == 6 | c3 == 6 | c4 == 5) | (ht19 / d9) >2 | d19 == 3, 1, 0),
      UBN_water = ifelse(d12 %in% 2:4 | d11 %in% 2:6, 1, 0),
      UBN_sewerage = ifelse(d13 == 3 | d14 == 0 | d15 == 2 | d16 %in% 3:4, 1, 0),
      UBN_electricity = ifelse(d18 > 2, 1, 0),
      UBN_confort = ifelse(d260 == 6 | d21_3 == 2 | (d21_1 == 2 & d21_2 == 2), 1, 0)#,
      # UBN_education = ifelse(.data$enrollment == 0 & .data$years_schooling < 12, 1, 0),
      # UBN_q = sum(c_across(UBN_housing:UBN_education)),
      # UBN = dplyr::case_when(
      #   UBN_q == 0 ~ "Sin NBI",
      #   UBN_q == 1 ~ "Con 1 NBI",
      #   UBN_q == 2 ~ "Con 2 NBI",
      #   UBN_q >= 3 ~ "Con 3 o mas NBI")
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
}

