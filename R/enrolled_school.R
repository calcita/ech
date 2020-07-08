#' A function to estimate people enrolled in school
#'
#' This function allows you to estimate people enrolled in school
#' @param data data.frame with necessary variables Defaults to ech.
#' @param e193 attendance school
#' @param e197 attendance primary
#' @param e201 attendance secondary
#' @param e212 attendance technical education (non-university)
#' @param e215 attendance magisterio
#' @param e218 attendance university
#' @param e221 attendance tertiary
#' @param e224 attendance postgrade
#' @keywords education
#' @export
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @importFrom glue glue
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- enrolled_school(data = ech::toy_ech_2018)
#' }

enrolled_school <- function(data = ech::toy_ech_2018,
                            e193 = "e193",
                            e197 = "e197",
                            e201 = "e201",
                            e212 = "e212",
                            e215 = "e215",
                            e218 = "e218",
                            e221 = "e221",
                            e224 = "e224") {

# checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(e193  %in% names(data), msg =  glue:glue("Sorry... :( \n {e193} is not in data"))
  assertthat::assert_that(e197  %in% names(data), msg =  glue:glue("Sorry... :( \n {e197} is not in data"))
  assertthat::assert_that(e201  %in% names(data), msg =  glue:glue("Sorry... :( \n {e201} is not in data"))
  assertthat::assert_that(e212  %in% names(data), msg =  glue:glue("Sorry... :( \n {e212} is not in data"))
  assertthat::assert_that(e215  %in% names(data), msg =  glue:glue("Sorry... :( \n {e215} is not in data"))
  assertthat::assert_that(e218  %in% names(data), msg =  glue:glue("Sorry... :( \n {e218} is not in data"))
  assertthat::assert_that(e221  %in% names(data), msg =  glue:glue("Sorry... :( \n {e221} is not in data"))
  assertthat::assert_that(e224  %in% names(data), msg =  glue:glue("Sorry... :( \n {e224} is not in data"))

  if (exists("edu_asist", data)) warning('edu_asist pre-existing')

  data %<>% dplyr::mutate(edu_asist = dplyr::case_when((.data$e193 == 1 |
                                                          .data$e197 ==  1|
                                                          .data$e201 == 1 |
                                                          .data$e212 == 1 |
                                                          .data$e215 == 1 |
                                                          .data$e218 == 1 |
                                                          .data$e221 == 1 |
                                                          .data$e224 == 1) ~ 1,
                                                       TRUE ~ 0))
}


#' A function to calculate the years of schooling
#'
#' @param data data frame
#' @param e193  attendance school
#' @param e51_2 years passed in primary
#' @param e51_3 years passed in special primary
#' @param e51_4 years passed in lower secondary
#' @param e51_5 years passed in upper secondary
#' @param e51_6 years passed in bachillerato tecnologico
#' @param e51_7 years passed in technical education
#' @param e51_7_1 technical education requirements
#' @param e51_8 years passed in magisterio/profesorado
#' @param e51_9 years passed in university or similar
#' @param e51_10 years passed in tertiary (non-university)
#' @param e51_11 years passed in postgrade
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <-years_of_schooling(data = ech::toy_ech_2018)
#' }

years_of_schooling <- function(data = ech::toy_ech_2018,
                               e193 = "e193",
                               e51_2 = "e51_2",
                               e51_3 = "e51_3",
                               e51_4 = "e51_4",
                               e51_5 = "e51_5",
                               e51_6 = "e51_6",
                               e51_7 = "e51_7",
                               e51_7_1 = "e51_7_1",
                               e51_8 = "e51_8",
                               e51_9 = "e51_9",
                               e51_10 = "e51_10",
                               e51_11 = "e51_11"
                               ){

  data %<>%
    dplyr::mutate_at(dplyr::vars({{e51_2}}, {{e51_3}}, {{e51_4}}, {{e51_5}}, {{e51_6}}, {{e51_7}}), list(~ ifelse( . == 9, 0, .))) %>%

    dplyr::mutate(e51_71 = ifelse(e51_7_1 == 1, e51_7, 0), # Enseñanza secundaria completa
                  e51_72 = ifelse(e51_7_1 == 2, e51_7, 0), # Ciclo Básico, liceo o UTU
                  e51_73 = ifelse(e51_7_1 == 3, e51_7, 0), # Enseñanza primaria completa
                  e51_74 = ifelse(e51_7_1 == 4, e51_7, 0)) # Ninguna


  data %<>% dplyr::mutate(anios_edu = dplyr::case_when(e49 == 2 ~ 0, # nunca asistió
                                     e51_11 %in% 1:6 ~ pmax(12 + e51_9 + e51_11, # sec + uni + pos
                                                            12 + e51_8 + e51_11, # sec + mag + pos
                                                            12 + e51_10 + e51_11), # sec + ter + pos
                                     e51_9 %in% 1:8 | e51_10 %in% 1:8 |
                                       e51_8 %in% 1:8 | e51_7_1 == 1 |
                                       (e51_7_1 == 2 & e51_72 > 3) ~ pmax(12 + e51_9, # sec + uni
                                                                          12 + e51_10, # sec + ter
                                                                          12 + e51_8, # sec + mag
                                                                          12 + e51_71, # sec + tec
                                                                          9 + e51_72), # cb + tec
                                     e51_7_1 == 2 | e51_6 %in% 1:3 |
                                       e51_5 %in% 1:3 ~ pmax(9 + e51_72, # cb + tec
                                                             9 + e51_6, # cb + bach tec.
                                                             9 + e51_5, # cb + bach
                                                             6 + e51_73), # cb + tec
                                     e51_4 %in% 1:3 | e51_7_1 == 3 ~ pmax(6 + e51_4, # pri + cb
                                                                          6 + e51_73), # pri + tec
                                     e51_2 %in% 1:6 | e51_7_1 == 4 | e51_3 %in% 1:4 ~ pmax(e51_2, # pri
                                                                                           e51_74, # tec
                                                                                           e51_3), # pri esp
                                     e193 %in% 1:2 ~ 0,
                                     TRUE ~ 0))

  data %<>% dplyr::mutate(anios_edu = dplyr::case_when(anios_edu < 12 & (e51_9 == 9 | e51_8 == 9 |
                                                           e51_10 == 9 | (e51_7 == 9 & e51_7_1 == 1)) ~ 12,
                                               anios_edu > 22 ~ 22,
                                               TRUE ~ anios_edu))
}


