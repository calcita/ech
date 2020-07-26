#' A function to calculate people enrolled in school
#'
#' This function allows you to calculate people enrolled in school
#' @param data data.frame with necessary variables Defaults to ech.
#' @param e193 attendance school
#' @param e197 attendance primary
#' @param e201 attendance secondary
#' @param e212 attendance technical school (non-university)
#' @param e215 attendance magisterio
#' @param e218 attendance university
#' @param e221 attendance tertiary
#' @param e224 attendance postgrade
#' @keywords education
#' @return data.frame
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

  data %<>% dplyr::mutate(enrollment = dplyr::case_when((.data$e193 == 1 |
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
#' @export
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @importFrom glue glue
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- years_of_schooling(data = ech::toy_ech_2018)
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
                               e51_11 = "e51_11"){

  # checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(e193  %in% names(data), msg =  glue:glue("Sorry... :( \n {e193} is not in data"))
  assertthat::assert_that(e51_2  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_2} is not in data"))
  assertthat::assert_that(e51_3  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_3} is not in data"))
  assertthat::assert_that(e51_4  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_4} is not in data"))
  assertthat::assert_that(e51_5  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_5} is not in data"))
  assertthat::assert_that(e51_6  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_6} is not in data"))
  assertthat::assert_that(e51_7  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_7} is not in data"))
  assertthat::assert_that(e51_7_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_7_1} is not in data"))
  assertthat::assert_that(e51_8  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_8} is not in data"))
  assertthat::assert_that(e51_9  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_9} is not in data"))
  assertthat::assert_that(e51_10  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_10} is not in data"))
  assertthat::assert_that(e51_11  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_11} is not in data"))

  data %<>%
    dplyr::mutate_at(dplyr::vars({{e51_2}}, {{e51_3}}, {{e51_4}}, {{e51_5}}, {{e51_6}}, {{e51_7}}), list(~ ifelse( . == 9, 0, .))) %>%

    dplyr::mutate(e51_71 = ifelse(e51_7_1 == 1, e51_7, 0),
                  e51_72 = ifelse(e51_7_1 == 2, e51_7, 0),
                  e51_73 = ifelse(e51_7_1 == 3, e51_7, 0),
                  e51_74 = ifelse(e51_7_1 == 4, e51_7, 0))


  data %<>% dplyr::mutate(years_schooling = dplyr::case_when(e49 == 2 ~ 0, # nunca asistió
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

  data %<>% dplyr::mutate(years_schooling = dplyr::case_when(years_schooling < 12 & (e51_9 == 9 | e51_8 == 9 |
                                                                           e51_10 == 9 | (e51_7 == 9 & e51_7_1 == 1)) ~ 12,
                                                       TRUE ~ years_schooling))
}



#' A function to calculate the highest level of education achieved
#'
#' @param data data frame
#' @param e51_2 years passed in primary
#' @param e51_3 years passed in special primary
#' @param e51_4 years passed in lower secondary
#' @param e51_5 years passed in upper secondary
#' @param e51_6 years passed in technical upper secondary
#' @param e51_7 years passed in technical school
#' @param e51_7_1 technical school requirements
#' @param e51_8 years passed in magisterio/profesorado
#' @param e51_9 years passed in university or similar
#' @param e51_10 years passed in tertiary (non-university)
#' @param e51_11 years passed in postgrade
#' @param e193  attendance school
#' @param e49 attendance school ever
#' @export
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- level_education(data = ech::toy_ech_2018)
#' }

level_education <- function(data = ech::toy_ech_2018,
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
                                e51_11 = "e51_11",
                                e193 = "e193",
                                e49 = "e49"){

  data <- data %>% dplyr::mutate(
    level_education = dplyr::case_when(
      e49 == 2 & e51_2 == 0 & e51_3 == 0 & e51_4 == 0 & e51_5 == 0 & e51_6 == 0 & e51_7 == 0 & e51_8 == 0 & e51_9 == 0 & e51_10 == 0 & e51_11 == 0 ~ "Sin instruccion",
      e51_2 == 9 | e51_3 == 9 | e193 == 1 ~ "Sin instruccion",
      e51_2 == 0 & e51_3 == 0 ~ "Sin instruccion",
      e51_2 %in% 1:6 & e51_4 %in% c(0, 9) ~ "Primaria",
      e51_3 %in% 1:6 & e51_4 %in% c(0, 9) ~ "Primaria",
      e51_7 > 0 & e51_7_1 == 4 & e51_4 %in% c(0, 9) ~ "Primaria",
      e51_4 %in% 1:3 | e51_5 %in% 1:3 | (e51_6 > 0 & e51_4 <= 6) & (e51_8 == 0 & e51_9 == 0 & e51_10 == 0 & e51_11 == 0) ~ "Secundaria",
      ((e51_4 == 3 & e51_8 == 9) | (e51_5 == 3 & e51_8 == 9) | (e51_6 == 3 & e51_8 == 9)) ~ "Secundaria",
      e51_7 != 0 & e51_7_1 == 3 ~ "Secundaria",
      ((e51_7 %in% 1:9 & e51_7_1 < 3) | (e51_7 != 0 & e51_7_1 == 3 & e51_4 == 0 & e51_5 == 0 & e51_6 == 0)) & e51_8 == 0 & e51_9 == 0 & e51_10 == 0 ~ "UTU",
      e51_8 %in% 1:5 & e51_9 == 0 & e51_10 == 0 & e51_11 == 0 ~ "Magisterio",
      e51_9 %in% 1:9 | e51_10 %in% 1:9 | e51_11 %in% 1:9 ~ "Universidad",
      TRUE ~ "Error")
    )
}



#' A function to calculate the level of school completion
#'
#' @param data ech
#' @param e197 attends primary school
#' @param e197_1 completed primary
#' @param e201 attends secondary
#' @param e51_4 years passed in lower secondary
#' @param e51_5 years passed in upper secondary
#' @param e51_6 years passed in technical upper secondary
#'
#' @importFrom dplyr mutate
#' @return data.frame
#' @export
#'
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- level_completion(data = ech::toy_ech_2018)
#' }
#'
level_completion <- function(data = ech::toy_ech_2018,
                             e197 = "e197",
                             e197_1 = "e197_1",
                             e201 = "e201",
                             e51_4 = "e51_4",
                             e51_5 = "e51_5",
                             e51_6 = "e51_6"){

 data <- data %>% dplyr::mutate(primary_completion = ifelse(e197 == 2 & e197_1 == 1, 1, 0),
                         lower_secondary_completion = ifelse(e201 %in% 1:2 & e51_4 == 3, 1, 0),
                         upper_secondary_completion = ifelse(e201 %in% 1:2 & (e51_5 == 3 | e51_6 == 3), 1, 0)#,
                         #tertiary_completion =
)

}
