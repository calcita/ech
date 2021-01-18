#' This function allows you to calculate the people enrolled in school
#' @param data data.frame with necessary variables Defaults to ech.
#' @param e27 Variable name of age
#' @param e193 Variable name of attendance school
#' @param e197 Variable name of attendance primary
#' @param e201 Variable name of attendance secondary
#' @param e212 Variable name of attendance technical school (non-university)
#' @param e215 Variable name of attendance magisterio
#' @param e218 Variable name of attendance university
#' @param e221 Variable name of attendance tertiary
#' @param e224 Variable name of attendance postgrade
#' @keywords education
#' @return data.frame
#' @export
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data
#' @importFrom glue glue
#' @family  education
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' \donttest{
#' toy_ech_2018 <- enrolled_school(data = ech::toy_ech_2018)
#' }

enrolled_school <- function(data = ech::toy_ech_2018,
                            e27 = "e27",
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
  assertthat::assert_that(e27  %in% names(data), msg =  glue:glue("Sorry... :( \n {e27} is not in data"))
  assertthat::assert_that(e193  %in% names(data), msg =  glue:glue("Sorry... :( \n {e193} is not in data"))
  assertthat::assert_that(e197  %in% names(data), msg =  glue:glue("Sorry... :( \n {e197} is not in data"))
  assertthat::assert_that(e201  %in% names(data), msg =  glue:glue("Sorry... :( \n {e201} is not in data"))
  assertthat::assert_that(e212  %in% names(data), msg =  glue:glue("Sorry... :( \n {e212} is not in data"))
  assertthat::assert_that(e215  %in% names(data), msg =  glue:glue("Sorry... :( \n {e215} is not in data"))
  assertthat::assert_that(e218  %in% names(data), msg =  glue:glue("Sorry... :( \n {e218} is not in data"))
  assertthat::assert_that(e221  %in% names(data), msg =  glue:glue("Sorry... :( \n {e221} is not in data"))
  assertthat::assert_that(e224  %in% names(data), msg =  glue:glue("Sorry... :( \n {e224} is not in data"))

  # if ("school_enrollment" %in% names(data)) {
  #   message("The data.frame already contains a variable with the name school_enrollment, it will be overwritten")
  # }

  data <- data %>% dplyr::mutate(school_enrollment = dplyr::case_when((.data$e193 == 1 |
                                                                .data$e197 == 1 |
                                                                (.data$e201 == 1 & .data$e27 > 10) |
                                                                .data$e212 == 1 |
                                                                (.data$e215 == 1 & .data$e27 > 17) |
                                                                (.data$e218 == 1 & .data$e27 > 17) |
                                                                (.data$e221 == 1 & .data$e27 > 17) |
                                                                (.data$e224 == 1 & .data$e27 > 17)) ~ 1,
                                                               TRUE ~ 0),
                          school_enrollment = haven::labelled(school_enrollment, labels = c("Si" = 1, "No" = 0),
                                                 label = "Matriculacion escolar"))

  message("A variable has been created: \n \t  school_enrollment (matriculacion escolar)")
  return(data)
}

#' This function allows you to calculate the years of schooling
#' @param data data.frame
#' @param e193  Variable name of attendance school
#' @param e51_2 Variable name of years passed in primary
#' @param e51_3 Variable name of years passed in special primary
#' @param e51_4 Variable name of years passed in lower secondary
#' @param e51_5 Variable name of years passed in upper secondary
#' @param e51_6 Variable name of years passed in bachillerato tecnologico
#' @param e51_7 Variable name of years passed in technical education
#' @param e51_7_1 Variable name of technical education requirements
#' @param e51_8 Variable name of years passed in magisterio/profesorado
#' @param e51_9 Variable name of years passed in university or similar
#' @param e51_10 Variable name of years passed in tertiary (non-university)
#' @param e51_11 Variable name of years passed in postgrade
#' @param max_years Maximum years of schooling
#' @export
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data
#' @importFrom glue glue
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#' @family  education
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
                               e51_11 = "e51_11",
                               max_years = 22){

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

  # if ("years_schooling" %in% names(data)) {
  #   message("The data.frame already contains a variable with the name years_schooling, it will be overwritten")
  # }

  data <- data %>%
    dplyr::mutate_at(dplyr::vars({{e51_2}}, {{e51_3}}, {{e51_4}}, {{e51_5}}, {{e51_6}}, {{e51_7}}, {{e51_8}}, {{e51_9}}, {{e51_10}}, {{e51_11}}), list(~ ifelse( . == 9, 0, .))) %>%

    dplyr::mutate(e51_71 = ifelse(e51_7_1 == 1, e51_7, 0),
                  e51_72 = ifelse(e51_7_1 == 2, e51_7, 0),
                  e51_73 = ifelse(e51_7_1 == 3, e51_7, 0),
                  e51_74 = ifelse(e51_7_1 == 4, e51_7, 0))


  data <- data %>% dplyr::mutate(years_schooling = dplyr::case_when(e49 == 2 ~ 0, # nunca asistió
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

  data <- data %>% dplyr::mutate(years_schooling = dplyr::case_when(years_schooling < 12 & (e51_9 == 9 | e51_8 == 9 |
                                                                           e51_10 == 9 | (e51_7 == 9 & e51_7_1 == 1)) ~ 12,
                                                       TRUE ~ years_schooling))

  if (!is.null(max_years)){
    data <- data %>% dplyr::mutate(years_schooling = dplyr::case_when(years_schooling > max_years ~ max_years,
                                                               TRUE ~ years_schooling))
  }

  message("A variable has been created: \n \t  years_schooling (anios de escolaridad)")
  return(data)
}



#' This function allows you to calculate the highest level of education achieved
#' @param data data.frame
#' @param e51_2 Variable name of years passed in primary
#' @param e51_3 Variable name of years passed in special primary
#' @param e51_4 Variable name of years passed in lower secondary
#' @param e51_5 Variable name of years passed in upper secondary
#' @param e51_6 Variable name of years passed in technical upper secondary
#' @param e51_7 Variable name of years passed in technical school
#' @param e51_7_1 Variable name of technical school requirements
#' @param e51_8 Variable name of years passed in magisterio/profesorado
#' @param e51_9 Variable name of years passed in university or similar
#' @param e51_10 Variable name of years passed in tertiary (non-university)
#' @param e51_11 Variable name of years passed in postgrade
#' @param e193  Variable name of attendance school
#' @param e49 Variable name of attendance school ever
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#' @family  education
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

  # checks ---
  assertthat::assert_that(is.data.frame(data))
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
  assertthat::assert_that(e193  %in% names(data), msg =  glue:glue("Sorry... :( \n {e193} is not in data"))
  assertthat::assert_that(e49  %in% names(data), msg =  glue:glue("Sorry... :( \n {e49} is not in data"))

  # if ("level_education" %in% names(data)) {
  #   message("The data.frame already contains a variable with the name level_education, it will be overwritten")
  # }

  data <- data %>% dplyr::mutate(
    level_education = dplyr::case_when(
      e49 == 2 & e51_2 == 0 & e51_3 == 0 & e51_4 == 0 & e51_5 == 0 & e51_6 == 0 & e51_7 == 0 & e51_8 == 0 & e51_9 == 0 & e51_10 == 0 & e51_11 == 0 ~ 0,
      e51_2 == 9 | e51_3 == 9 | e193 == 1 ~ 0,
      e51_2 == 0 & e51_3 == 0 ~ 0))

  data <- data %>% dplyr::mutate(
    level_education = dplyr::case_when(
      e51_2 %in% 1:6 & e51_4 %in% c(0, 9) ~ 1,
      e51_3 %in% 1:6 & e51_4 %in% c(0, 9) ~ 1,
      e51_7 > 0 & e51_7_1 == 4 & e51_4 %in% c(0, 9) ~ 1,
      TRUE ~ level_education))

  data <- data %>% dplyr::mutate(
    level_education = dplyr::case_when(
      (e51_4 %in% 1:3 | e51_5 %in% 1:3 | e51_6 %in% 1:3) & (e51_8 == 0 & e51_9 == 0 & e51_10 == 0 & e51_11 == 0) ~ 2,
      ((e51_4 == 3 & e51_8 %in% c(0,9)) | (e51_5 == 3 & e51_8 %in% c(0,9)) | (e51_6 == 3 & e51_8 %in% c(0,9))) ~ 2,
      (e51_7 > 0 & e51_7_1 == 3) & (e51_8 %in% c(0,9) & e51_9 %in% c(0,9) & e51_10 %in% c(0, 9)) ~ 2,
      (e51_7 >0 & e51_7_1 < 3) & (e51_8 %in% c(0,9) & e51_9 %in% c(0,9) & e51_10 %in% c(0, 9)) ~ 2,
      TRUE ~ level_education))

  data <- data %>% dplyr::mutate(
    level_education = dplyr::case_when(
      e51_8 %in% 1:4 & e51_9 %in% c(0,9) & e51_10 %in% c(0,9) & e51_11 %in% c(0,9) ~ 3,
      e51_9 %in% 1:8 | e51_10 %in% 1:4 | e51_11 %in% 1:4 ~ 4,
      TRUE ~ level_education))

  data <- data %>% dplyr::mutate(
    level_education = haven::labelled(level_education,
                                        labels = c("Sin instruccion" = 0, "Primaria" = 1, "Secundaria o UTU" = 2, "Magisterio o profesorado" = 3, "Universidad o similar" = 4),
                                        label = "Nivel educativo")
    )
  message("A variable has been created: \n \t highest level_education achieved (maximo nivel educativo alcanzado)")
  return(data)
}



#' This function allows you to calculate the level of school completion
#' @param data data.frame
#' @param e197 Variable name of attends primary school
#' @param e197_1 Variable name of completed primary
#' @param e201 Variable name of attends secondary
#' @param e51_4 Variable name of years passed in lower secondary
#' @param e51_5 Variable name of years passed in upper secondary
#' @param e51_6 Variable name of years passed in technical upper secondary
#' @param e51_7 Variable name of years passed in technical education
#' @param e51_7_1 Variable name of technical education requirements
#' @param e51_8 Variable name of years passed in magisterio/profesorado
#' @param e51_9 Variable name of years passed in university or similar
#' @param e51_10 Variable name of years passed in tertiary (non-university)
#' @param e212 Variable name of attendance technical school (non-university)
#' @param e215 Variable name of attendance magisterio
#' @param e218 Variable name of attendance university
#' @param e221 Variable name of attendance tertiary
#' @param n years of tertiary
#' @importFrom dplyr mutate
#' @return data.frame
#' @export
#' @family  education
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
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
                             e51_6 = "e51_6",
                             e51_7_1 = "e51_7_1",
                             e51_7 = "e51_7",
                             e51_8 = "e51_8",
                             e51_9 = "e51_9",
                             e51_10 = "e51_10",
                             e212 = "e212",
                             e215 = "e215",
                             e218 = "e218",
                             e221 = "e221",
                             n = 4){

  # checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(e197  %in% names(data), msg =  glue:glue("Sorry... :( \n {e197} is not in data"))
  assertthat::assert_that(e197_1  %in% names(data), msg =  glue:glue("Sorry... :( \n {e197_1} is not in data"))
  assertthat::assert_that(e51_4  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_4} is not in data"))
  assertthat::assert_that(e51_5  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_5} is not in data"))
  assertthat::assert_that(e51_6  %in% names(data), msg =  glue:glue("Sorry... :( \n {e51_6} is not in data"))
  assertthat::assert_that(e201  %in% names(data), msg =  glue:glue("Sorry... :( \n {e201} is not in data"))

  # if ("primary_completion" %in% names(data)) {
  #   message("The data.frame already contains a variable with the name primary_completion, it will be overwritten")
  # }
  # if ("lower_secondary_completion" %in% names(data)) {
  #   message("The data.frame already contains a variable with the name lower_secondary_completion, it will be overwritten")
  # }
  # if ("upper_secondary_completion " %in% names(data)) {
  #   message("The data.frame already contains a variable with the name upper_secondary_completion, it will be overwritten")
  # }

  data <- data %>% dplyr::mutate(primary_completion = ifelse(e197 == 2 & e197_1 == 1, 1, 0),
                                 lower_secondary_completion = ifelse(e201 %in% 1:2 & e51_4 == 3, 1, 0),
                                 upper_secondary_completion = ifelse(e201 %in% 1:2 & (e51_5 == 3 | e51_6 == 3), 1, 0),
                                 tertiary_completion = ifelse((e212 %in% 1:2 & e51_7_1 == 1 & e51_7 >= n) | (e215 %in% 1:2 & e51_8 >= n) | (e218 %in% 1:2 & e51_9 >= n) | (e221 %in% 1:2 & e51_10 >= n), 1, 0),
                                 primary_completion = haven::labelled(primary_completion, labels = c("Si" = 1, "No" = 0),
                                                                     label = "Primaria completa"),
                                 lower_secondary_completion = haven::labelled(lower_secondary_completion, labels = c("Si" = 1, "No" = 0),
                                                                     label = "Ciclo basico completo"),
                                 upper_secondary_completion = haven::labelled(upper_secondary_completion, labels = c("Si" = 1, "No" = 0),
                                                                     label = "bachillerato completo"),
                                 tertiary_completion = haven::labelled(tertiary_completion, labels = c("Si" = 1, "No" = 0),
                                                                              label = "terciaria completa")
                                 )
  message("Variables have been created: \n \t primary_completion (primaria completa);
         lower_secondary_completion (ciclo basico completo) &
         upper_secondary_completion (bachillerato completo) &
          tertiary_completion (terciaria completa)")
  return(data)
}
