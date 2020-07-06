#' A function to estimate people enrolled in school
#'
#' This function allows you to estimate people enrolled in school
#' @param data data.frame with necessary variables Defaults to ech.
#' @param e193 attendance school
#' @param e197 attendance primary
#' @param e201 attendance secondary
#' @param e212 attendance educacion tecnica
#' @param e215 attendance magisterio
#' @param e218 attendance university
#' @param e221 attendance terciario no universitario
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
