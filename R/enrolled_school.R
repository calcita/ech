#' A function to estimate people enrolled in school
#'
#' This function allows you to estimate people enrolled in school
#' @param data data.frame with necessary variables Defaults to ech.
#' @keywords education
#' @export
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' enrolled_school(data = ech::toy_ech_2018)
#' }

enrolled_school <- function(data = ech::toy_ech_2018){
  if (exists("edu_asist", data)) warning('enrolled_school pre-existing')
  data %<>% dplyr::mutate(edu_asist = dplyr::case_when((.data$e193 == 1 | # "Asiste actualmente"
                                                          .data$e197 ==  1|
                                                          .data$e201 == 1 |
                                                          .data$e212 == 1 |
                                                          .data$e215 == 1 |
                                                          .data$e218 == 1 |
                                                          .data$e221 == 1 |
                                                          .data$e224 == 1) ~ 1,
                                                       TRUE ~ 0))
}
