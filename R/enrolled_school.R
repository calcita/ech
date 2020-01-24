#' A function to estimate people enrolled in school
#'
#' This function allows you to estimate people enrolled in school
#' @param data data.frame with necessary variables Defaults to ech.
#' @keywords education
#' @export
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %<>%
#' @examples
#' enrolled_school(data = ech::toy_ech_2018)

enrolled_school <- function(data = ech::toy_ech_2018){
  #if (exists("edu_asist", data)) warning('enrolled_school pre-existing')
  # data %<>% dplyr::mutate(edu_asist = dplyr::case_when((e193 == "Asiste actualmente" | # == 1
  #                                                         e197 == "Asiste actualmente" |
  #                                                         e201 == "Asiste actualmente" |
  #                                                         e212 == "Asiste actualmente" |
  #                                                         e215 == "Asiste actualmente" |
  #                                                         e218 == "Asiste actualmente" |
  #                                                         e221 == "Asiste actualmente" |
  #                                                         e224 == "Asiste actualmente") ~ 1,
  #                                                      TRUE ~ 0))
}
