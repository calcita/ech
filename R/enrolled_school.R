#' A function to estimate people enrolled in school
#'
#' This function allows you to estimate people enrolled in school
#' @param base Base with necessary variables Defaults to ech.
#' @keywords education
#' @export
#' @import tidyverse survey
#' @examples
#' edu.asist(df)

enrolled_school <- function(data = df, geo.unit = c("uy", "region_3", "region_4", "dpto", "secc"), by = NULL){
    if (exists("edu_asist", df)) warning('enrolled_school pre-existing')

   data %<>% mutate(edu_asist = case_when((e193 == "Asiste actualmente" | # == 1
                                                       e197 == "Asiste actualmente" |
                                                       e201 == "Asiste actualmente" |
                                                       e212 == "Asiste actualmente" |
                                                       e215 == "Asiste actualmente" |
                                                       e218 == "Asiste actualmente" |
                                                       e221 == "Asiste actualmente" |
                                                       e224 == "Asiste actualmente") ~ 1,
                                                    TRUE ~ 0))
}
