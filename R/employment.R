#' An employment function
#'
#' This function allows you to calculate the variables: PEA, PET, PO, PD
#' @param data data frame with microdata
#' @param pobpcoac Definition of population by activity status
#' @keywords employment
#' @export
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- employment(data = ech::toy_ech_2018, pobpcoac = "pobpcoac")
#' }

employment <- function(data = ech::toy_ech_2018,
                       pobpcoac = "pobpcoac"){

# checks ---
   if (exists("pea", data)) warning('pea pre-existing')
   if (!exists(pobpcoac, data)) stop("pobpcoac variable name not in data")

# variables ---
    data %<>% dplyr::mutate(pea = ifelse({{pobpcoac}} %in% 2:5, 1, 0),
                            pet = ifelse({{pobpcoac}} != 1, 1, 0),
                            po  = ifelse({{pobpcoac}} == 2, 1, 0),
                            pd  = ifelse({{pobpcoac}} %in% 3:5, 1, 0)
  )
}


#' Title
#'
#' @param data data.frame
#' @param f72_2 ciiu code rev.4
#' @param group logical
#'
#' @return data.frame
#' @export
#'
#' @example
#' \donttest{
#' toy_ech_2018 <- branch_ciiu(data = ech::toy_ech_2018)
#' }

branch_ciiu <- function(data = ech::toy_ech_2018,
                        f72_2 = "f72_2",
                        group = TRUE){

 data <- data %>%  mutate(branch_ciiu = dplyr::case_when(f72_2 == "" ~ NA,
                                        f72_2 < 1000 ~ 1,
                                        f72_2 < 4000 ~ 2,
                                        f72_2 < 4500 ~ 3,
                                        f72_2 < 4900 ~ 4,
                                        f72_2 < 5500 ~ 5,
                                        f72_2 < 5800 ~ 6,
                                        f72_2 < 6400 ~ 7,
                                        f72_2 < 6800 ~ 8,
                                        f72_2 < 6900 ~ 9,
                                        f72_2 < 7500 ~ 10,
                                        f72_2 < 8400 ~ 11,
                                        f72_2 < 8500 ~ 12,
                                        f72_2 < 8600 ~ 13,
                                        f72_2 < 9000 ~ 14,
                                        f72_2 < 9400 ~ 15,
                                        f72_2 < 9700 ~ 16,
                                        f72_2 < 9900 ~ 17,
                                        TRUE ~ 18
    )
  )

 if (group == TRUE){
   data <- data %>%
     mutate(branch_group_ciiu = dplyr::case_when(
       branch_ciiu == "" ~ NA,
       branch_ciiu == 1 ~ 1,
       branch_ciiu == 2 ~ 2,
       branch_ciiu == 3 ~ 3,
       branch_ciiu %in% c(4, 6) ~ 4,
       branch_ciiu == 5 ~ 5,
       branch_ciiu %in% 7:10 ~ 6,
       branch_ciiu == 11 ~ 7,
       branch_ciiu %in% c(12, 18) ~ 8,
       branch_ciiu == 13 ~ 9,
       branch_ciiu == 14 ~ 10,
       branch_ciiu %in% 15:16 ~ 11,
       TRUE ~ 12
     )
   )
 }

}
