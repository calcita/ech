#' household_type
#' This function allows you to calculate the household type for each household in the survey. A household is composed of one or more people who occupy a housing unit.
#'
#' @param data data frame with ECH microdata
#' @param e26 Variable name of sex
#' @param e27 Variable name of age
#' @param e30 Variable name of householder
#'
#' @importFrom dplyr mutate group_by select
#' @importFrom glue glue
#' @importFrom rlang .data
#' @keywords household_type
#' @export
#' @return data.frame
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' \donttest{
#' toy_ech_2018 <- household_type(data = ech::toy_ech_2018)
#' }

household_type <- function(data = ech::toy_ech_2018,
                           e26 = "e26",
                           e27 = "e27",
                           e30 = "e30") {

  # checks ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(e26  %in% names(data), msg =  glue::glue("Sorry... :( \n {e26} is not in data"))
  assertthat::assert_that(e27  %in% names(data), msg =  glue::glue("Sorry... :( \n {e27} is not in data"))
  assertthat::assert_that(e30  %in% names(data), msg =  glue::glue("Sorry... :( \n {e30} is not in data"))

  # if ("household_type" %in% names(data)) {
  #   message(glue::glue("The data.frame already contains a variable with the name household_type, it will be overwritten"))
  # }

  data <- data %>%
    dplyr::mutate(sex_householder = ifelse(.data[[e26]] == 1 & .data[[e30]] == 1,1, # 1 is man and householder
                                        ifelse(.data[[e26]] == 2 & .data[[e30]] == 1, 2, 0)), #0 is woman householder
                 partner = ifelse(.data[[e30]] == 2, 1, 0),
                 child = ifelse(.data[[e30]] %in% 3:5, 1, 0),
                 child_law = ifelse(.data[[e30]] == 6, 1, 0),
                 under_18 = ifelse(.data[[e27]] < 18, 1, 0),
                 parents_brosis = ifelse(.data[[e30]] %in% 7:10, 1, 0),
                 grandchild = ifelse(.data[[e30]] == 11, 1, 0),
                 other_rel = ifelse(.data[[e30]] == 12, 1, 0),
                 no_rel = ifelse(.data[[e30]] == 13, 1, 0)) %>%
    dplyr::group_by(.data$numero) %>%
    dplyr::mutate(sex_householder = max(.data$sex_householder),
           under_18 = max(.data$under_18),
           partner = max(.data$partner),
           child = max(.data$child),
           child_law = max(.data$child_law),
           parents_brosis = max(.data$parents_brosis),
           grandchild = max(.data$grandchild),
           other_rel = max(.data$other_rel),
           no_rel = max(.data$no_rel),
           household_type = ifelse(sum(.data$partner, .data$child, .data$parents_brosis, .data$grandchild, .data$child_law, .data$other_rel, .data$no_rel) == 0, "unipersonal", #Single person
                            ifelse(.data$partner > 0 & sum(.data$child, .data$parents_brosis, .data$grandchild, .data$child_law, .data$other_rel, .data$no_rel) == 0, "pareja",#Couple without children
                            ifelse(.data$partner == 0 & .data$child > 0  & .data$sex_householder == 1 & sum(.data$parents_brosis, .data$grandchild, .data$child_law, .data$other_rel, .data$no_rel) == 0,"monoparental", #Single parent or Single father
                            ifelse(.data$partner == 0 & .data$child > 0 & .data$sex_householder == 2 & sum(.data$parents_brosis, .data$grandchild, .data$child_law, .data$other_rel, .data$no_rel) == 0, "monomarental", #Single parent or Single mother
                            ifelse(.data$partner > 0 & .data$child > 0 & sum(.data$parents_brosis, .data$grandchild, .data$child_law, .data$other_rel, .data$no_rel) == 0, "biparental", #Couple with children
                            ifelse(.data$under_18 == 0 & (.data$parents_brosis > 0 | .data$grandchild > 0 | .data$child_law > 0 | .data$other_rel > 0) & .data$no_rel == 0, "extendido sin menores", #Extended without children
                            ifelse(.data$under_18 == 1 & (.data$parents_brosis > 0 | .data$grandchild > 0 | .data$child_law > 0 | .data$other_rel > 0) & .data$no_rel == 0, "extendido con menores", #Extended with children
                            ifelse(.data$no_rel > 0, "compuesto","error")))))))) # composite) %>%
           )

  data <- data %>% dplyr::select(everything(), -.data$sex_householder:-.data$no_rel)
  message("A variable has been created: \n \t household_type (Tipo de hogar)")
  data
 }
