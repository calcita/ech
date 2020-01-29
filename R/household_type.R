#' A household function
#'
#' This function allows you to calculate the household type for each household in the survey. A household is composed of one or more people who occupy a housing unit.
#' @param data data frame with ECH microdata
#' @param e26 data frame column ('sex')
#' @param e27 data frame column ('age')
#' @param e30 data frame column ('householder')
#' @param colname custom name for the new variable
#' @importFrom dplyr mutate group_by select enquo
#' @importFrom glue glue
#' @importFrom rlang .data sym quo_name
#' @keywords household_type
#' @export
#' @examples
#' \donttest{
#' household_type(data = ech::toy_ech_2018)
#' }

household_type <- function(data = ech::toy_ech_2018,
                           e26 = "e26",
                           e27 = "e27",
                           e30 = "e30",
                           colname = "tipo_hogar") {

  e26 = quo_name(enquo(e26))
  e27 = quo_name(enquo(e27))
  e30 = quo_name(enquo(e30))
  if (colname %in% names(data)) {
    message(glue::glue("El data frame ya contiene una variable con ese nombre, se sobreescribira"))
  }
  colname = quo_name(enquo(colname))
  data <- data %>%
    dplyr::mutate(sex_householder = ifelse(!!sym(e26) == 1 & !!sym(e30) == 1,1, # 1 is man and householder
                                        ifelse(!!sym(e26) == 2 & !!sym(e30) == 1,2,0)), #0 is woman householder
                 partner = ifelse(!!sym(e30) == 2, 1, 0),
                 child = ifelse(!!sym(e30) %in% 3:5, 1, 0),
                 child_law = ifelse(!!sym(e30) == 6, 1, 0),
                 under_18 = ifelse(!!sym(e27) < 18, 1, 0),
                 parents_brosis = ifelse(!!sym(e30) %in% 7:10, 1, 0),
                 grandchild = ifelse(!!sym(e30) == 11, 1, 0),
                 other_rel = ifelse(!!sym(e30) == 12, 1, 0),
                 no_rel = ifelse(!!sym(e30) == 13, 1, 0)) %>%
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
  names(data)[which(names(data) == "household_type")] <- colname
  message(glue::glue("Se ha creado la variable {colname} en la base"))
  return(data)
 }
