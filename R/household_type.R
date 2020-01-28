#' A household function
#'
#' This function allows you to calculate the household type for each household in the survey. A household is composed of one or more people who occupy a housing unit.
#' @param data data frame with ECH microdata
#' @param e26 data frame column ('sex')
#' @param e27 data frame column ('age')
#' @param e30 data frame column ('householder')
#' @param colname custom name for the new variable
#' @importFrom dplyr mutate group_by select
#' @importFrom glue glue
#' @importFrom rlang .data
#' @keywords household_type
#' @export
#' @examples
#' household_type(data = ech::toy_ech_2018)
#

household_type <- function(data = ech::toy_ech_2018,
                           e26 = e26,
                           e27 = e27,
                           e30 = e30,
                           colname = "tipo_hogar") {

#   if (colname %in% names(data)) {
#     message(glue::glue("El data frame ya contiene una variable con ese nombre, se sobreescribira"))
#   }
#
#   data <- data %>%
#     dplyr::mutate(sex_householder = ifelse(e26 == 1 & e30 == 1,1, # 1 is man and householder
#                                         ifelse(e26 == 2 & e30 == 1,2,0)), #0 is woman householder
#                  partner = ifelse(e30 == 2, 1, 0),
#                  child = ifelse(e30 %in% 3:5, 1, 0),
#                  child_law = ifelse(e30 == 6, 1, 0),
#                  under_18 = ifelse(e27 < 18, 1, 0),
#                  parents_brosis = ifelse(e30 %in% 7:10, 1, 0),
#                  grandchild = ifelse(e30 == 11, 1, 0),
#                  other_rel = ifelse(e30 == 12, 1, 0),
#                  no_rel = ifelse(e30 == 13, 1, 0)) %>%
#     dplyr::group_by(numero) %>%
#     dplyr::mutate(sex_householder = max(sex_householder),
#            under_18 = max(under_18),
#            partner = max(partner),
#            child = max(child),
#            child_law = max(child_law),
#            parents_brosis = max(parents_brosis),
#            grandchild = max(grandchild),
#            other_rel = max(other_rel),
#            no_rel = max(no_rel),
#            household_type = ifelse(sum(partner, child, parents_brosis, grandchild, child_law, other_rel, no_rel) == 0, "unipersonal", #Single person
#                             ifelse(partner > 0 & sum(child, parents_brosis, grandchild, child_law, other_rel, no_rel) == 0, "pareja",#Couple without children
#                             ifelse(partner == 0 & child > 0  & sex_householder == 1 & sum(parents_brosis, grandchild, child_law, other_rel, no_rel) == 0,"monoparental", #Single parent or Single father
#                             ifelse(partner == 0 & child > 0 & sex_householder == 2 & sum(parents_brosis, grandchild, child_law, other_rel, no_rel) == 0, "monomarental", #Single parent or Single mother
#                             ifelse(partner > 0 & child > 0 & sum(parents_brosis, grandchild, child_law, other_rel, no_rel) == 0, "biparental", #Couple with children
#                             ifelse(under_18 == 0 & (parents_brosis > 0 | grandchild > 0 | child_law > 0 | other_rel > 0) & no_rel == 0, "extendido sin menores", #Extended without children
#                             ifelse(under_18 == 1 & (parents_brosis > 0 | grandchild > 0 | child_law > 0 | other_rel > 0) & no_rel == 0, "extendido con menores", #Extended with children
#                             ifelse(no_rel > 0, "compuesto","error")))))))) # composite) %>%
#            )
#
#   data <- data %>% dplyr::select(everything(), -sex_householder:-no_rel)
#   message(glue::glue("Se ha creado la variable {{colname}} en {{data}}"))
 }
