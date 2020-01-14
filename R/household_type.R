#' A household function
#'
#' This function allows you to calculate the household type for each household in the survey. A household is composed of one or more people who occupy a housing unit.
#' @param
#' @keywords household_type
#' @export
#' @import tidyverse srvyr
#' @examples
#'
#

household_type <- function(data = df,
                           weights = "pesoano",
                           strata = "estred13") {

  #data <- data %>% mutate_at(vars(e30,e26), unclass(.))

  data <- data %>% dplyr::mutate(sex_householder = ifelse(e26 == 1 & e30 == 1,1, # 1 is man and householder
                                                   ifelse(e26 == 2 & e30 == 1,2,0)), #0 is woman householder
                                   partner = ifelse(e30 == 2, 1, 0),
                                   child = ifelse(e30 %in% 3:5, 1, 0),
                                   child_law = ifelse(e30 == 6, 1, 0),
                                   under_18 = ifelse(e27 < 18, 1, 0),
                                   parents_brosis = ifelse(e30 %in% 7:10, 1, 0),
                                   grandchild = ifelse(e30 == 11, 1, 0),
                                   other_rel = ifelse(e30 == 12, 1, 0),
                                   no_rel = ifelse(e30 == 13, 1, 0),)

  data_h <- data %>% group_by(numero) %>%
    mutate(sex_householder = max(sex_householder),
           under_18 = max(under_18),
           partner = max(partner),
           child = max(child),
           child_law = max(child_law),
           parents_brosis = max(parents_brosis),
           grandchild = max(grandchild),
           other_rel = max(other_rel),
           no_rel = max(no_rel)) %>%
    filter(e30==1)

  data_h <- data_h %>% mutate(household_type = ifelse(sum(partner, child, parents_brosis, grandchild, child_law, other_rel, no_rel) == 0, "unipersonal", #Single person
                                               ifelse(partner > 0 & sum(child, parents_brosis, grandchild, child_law, other_rel, no_rel) == 0, "pareja",#Couple without children
                                                      ifelse(partner == 0 & child > 0  & sex_householder == 1 & sum(parents_brosis, grandchild, child_law, other_rel, no_rel) == 0,"monoparental", #Single parent or Single father
                                                             ifelse(partner == 0 & child > 0 & sex_householder == 2 & sum(parents_brosis, grandchild, child_law, other_rel, no_rel) == 0, "monomarental", #Single parent or Single mother
                                                                    ifelse(partner > 0 & child > 0 & sum(parents_brosis, grandchild, child_law, other_rel, no_rel) == 0, "biparental", #Couple with children
                                                                           ifelse(under_18 == 0 & (parents_brosis > 0 | grandchild > 0 | child_law > 0 | other_rel > 0) & no_rel == 0, "extendido sin menores", #Extended without children
                                                                                  ifelse(under_18 == 1 & (parents_brosis > 0 | grandchild > 0 | child_law > 0 | other_rel > 0) & no_rel == 0, "extendido con menores", #Extended with children
                                                                                         ifelse(no_rel > 0, "compuesto","error")))))))) # composite)
  )
  # Estimación Total País
  est_total <- data_h %>%
    srvyr::as_survey_design(ids = numero, strata = strata, weights = weights) %>%
    srvyr::group_by(household_type) %>%
    srvyr::summarise(tipo_hogar = srvyr::survey_total(vartype = "ci"))

  return(est_total)
}
