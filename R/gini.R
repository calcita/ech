#' gini
#'
#' @param data data frame with ECH microdata
#' @param df_year year of the ech data.frame
#' @importFrom dplyr mutate
#' @importFrom convey svygini
#' @export

gini <- function(data = ech::toy_ech_2018,
                 df_year = 2018) {

  data <- data %>% mutate(deflactor_gini_i =  deflate_gini(base_month = "01",
                                                           base_year = "2005",
                                                           ipc = "I",
                                                           df_year = 2018),
                          deflactor_gini_m =  deflate_gini(base_month = "01",
                                                           base_year = "2005",
                                                           ipc = "M",
                                                           df_year = 2018),
                          deflactor_gini = ifelse(dpto == 1, deflactor_gini_m, deflactor_gini_i),
                          ht11_svl_per_capita_deflate_gini = ysvl / ht19 * deflactor_gini)
}
