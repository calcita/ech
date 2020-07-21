#' \code{ech} package
#'
#' Caja de Herramientas para el procesamiento de la Encuesta Continua de Hogares de Uruguay
#'
#' See the README on
#' \href{https://github.com/calcita/ech/blob/master/README.md}{Github}
#'
#' @docType package
#' @name ech
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c(".",
                                                       "var17",
                                                       "unidad",
                                                       "fecha",
                                                       "indice",
                                                       "yy",
                                                       "mm",
                                                       "dd",
                                                       "dpto",
                                                       "deflactor_gini_m",
                                                       "deflactor_gini_i",
                                                       "ysvl",
                                                       "ht19",
                                                       "deflactor_gini",
                                                       "ht11_svl_per_capita_deflate_gini",
                                                       "d14",
                                                       "overcrowding",
                                                       "bathroom",
                                                       "rooms",
                                                       "roof_materials",
                                                       "wall_materials",
                                                       "floor_materials",
                                                       "water",
                                                       "running_water",
                                                       "drainage",
                                                       "electricity",
                                                       'housing_deprivation_q',
                                                       'main_work',
                                                       'second_work',
                                                       'self_employment',
                                                       'labor_income',
                                                       'labor_income_h',
                                                       'mes',
                                                       'hours_per_month'
                                                       ))
