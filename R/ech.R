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
                                                       "deflactor_m",
                                                       "deflactor_i",
                                                       "deflactor_r",
                                                       "deflactor_gini",
                                                       "ht19",
                                                       "ysvl",
                                                       "y_pc_d",
                                                       "rv_d",
                                                       "y_wrv_d",
                                                       "y_wrv_pc_d",
                                                       "deflactor_r",
                                                       "y_wrv_pc_d_r",
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
                                                       'hours_per_month',
                                                       'housing_situation',
                                                       'overcrowding',
                                                       "homeownership",
                                                       "housing_conditions"
                                                       ))
