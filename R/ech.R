#' \code{ech} package
#'
#' Caja de Herramientas para el procesamiento de la Encuesta Continua de Hogares de Uruguay
#'
#' See the README on
#' \href{https://github.com/calcita/ech/blob/master/README.md}{Github}
#'
#' @docType package
#' @name eph
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c(".",
                                                       "var17",
                                                       "unidad"))
