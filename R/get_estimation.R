#' A function to estimate variables at universe level
#'
#' This function allows you to estimate variable.
#' @param data data frame with ECH microdata
#' @param variable data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference
#' @param level is household or individual
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @keywords inference
#' @export
#' @import srvyr
#' @return table
#' @examples
#' \donttest{
#' get_estimation_mean(data = ech::toy_ech_2018)
#' }
#

get_estimation_mean <- function(data = ech::toy_ech_2018,
                           variable = NULL,
                           by.x = NULL,
                           by.y = NULL,
                           domain = NULL,
                           level = NULL){

  assertthat::assert_that(is.null(data) | is.null(variable), msg = "Debe indicar la variable")
  assertthat::assert_that(all(variable %in% names(data)))

 #  message(glue:glue("Debe indicar una variable a estimar"))
 # checks ---

 # estimation ---
 #  if(is.character(variable) & nchar(variable)==2 | is.numeric(variable)){
 #
 #    if(is.null(by.x) & is.null(by.y) & is.null(domain)){
 #    estimation <- .ech.pkg.env$design %>%
 #      srvyr::summarise(colname = srvyr::survey_mean(variable))
 #     } else if(is.null(by.x) & is.null(by.y) & is.character(domain)){
 #       estimation <- .ech.pkg.env$design %>%
 #         srvyr::filter(domain) %>%
 #         srvyr::summarise(colname = srvyr::survey_mean(variable))
 #     } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
 #       estimation <- .ech.pkg.env$design %>%
 #         srvyr::group_by({{by.x}}) %>%
 #         srvyr::summarise(colname = srvyr::survey_mean(variable))
 #     } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
 #       estimation <- .ech.pkg.env$design %>%
 #         srvyr::group_by({{by.x}},{{by.y}}) %>%
 #         srvyr::summarise(colname = srvyr::survey_mean(variable))
 #     } else if(is.character(by.x) & is.null(by.y) & is.character(domain)){
 #       estimation <- .ech.pkg.env$design %>%
 #         srvyr::group_by({{by.x}}) %>%
 #         srvyr::summarise(colname = srvyr::survey_mean(variable))
 #     } else {
 #       estimation <- .ech.pkg.env$design %>%
 #         srvyr::filter(domain) %>%
 #         srvyr::group_by({{by.x}},{{by.y}}) %>%
 #         srvyr::summarise(colname = srvyr::survey_mean(variable))
 #     }
 #  }
 #
 # return(estimation)

}


#' A function to estimate variables at universe level
#'
#' This function allows you to estimate variable.
#' @param data data frame with ECH microdata
#' @param variable.x data frame column to estimate
#' @param variable.y data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference
#' @param level is household or individual
#' @importFrom dplyr mutate select filter group_by %>%
#' @importFrom glue glue
#' @importFrom srvyr summarise
#' @keywords inference
#' @export
#' @return table
#' @examples
#' \donttest{
#' get_estimation_ratio(data = ech::toy_ech_2018)
#' }


get_estimation_ratio <- function(data = ech::toy_ech_2018,
                                variable.x = NULL,
                                variable.y = NULL,
                                by.x = NULL,
                                by.y = NULL,
                                domain = NULL,
                                level = NULL){

  # stopifnot(!is.null(data) | !is.null(variable) | is.null(design))
  #  message(glue:glue("Debe indicar una variable a estimar"))
  # checks ---

  # estimation ---
  # if(is.character(variable) & nchar(variable)==2 | is.numeric(variable)){
  #
  #   if(is.null(by.x) & is.null(by.y) & is.null(domain)){
  #     estimation <- .ech.pkg.env$design %>%
  #       srvyr::summarise(colname = srvyr::survey_ratio(variable.x, variable.y))
  #   } else if(is.null(by.x) & is.null(by.y) & is.character(domain)){
  #     estimation <- .ech.pkg.env$design %>%
  #       srvyr::filter(domain) %>%
  #       srvyr::summarise(colname = srvyr::survey_ratio(variable.x, variable.y))
  #   } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
  #     estimation <- .ech.pkg.env$design %>%
  #       srvyr::group_by({{by.x}}) %>%
  #       srvyr::summarise(colname = srvyr::survey_ratio(variable.x, variable.y))
  #   } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
  #     estimation <- .ech.pkg.env$design %>%
  #       srvyr::group_by({{by.x}},{{by.y}}) %>%
  #       srvyr::summarise(colname = srvyr::survey_ratio(variable.x, variable.y))
  #   } else if(is.character(by.x) & is.null(by.y) & is.character(domain)){
  #     estimation <- .ech.pkg.env$design %>%
  #       srvyr::group_by({{by.x}}) %>%
  #       srvyr::summarise(colname = srvyr::survey_mean(variable))
  #   } else {
  #     estimation <- .ech.pkg.env$design %>%
  #       srvyr::filter(domain) %>%
  #       srvyr::group_by({{by.x}},{{by.y}}) %>%
  #       srvyr::summarise(colname = srvyr::survey_mean(variable))
  #   }
  #
  #
  # }
  #
  # return(estimation)

}
