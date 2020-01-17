#' A function to estimate variables at universe level
#'
#' This function allows you to estimate variable.
#' @param data data frame with ECH microdata
#' @param variable data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference
#' @param level is household or individual
#' @param design survey design
#' @importFrom dplyr mutate, select, filter, group_by
#' @importFrom glue glue
#' @importFrom srvyr summarise
#' @keywords inference
#' @export
#' @import tidyverse
#' @return table
#' @examples
#' get_estimation(data = df)
#

get_estimation_mean <- function(data = df,
                           variable = NULL,
                           by.x = NULL,
                           by.y = NULL,
                           domain = NULL,
                           level = NULL,
                           design = d){

  assertthat::assert_that(is.null(data) | is.null(variable) | is.null(design), msg = "Debe indicar la variable")
  assertthat::assert_that(all(variable %in% names(data)))

 #  message(glue:glue("Debe indicar una variable a estimar"))
 # checks ---

 # estimation ---
  if(is.character(variable) & nchar(variable)==2 | is.numeric(variable)){

    if(is.null(by.x) & is.null(by.y) & is.null(domain)){
    estimation <- design %>%
      srvyr::summarise(colname = srvyr::survey_mean(variable))
     } else if(is.null(by.x) & is.null(by.y) & is.character(domain)){
       estimation <- design %>%
         srvyr::filter(domain) %>%
         srvyr::summarise(colname = srvyr::survey_mean(variable))
     } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
       estimation <- design %>%
         srvyr::group_by({{by.x}}) %>%
         srvyr::summarise(colname = srvyr::survey_mean(variable))
     } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
       estimation <- design %>%
         srvyr::group_by({{by.x}},{{by.y}}) %>%
         srvyr::summarise(colname = srvyr::survey_mean(variable))
     } else if(is.character(by.x) & is.null(by.y) & is.character(domain)){
       estimation <- design %>%
         srvyr::group_by({{by.x}}) %>%
         srvyr::summarise(colname = srvyr::survey_mean(variable))
     } else {
       estimation <- design %>%
         srvyr::filter(domain) %>%
         srvyr::group_by({{by.x}},{{by.y}}) %>%
         srvyr::summarise(colname = srvyr::survey_mean(variable))
     }


  }

 return(estimation)

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
#' @param design survey design
#' @importFrom dplyr mutate, select, filter, group_by
#' @importFrom glue glue
#' @importFrom srvyr summarise
#' @keywords inference
#' @export
#' @import tidyverse
#' @return table
#' @examples
#' get_estimation(data = df)
#

get_estimation_ratio <- function(data = df,
                                variable.x = NULL,
                                variable.y = NULL,
                                by.x = NULL,
                                by.y = NULL,
                                domain = NULL,
                                level = NULL,
                                design = d){

  # stopifnot(!is.null(data) | !is.null(variable) | is.null(design))
  #  message(glue:glue("Debe indicar una variable a estimar"))
  # checks ---

  # estimation ---
  if(is.character(variable) & nchar(variable)==2 | is.numeric(variable)){

    if(is.null(by.x) & is.null(by.y) & is.null(domain)){
      estimation <- design %>%
        srvyr::summarise(colname = srvyr::survey_ratio(variable.x, variable.y))
    } else if(is.null(by.x) & is.null(by.y) & is.character(domain)){
      estimation <- design %>%
        srvyr::filter(domain) %>%
        srvyr::summarise(colname = srvyr::survey_ratio(variable.x, variable.y))
    } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
      estimation <- design %>%
        srvyr::group_by({{by.x}}) %>%
        srvyr::summarise(colname = srvyr::survey_ratio(variable.x, variable.y))
    } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
      estimation <- design %>%
        srvyr::group_by({{by.x}},{{by.y}}) %>%
        srvyr::summarise(colname = srvyr::survey_ratio(variable.x, variable.y))
    } else if(is.character(by.x) & is.null(by.y) & is.character(domain)){
      estimation <- design %>%
        srvyr::group_by({{by.x}}) %>%
        srvyr::summarise(colname = srvyr::survey_mean(variable))
    } else {
      estimation <- design %>%
        srvyr::filter(domain) %>%
        srvyr::group_by({{by.x}},{{by.y}}) %>%
        srvyr::summarise(colname = srvyr::survey_mean(variable))
    }


  }

  return(estimation)

}

