#' A function to estimate variables at universe level
#'
#' This function allows you to estimate variable.
#' @param data data frame with ECH microdata
#' @param variable data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference
#' @param level is household ("h") or individual ("i").
#' @import survey
#' @import srvyr
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @keywords inference
#' @export
#' @return table
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' get_estimation_mean(variable = "pobre06", by.x = "dpto", level = "h")
#' }

get_estimation_mean <- function(data = ech::toy_ech_2018,
                           variable = NULL,
                           by.x = NULL,
                           by.y = NULL,
                           domain = NULL,
                           level = NULL){
 # checks ----
  assertthat::assert_that(!is.null(data) | !is.null(variable), msg = "Debe indicar la variable")
  assertthat::assert_that(all(variable %in% names(data)), msg = glue:glue("La variable {variable} no esta en {data}"))
  if(!is.null(by.x)) assertthat::assert_that(by.x %in% names(data), msg = glue:glue("La variable {by.x} no esta en {data}"))
  if(!is.null(by.y)) assertthat::assert_that(by.y %in% names(data), msg = glue:glue("La variable {by.y} no esta en {data}"))
  if(!is.null(domain)) assertthat::assert_that(domain %in% names(data), msg = glue:glue("La variable {domain} no esta en {data}"))
  if(!is.null(level)) assertthat::assert_that(level %in% c("household", "h", "individual", "i"), msg = "Verifica el nivel seleccionado")

# design ----
  design_ech <- ech::set_design(data = data, level = level)
  options(survey.lonely.psu="adjust")

# estimation ----
  #if(is.character(variable) & nchar(variable)==2 | is.numeric(variable)){

  if(is.null(by.x) & is.null(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  } else if(is.null(by.x) & is.null(by.y) & is.character(domain)){
    estimation <- design_ech %>%
      srvyr::filter(.data[[domain]]) %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::group_by(.data[[by.x]], add = T) %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::group_by(.data[[by.x]], .data[[by.y]]) %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  } else if(is.character(by.x) & is.null(by.y) & is.character(domain)){
    estimation <- design_ech %>%
      srvyr::group_by(.data[[by.x]]) %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  } else {
    estimation <- design_ech %>%
      srvyr::filter(.data[[domain]]) %>%
      srvyr::group_by(.data[[by.x]], .data[[by.y]]) %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  }
  #  }

 #return(estimation)

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
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
# @examples
# \donttest{
# get_estimation_ratio(data = ech::toy_ech_2018)
# }


get_estimation_ratio <- function(data = ech::toy_ech_2018,
                                variable.x = NULL,
                                variable.y = NULL,
                                by.x = NULL,
                                by.y = NULL,
                                domain = NULL,
                                level = NULL){

  # checks ----
  assertthat::assert_that(!is.null(data) | !is.null(variable.x) | !is.null(variable.y), msg = "Debe indicar la variable")
  assertthat::assert_that(all(variable.x %in% names(data)), msg = glue:glue("La variable {variable.x} no esta en {data}"))
  assertthat::assert_that(all(variable.y %in% names(data)), msg = glue:glue("La variable {variable.y} no esta en {data}"))
  if(!is.null(by.x)) assertthat::assert_that(by.x %in% names(data), msg = glue:glue("La variable {by.x} no esta en {data}"))
  if(!is.null(by.y)) assertthat::assert_that(by.y %in% names(data), msg = glue:glue("La variable {by.y} no esta en {data}"))
  if(!is.null(domain)) assertthat::assert_that(domain %in% names(data), msg = glue:glue("La variable {domain} no esta en {data}"))
  if(!is.null(level)) assertthat::assert_that(level %in% c("household", "h", "individual", "i"), msg = "Verifica el nivel seleccionado")

  # estimation ---
  # if(is.character(variable) | is.numeric(variable)){
  #
  design_ech <- ech::set_design(data = data, level = level)

    if(is.null(by.x) & is.null(by.y) & is.null(domain)){
      estimation <- design_ech %>%
        srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
    } else if(is.null(by.x) & is.null(by.y) & is.character(domain)){
      estimation <- design_ech %>%
        srvyr::filter(.data[[domain]]) %>%
        srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
    } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
      estimation <- design_ech %>%
        srvyr::group_by(.data[[by.x]]) %>%
        srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
    } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
      estimation <- design_ech %>%
        srvyr::group_by(.data[[by.x]], .data[[by.y]]) %>%
        srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
    } else if(is.character(by.x) & is.null(by.y) & is.character(domain)){
      estimation <- design_ech %>%
        srvyr::group_by(.data[[by.x]]) %>%
        srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
    } else {
      estimation <- design_ech %>%
        srvyr::filter(domain) %>%
        srvyr::group_by(.data[[by.x]], .data[[by.y]]) %>%
        srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
    }

  #}
  #
  # return(estimation)

}

