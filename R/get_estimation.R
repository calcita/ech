#' A function to estimate mean variables at universe level
#'
#' This function allows you to estimate mean variable.
#' @param data data frame with ECH microdata
#' @param variable data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference setted as logical
#' @param level is household ("h") or individual ("i").
#' @param name name for the estimation new column
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
                           level = NULL,
                           name = "estimacion"){
 # checks ----
  assertthat::assert_that(!is.null(data) | !is.null(variable), msg = "Debe indicar la variable")
  assertthat::assert_that(all(variable %in% names(data)), msg = glue::glue("La variable {variable} no esta en {data}"))
  if(!is.null(by.x)) assertthat::assert_that(by.x %in% names(data), msg = glue::glue("La variable {by.x} no esta en {data}"))
  if(!is.null(by.y)) assertthat::assert_that(by.y %in% names(data), msg = glue::glue("La variable {by.y} no esta en {data}"))
  # if(!is.null(domain)) assertthat::assert_that(domain %in% names(data), msg = glue::glue("La variable {domain} no esta en {data}"))
  if(!is.null(level)) assertthat::assert_that(level %in% c("household", "h", "individual", "i"), msg = "Verifica el nivel seleccionado")

# design ----
  design_ech <- ech::set_design(data = data, level = level)

# supressed warnings ---
  options(survey.lonely.psu = "adjust")
  options(dplyr.summarise.inform = FALSE)

# estimation ----

  if(is.null(by.x) & is.null(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::group_by(.data[[by.x]], add = T) %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::group_by(.data[[by.x]], .data[[by.y]], add = T) %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  } else if(is.null(by.x) & is.null(by.y) & is.logical(domain)){
    estimation <- design_ech %>%
      srvyr::filter(domain) %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  } else if(is.character(by.x) & is.null(by.y) & is.logical(domain)){
    estimation <- design_ech %>%
      srvyr::filter(domain) %>%
      srvyr::group_by(.data[[by.x]], add = T) %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  } else {
    estimation <- design_ech %>%
      srvyr::filter(domain) %>%
      srvyr::group_by(.data[[by.x]], .data[[by.y]], add = T) %>%
      srvyr::summarise(colname = srvyr::survey_mean(.data[[variable]]))
  }
  if (name != "estimacion"){
    names(estimation) <- stringr::str_replace_all(names(estimation), "colname", name)
  }
  return(estimation)

}

#' A function to estimate total variables at universe level
#'
#' This function allows you to estimate total variable.
#' @param data data frame with ECH microdata
#' @param variable data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference setted as logical
#' @param level is household ("h") or individual ("i").
#' @param name name for the estimation new column
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
#' get_estimation_total(variable = "pobre06", by.x = "dpto", level = "h")
#' }

get_estimation_total <- function(data = ech::toy_ech_2018,
                                variable = NULL,
                                by.x = NULL,
                                by.y = NULL,
                                domain = NULL,
                                level = NULL,
                                name = "estimacion"){
  # checks ----
  assertthat::assert_that(!is.null(data) | !is.null(variable), msg = "Debe indicar la variable")
  assertthat::assert_that(all(variable %in% names(data)), msg = glue::glue("La variable {variable} no esta en {data}"))
  if(!is.null(by.x)) assertthat::assert_that(by.x %in% names(data), msg = glue::glue("La variable {by.x} no esta en {data}"))
  if(!is.null(by.y)) assertthat::assert_that(by.y %in% names(data), msg = glue::glue("La variable {by.y} no esta en {data}"))
  if(!is.null(level)) assertthat::assert_that(level %in% c("household", "h", "individual", "i"), msg = "Verifica el nivel seleccionado")

  # design ----
  design_ech <- ech::set_design(data = data, level = level)

  # supressed warnings ---
  options(survey.lonely.psu = "adjust")
  options(dplyr.summarise.inform = FALSE)

  # estimation ----

  if(is.null(by.x) & is.null(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::summarise(colname = srvyr::survey_total(.data[[variable]]))
  } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::group_by(.data[[by.x]], add = T) %>%
      srvyr::summarise(colname = srvyr::survey_total(.data[[variable]]))
  } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::group_by(.data[[by.x]], .data[[by.y]]) %>%
      srvyr::summarise(colname = srvyr::survey_total(.data[[variable]]))
  } else if(is.null(by.x) & is.null(by.y) & is.logical(domain)){
    estimation <- design_ech %>%
      srvyr::filter(domain) %>%
      srvyr::summarise(colname = srvyr::survey_total(.data[[variable]]))
  } else if(is.character(by.x) & is.null(by.y) & is.logical(domain)){
    estimation <- design_ech %>%
      srvyr::filter(domain) %>%
      srvyr::group_by(.data[[by.x]]) %>%
      srvyr::summarise(colname = srvyr::survey_total(.data[[variable]]))
  } else {
    estimation <- design_ech %>%
      srvyr::filter(domain) %>%
      srvyr::group_by(.data[[by.x]], .data[[by.y]]) %>%
      srvyr::summarise(colname = srvyr::survey_total(.data[[variable]]))
  }
  if (name != "estimacion"){
    names(estimation) <- stringr::str_replace_all(names(estimation), "colname", name)
  }
  return(estimation)

}


#' A function to estimate ratio variables at universe level
#'
#' This function allows you to estimate ratio variable.
#' @param data data frame with ECH microdata
#' @param variable.x data frame column to estimate
#' @param variable.y data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference setted as logical
#' @param level is household ("h") or individual ("i")
#' @param name name for the estimation new column
#' @importFrom dplyr mutate select filter group_by %>%
#' @importFrom glue glue
#' @importFrom srvyr summarise
#' @keywords inference
#' @export
#' @return table
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- employment(data = ech::toy_ech_2018, pobpcoac = "pobpcoac")
#' get_estimation_ratio(data = toy_ech_2018, variable.x = "po", variable.y = "pea", level = "i")
#' }


get_estimation_ratio <- function(data = ech::toy_ech_2018,
                                variable.x = NULL,
                                variable.y = NULL,
                                by.x = NULL,
                                by.y = NULL,
                                domain = NULL,
                                level = NULL,
                                name = "estimacion"){

  # checks ----
  assertthat::assert_that(!is.null(data) | !is.null(variable.x) | !is.null(variable.y), msg = "Debe indicar la variable")
  assertthat::assert_that(all(variable.x %in% names(data)), msg = glue::glue("La variable {variable.x} no esta en {data}"))
  assertthat::assert_that(all(variable.y %in% names(data)), msg = glue::glue("La variable {variable.y} no esta en {data}"))
  if(!is.null(by.x)) assertthat::assert_that(by.x %in% names(data), msg = glue::glue("La variable {by.x} no esta en {data}"))
  if(!is.null(by.y)) assertthat::assert_that(by.y %in% names(data), msg = glue::glue("La variable {by.y} no esta en {data}"))
  if(!is.null(level)) assertthat::assert_that(level %in% c("household", "h", "individual", "i"), msg = "Verifica el nivel seleccionado")

  # design ---
  design_ech <- ech::set_design(data = data, level = level)

  # supressed warnings ---
  options(survey.lonely.psu="adjust")
  options(dplyr.summarise.inform = FALSE)

  # estimation ---

  if(is.null(by.x) & is.null(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
  } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::group_by(.data[[by.x]]) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
  } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::group_by(.data[[by.x]], .data[[by.y]]) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
  } else if(is.null(by.x) & is.null(by.y) & is.logical(domain)){
    estimation <- design_ech %>%
      srvyr::filter(domain) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
  } else if(is.character(by.x) & is.null(by.y) & is.logical(domain)){
    estimation <- design_ech %>%
      srvyr::filter(domain) %>%
      srvyr::group_by(.data[[by.x]]) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
  } else {
    estimation <- design_ech %>%
      srvyr::filter(domain) %>%
      srvyr::group_by(.data[[by.x]], .data[[by.y]]) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(.data[[variable.x]], .data[[variable.y]]))
  }
  if (name != "estimacion"){
    names(estimation) <- stringr::str_replace_all(names(estimation), "colname", name)
  }
  return(estimation)

}

#' Title
#'
#' @param data ech data frame
#' @param variable income without rental value per capita deflated
#' @param by.x variable
#' @param by.y variable
#' @param domain subpoblacion
#' @param level household or individual
#' @param name nombre
#'
#' @return table
#' @export

get_estimation_gini <- function(data = ech::toy_ech_2018,
                                variable = y_wrv_pc_d_r,
                                by.x = NULL,
                                by.y = NULL,
                                domain = NULL,
                                level = NULL,
                                name = "estimacion"){


  # design ---
  design_ech <- ech::set_design(data = data, level = level)

  # supressed warnings ---
  options(survey.lonely.psu = "adjust")
  options(dplyr.summarise.inform = FALSE)

  # estimation ---
  design_ech <- convey::convey_prep(design_ech)
  estimation <- convey::svygini(~variable, design_ech, na.rm = TRUE)

}
