#' get_estimation_mean
#'
#' This function allows you to estimate mean variable at universe level.
#' @param data data frame with ECH microdata
#' @param variable data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference setted as character expresion of logical evaluation
#' @param level is household ("h") or individual ("i").
#' @param ids ids
#' @param numero household id
#' @param estrato strata
#' @param pesoano weights
#' @param name name for the estimation new column
#'
#' @import survey
#' @import srvyr
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#'
#' @keywords inference
#' @export
#' @return table
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' \donttest{
#' get_estimation_mean(data = ech::toy_ech_2018, variable = "pobre06", by.x = "dpto", level = "h")
#' }

get_estimation_mean <- function(data = ech::toy_ech_2018,
                                variable = NULL,
                                by.x = NULL,
                                by.y = NULL,
                                domain = NULL,
                                level = NULL,
                                ids = NULL,
                                numero = "numero",
                                estrato = NULL,
                                pesoano = "pesoano",
                                name = "estimacion"){
 # checks ----
  assertthat::assert_that(!is.null(data) | !is.null(variable), msg = "You must indicate a variable")
  assertthat::assert_that(all(variable %in% names(data)), msg = glue::glue("Sorry... :( \n  {variable} is not in data"))
  if(!is.null(by.x)) assertthat::assert_that(by.x %in% names(data), msg = glue::glue("Sorry... :( \n  {by.x} is not in data"))
  if(!is.null(by.y)) assertthat::assert_that(by.y %in% names(data), msg = glue::glue("Sorry... :( \n  {by.y} is not in data"))
  if(!is.null(level)) assertthat::assert_that(level %in% c("household", "h", "individual", "i"), msg = "Check the level selected")
  if(!is.null(domain)) {
    dom <- strsplit(domain, '[==><!]')[[1]][1] %>% stringr::str_trim()
    assertthat::assert_that(dom %in% names(data), msg = glue::glue("Sorry... :( \n  {dom} is not in data"))
  }


# unlabelled
  d <- data %>% dplyr::select(!!!syms(c(variable, by.x, by.y, ids, numero, estrato, pesoano))) %>%
    unlabelled()

  d <- data %>% dplyr::select(if(!is.null(domain)){dom}) %>% dplyr::bind_cols(d, .)

# design ----
  design_ech <- ech::set_design(data = d, level = level)

# supressed warnings ---
  options(survey.lonely.psu = "adjust")
  options(dplyr.summarise.inform = FALSE)

# estimation ----

    if (is.factor(dplyr::pull(d[,variable]))) {
      if(is.null(by.x) & is.null(by.y) & is.null(domain)){
        estimation <- design_ech %>%
          srvyr::group_by(!!!syms(variable)) %>%
          srvyr::summarise(colname = srvyr::survey_mean(vartype = "ci"))
      } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
        estimation <- design_ech %>%
          srvyr::group_by(!!!syms(by.x), !!!syms(variable), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_mean(vartype = "ci"))
      } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
        estimation <- design_ech %>%
          srvyr::group_by(!!!syms(by.x), !!!syms(by.y), !!!syms(variable), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_mean(vartype = "ci"))
      } else if(is.null(by.x) & is.null(by.y) & !is.null(domain)){
        estimation <- design_ech %>%
          srvyr::filter(!!rlang::parse_expr(domain)) %>%
          srvyr::group_by(!!!syms(variable)) %>%
          srvyr::summarise(colname = srvyr::survey_mean(vartype = "ci"))
      } else if(is.character(by.x) & is.null(by.y) & !is.null(domain)){
        estimation <- design_ech %>%
          srvyr::filter(!!rlang::parse_expr(domain)) %>%
          srvyr::group_by(!!!syms(by.x), !!!syms(variable), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_mean(vartype = "ci"))
      } else {
        estimation <- design_ech %>%
          srvyr::filter(!!rlang::parse_expr(domain)) %>%
          srvyr::group_by(!!!syms(by.x), !!!syms(by.y), !!!syms(variable), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_mean(vartype = "ci"))
      }
  } else {
    if(is.null(by.x) & is.null(by.y) & is.null(domain)){
      estimation <- design_ech %>%
        srvyr::summarise(colname = srvyr::survey_mean(!!!syms(variable), vartype = "ci"))
    } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
      estimation <- design_ech %>%
        srvyr::group_by(!!!syms(by.x), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_mean(!!!syms(variable), vartype = "ci"))
    } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
      estimation <- design_ech %>%
        srvyr::group_by(!!!syms(by.x), !!!syms(by.y), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_mean(!!!syms(variable), vartype = "ci"))
    } else if(is.null(by.x) & is.null(by.y) & !is.null(domain)){
      estimation <- design_ech %>%
        srvyr::filter(!!rlang::parse_expr(domain)) %>%
        srvyr::summarise(colname = srvyr::survey_mean(!!!syms(variable), vartype = "ci"))
    } else if(is.character(by.x) & is.null(by.y) & !is.null(domain)){
      estimation <- design_ech %>%
        srvyr::filter(!!rlang::parse_expr(domain)) %>%
        srvyr::group_by(!!!syms(by.x), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_mean(!!!syms(variable), vartype = "ci"))
    } else {
      estimation <- design_ech %>%
        srvyr::filter(!!rlang::parse_expr(domain)) %>%
        srvyr::group_by(!!!syms(by.x), !!!syms(by.y), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_mean(!!!syms(variable), vartype = "ci"))
    }
  }

  if (name != "estimacion"){
    names(estimation) <- stringr::str_replace_all(names(estimation), "estimacion", name)
  }

  if (is.null(ids)) {
    message("These confidence intervals are only an approximation of the correct confidence intervals \n  that arise from fully defining the sample design")
    Sys.sleep(1)
    return(estimation)
  }
  return(estimation)

}

#' get_estimation_total
#'
#' This function allows you to estimate total variable at universe level.
#' @param data data frame with ECH microdata
#' @param variable data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference setted as character expresion of logical evaluation
#' @param level is household ("h") or individual ("i").
#' @param ids ids
#' @param numero household id
#' @param estrato strata
#' @param pesoano weights
#' @param name name for the estimation new column
#' @import survey
#' @import srvyr
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom haven is.labelled
#' @importFrom dplyr pull
#' @keywords inference
#' @export
#' @return table
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
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
                                ids = NULL,
                                numero = "numero",
                                estrato = NULL,
                                pesoano = "pesoano",
                                name = "estimacion"){
  # checks ----
  assertthat::assert_that(!is.null(data) | !is.null(variable), msg = "You must indicate a variable")
  assertthat::assert_that(all(variable %in% names(data)), msg = glue::glue("Sorry... :( \n  {variable} is not in {data}"))
  if(!is.null(by.x)) assertthat::assert_that(by.x %in% names(data), msg = glue::glue("Sorry... :( \n  {by.x} is not in {data}"))
  if(!is.null(by.y)) assertthat::assert_that(by.y %in% names(data), msg = glue::glue("Sorry... :( \n  {by.y} is not in {data}"))
  if(!is.null(level)) assertthat::assert_that(level %in% c("household", "h", "individual", "i"), msg = "Check the level selected")
  if(!is.null(domain)) {
    dom <- strsplit(domain, '[==><!]')[[1]][1] %>% stringr::str_trim()
    assertthat::assert_that(dom %in% names(data), msg = glue::glue("Sorry... :( \n  {dom} is not in data"))
  }

  # unlabelled
  d <- data %>% dplyr::select(!!!syms(c(variable, by.x, by.y, ids, numero, estrato, pesoano))) %>%
    unlabelled()

  d <- data %>% dplyr::select(if(!is.null(domain)){dom}) %>% dplyr::bind_cols(d, .)

  # design ----
  design_ech <- ech::set_design(data = d, level = level)

  # supressed warnings ---
  options(survey.lonely.psu = "adjust")
  options(dplyr.summarise.inform = FALSE)

  # estimation ----

  if (is.factor(dplyr::pull(d[,variable]))) {
    if(is.null(by.x) & is.null(by.y) & is.null(domain)){
      estimation <- design_ech %>%
        srvyr::group_by(!!!syms(variable)) %>%
        srvyr::summarise(colname = srvyr::survey_total(vartype = "ci"))
    } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
      estimation <- design_ech %>%
        srvyr::group_by(!!!syms(by.x), !!!syms(variable), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_total(vartype = "ci"))
    } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
      estimation <- design_ech %>%
        srvyr::group_by(!!!syms(by.x), !!!syms(by.y), !!!syms(variable), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_total(vartype = "ci"))
    } else if(is.null(by.x) & is.null(by.y) & !is.null(domain)){
      estimation <- design_ech %>%
        srvyr::filter(!!rlang::parse_expr(domain)) %>%
        srvyr::group_by(!!!syms(variable)) %>%
        srvyr::summarise(colname = srvyr::survey_total(vartype = "ci"))
    } else if(is.character(by.x) & is.null(by.y) & !is.null(domain)){
      estimation <- design_ech %>%
        srvyr::filter(!!rlang::parse_expr(domain)) %>%
        srvyr::group_by(!!!syms(by.x), !!!syms(variable), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_total(vartype = "ci"))
    } else {
      estimation <- design_ech %>%
        srvyr::filter(!!rlang::parse_expr(domain)) %>%
        srvyr::group_by(!!!syms(by.x), !!!syms(by.y), !!!syms(variable), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_total(vartype = "ci"))
    }
  } else {
      if(is.null(by.x) & is.null(by.y) & is.null(domain)){
        estimation <- design_ech %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
        estimation <- design_ech %>%
          srvyr::group_by(!!!syms(by.x), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
        estimation <- design_ech %>%
          srvyr::group_by(!!!syms(by.x), !!!syms(by.y), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      } else if(is.null(by.x) & is.null(by.y) & !is.null(domain)){
        estimation <- design_ech %>%
          srvyr::filter(eval(parse(text=domain))) %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      } else if(is.character(by.x) & is.null(by.y) & !is.null(domain)){
        estimation <- design_ech %>%
          srvyr::filter(eval(parse(text=domain))) %>%
          srvyr::group_by(!!!syms(by.x), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      } else {
        estimation <- design_ech %>%
          srvyr::filter(eval(parse(text=domain))) %>%
          srvyr::group_by(!!!syms(by.x), !!!syms(by.y), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      }
  }

  if (name != "estimacion"){
    names(estimation) <- stringr::str_replace_all(names(estimation), "estimacion", name)
  }

  if (is.null(ids)) {
    message("These confidence intervals are only an approximation of the correct confidence intervals \n  that arise from fully defining the sample design")
    Sys.sleep(1)
    return(estimation)
  }
  return(estimation)

}


#' get_estimation_ratio
#'
#' This function allows you to estimate ratio variable at universe level.
#' @param data data frame with ECH microdata
#' @param variable.x data frame column to estimate
#' @param variable.y data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference setted as character expresion of logical evaluation
#' @param level is household ("h") or individual ("i")
#' @param ids ids
#' @param numero household id
#' @param estrato strata
#' @param pesoano weights
#' @param name name for the estimation new column
#' @importFrom dplyr mutate select filter group_by %>%
#' @importFrom glue glue
#' @importFrom srvyr summarise
#' @keywords inference
#' @export
#' @return table
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
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
                                ids = NULL,
                                numero = "numero",
                                estrato = NULL,
                                pesoano = "pesoano",
                                name = "estimacion"){

  # checks ----
  assertthat::assert_that(!is.null(data) | !is.null(variable.x) | !is.null(variable.y), msg = "You must indicate a variable")
  assertthat::assert_that(all(variable.x %in% names(data)), msg = glue::glue("Sorry... :( \n {variable.x} is not in {data}"))
  assertthat::assert_that(all(variable.y %in% names(data)), msg = glue::glue("Sorry... :( \n {variable.y} is not in {data}"))
  if(!is.null(by.x)) assertthat::assert_that(by.x %in% names(data), msg = glue::glue("Sorry... :( \n {by.x} is not in {data}"))
  if(!is.null(by.y)) assertthat::assert_that(by.y %in% names(data), msg = glue::glue("Sorry... :( \n {by.y} is not in {data}"))
  if(!is.null(level)) assertthat::assert_that(level %in% c("household", "h", "individual", "i"), msg = "Check the level selected")
  if(!is.null(domain)) {
    dom <- strsplit(domain, '[==><!]')[[1]][1] %>% stringr::str_trim()
    assertthat::assert_that(dom %in% names(data), msg = glue::glue("Sorry... :( \n  {dom} is not in data"))
  }

  # unlabelled
  d <- data %>% dplyr::select(!!!syms(c(by.x, by.y, ids, numero, estrato, pesoano))) %>%
    unlabelled()

  d <- data %>% dplyr::select(if(!is.null(domain)){dom}) %>% dplyr::bind_cols(d, .)

  # design ---
  design_ech <- ech::set_design(data = data, level = level)

  # supressed warnings ---
  options(survey.lonely.psu="adjust")
  options(dplyr.summarise.inform = FALSE)

  # estimation ---

  if(is.null(by.x) & is.null(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::summarise(colname = srvyr::survey_ratio(!!!syms(variable.x), !!!syms(variable.y), vartype = "ci"))
  } else if(is.character(by.x) & is.null(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::group_by(!!!syms(by.x)) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(!!!syms(variable.x), !!!syms(variable.y), vartype = "ci"))
  } else if(is.character(by.x) & is.character(by.y) & is.null(domain)){
    estimation <- design_ech %>%
      srvyr::group_by(!!!syms(by.x), !!!syms(by.y)) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(!!!syms(variable.x), !!!syms(variable.y), vartype = "ci"))
  } else if(is.null(by.x) & is.null(by.y) & is.character(domain)){
    estimation <- design_ech %>%
      srvyr::filter(!!rlang::parse_expr(domain)) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(!!!syms(variable.x), !!!syms(variable.y), vartype = "ci"))
  } else if(is.character(by.x) & is.null(by.y) & is.character(domain)){
    estimation <- design_ech %>%
      srvyr::filter(!!rlang::parse_expr(domain)) %>%
      srvyr::group_by(!!!syms(by.x)) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(!!!syms(variable.x), !!!syms(variable.y), vartype = "ci"))
  } else {
    estimation <- design_ech %>%
      srvyr::filter(!!rlang::parse_expr(domain)) %>%
      srvyr::group_by(!!!syms(by.x), !!!syms(by.y)) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(!!!syms(variable.x), !!!syms(variable.y), vartype = "ci"))
  }

  if (name != "estimacion"){
    names(estimation) <- stringr::str_replace_all(names(estimation), "estimacion", name)
  }

  if (is.null(ids)) {
    message("These confidence intervals are only an approximation of the correct confidence intervals \n  that arise from fully defining the sample design")
    Sys.sleep(1)
    return(estimation)
  }
  return(estimation)

}

#' get_estimation_gini
#'
#' @param data ech data frame
#' @param variable income without rental value per capita deflated
#' @param by variable
#' @param level household or individual
#' @param name nombre
#'
#' @return table
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' \donttest{
#' df <- income_constant_prices(data = ech::toy_ech_2018, ipc = "R",
#'  base_month = "01", base_year = "2005")
#' get_estimation_gini(data = df, level = "h")
#' }
#
get_estimation_gini <- function(data = ech::toy_ech_2018,
                                variable = NULL,
                                by = NULL,
                                level = NULL,
                                name = "estimacion"){


  # design ---
  design_ech <- ech::set_design(data = data, level = level)

  # supressed warnings ---
  options(survey.lonely.psu = "adjust")

  # estimation ---
  design_ech <- convey::convey_prep(design_ech)
  estimation <- convey::svygini(~y_wrv_pc_d_r, design_ech, na.rm = TRUE)
  #estimation <- laeken::gini(variable, weights = weights, data = data)
}
