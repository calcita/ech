#' This function allows you to estimate mean variable at universe level.
#' @family estimation
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
#' @import srvyr
#' @importFrom assertthat assert_that
#' @importFrom glue glue
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
  if(!is.null(ids)) {
    assertthat::assert_that(ids %in% names(data), msg = glue::glue("Sorry... :( \n  {ids} is not in data"))
  }

# unlabelled
  d <- data %>% dplyr::select(!!!syms(c(variable, by.x, by.y, ids, numero, estrato, pesoano))) %>%
    unlabelled()
  d <- data %>% dplyr::select(if(!is.null(domain)){dom}) %>% dplyr::bind_cols(d, .)
  d <- d %>% tidyr::drop_na(dplyr::all_of(variable))

# design ----
  design_ech <- ech::set_design(data = d, level = level, numero = numero, ids = ids, estrato = estrato, pesoano = pesoano)

# supressed warnings ---
  old <- options()
  on.exit(options(old))
  options(survey.lonely.psu = "adjust")
  options(dplyr.summarise.inform = FALSE)

# estimation ----

  if (is.factor(d %>% dplyr::pull(variable))) {
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

  names(estimation) <- stringr::str_replace_all(names(estimation), "colname", name)

  if (is.null(ids)) {
    message("These confidence intervals are only an approximation of the correct confidence intervals \n  that arise from fully defining the sample design")
    Sys.sleep(1)
    return(estimation)
  }
  return(estimation)

}

#' This function allows you to estimate median variable at universe level.
#' @family estimation
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
#' @keywords inference
#' @export
#' @return table
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' \donttest{
#' get_estimation_median(data = ech::toy_ech_2018, variable = "ht11", by.x = "dpto", level = "h")
#' }

get_estimation_median <- function(data = ech::toy_ech_2018,
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
  if (!is.null(by.x)) assertthat::assert_that(by.x %in% names(data), msg = glue::glue("Sorry... :( \n  {by.x} is not in data"))
  if (!is.null(by.y)) assertthat::assert_that(by.y %in% names(data), msg = glue::glue("Sorry... :( \n  {by.y} is not in data"))
  if (!is.null(level)) assertthat::assert_that(level %in% c("household", "h", "individual", "i"), msg = "Check the level selected")
  if (!is.null(domain)) {
    dom <- strsplit(domain, '[==><!]')[[1]][1] %>% stringr::str_trim()
    assertthat::assert_that(dom %in% names(data), msg = glue::glue("Sorry... :( \n  {dom} is not in data"))
  }
  if (!is.null(ids)) {
    assertthat::assert_that(ids %in% names(data), msg = glue::glue("Sorry... :( \n  {ids} is not in data"))
  }

  # unlabelled
  d <- data %>% dplyr::select(!!!syms(c(variable, by.x, by.y, ids, numero, estrato, pesoano))) %>%
    unlabelled()

  d <- data %>% dplyr::select(if (!is.null(domain)) {dom}) %>% dplyr::bind_cols(d, .)

  d <- d %>% tidyr::drop_na(dplyr::all_of(variable))

  # design ----
  design_ech <- ech::set_design(data = d, level = level, numero = numero, ids = ids, estrato = estrato, pesoano = pesoano)

  # supressed warnings ---
  old <- options()
  on.exit(options(old))
  options(survey.lonely.psu = "adjust")
  options(dplyr.summarise.inform = FALSE)

  # estimation ----

  if (is.numeric(d %>% dplyr::pull(variable))) {
    if (is.null(by.x) & is.null(by.y) & is.null(domain)) {
      estimation <- design_ech %>%
        srvyr::summarise(colname = srvyr::survey_median(!!!syms(variable), vartype = "ci"))
    } else if (is.character(by.x) & is.null(by.y) & is.null(domain)) {
      estimation <- design_ech %>%
        srvyr::group_by(!!!syms(by.x), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_median(!!!syms(variable), vartype = "ci"))
    } else if (is.character(by.x) & is.character(by.y) & is.null(domain)) {
      estimation <- design_ech %>%
        srvyr::group_by(!!!syms(by.x), !!!syms(by.y), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_median(!!!syms(variable), vartype = "ci"))
    } else if (is.null(by.x) & is.null(by.y) & !is.null(domain)) {
      estimation <- design_ech %>%
        srvyr::filter(!!rlang::parse_expr(domain)) %>%
        srvyr::summarise(colname = srvyr::survey_median(!!!syms(variable), vartype = "ci"))
    } else if (is.character(by.x) & is.null(by.y) & !is.null(domain)) {
      estimation <- design_ech %>%
        srvyr::filter(!!rlang::parse_expr(domain)) %>%
        srvyr::group_by(!!!syms(by.x), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_median(!!!syms(variable), vartype = "ci"))
    } else {
      estimation <- design_ech %>%
        srvyr::filter(!!rlang::parse_expr(domain)) %>%
        srvyr::group_by(!!!syms(by.x), !!!syms(by.y), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_median(!!!syms(variable), vartype = "ci"))
    }
  } else {
    stop(glue::glue("Sorry... :( \n  {variable} is not numeric"))
  }

  names(estimation) <- stringr::str_replace_all(names(estimation), "colname", name)

  if (is.null(ids)) {
    message("These confidence intervals are only an approximation of the correct confidence intervals \n  that arise from fully defining the sample design")
    Sys.sleep(1)
    return(estimation)
  }
  return(estimation)

}

#' This function allows you to estimate total variable at universe level.
#' @family estimation
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
  assertthat::assert_that(all(variable %in% names(data)), msg = glue::glue("Sorry... :( \n  {variable} is not in data"))
  if (!is.null(by.x)) assertthat::assert_that(by.x %in% names(data), msg = glue::glue("Sorry... :( \n  {by.x} is not in data"))
  if (!is.null(by.y)) assertthat::assert_that(by.y %in% names(data), msg = glue::glue("Sorry... :( \n  {by.y} is not in data"))
  if (!is.null(level)) assertthat::assert_that(level %in% c("household", "h", "individual", "i"), msg = "Check the level selected")
  if (!is.null(domain)) {
    dom <- strsplit(domain, '[==><!]')[[1]][1] %>% stringr::str_trim()
    assertthat::assert_that(dom %in% names(data), msg = glue::glue("Sorry... :( \n  {dom} is not in data"))
  }
  if (!is.null(ids)) {
    assertthat::assert_that(ids %in% names(data), msg = glue::glue("Sorry... :( \n  {ids} is not in data"))
  }

  # unlabelled
  d <- data %>% dplyr::select(!!!syms(c(variable, by.x, by.y, ids, numero, estrato, pesoano))) %>%
    unlabelled()
  d <- data %>% dplyr::select(if (!is.null(domain)){dom}) %>% dplyr::bind_cols(d, .)
  d <- d %>% tidyr::drop_na(dplyr::all_of(variable))

  # design ----
  design_ech <- ech::set_design(data = d, level = level, numero = numero, ids = ids, estrato = estrato, pesoano = pesoano)

  # supressed warnings ---
  old <- options()
  on.exit(options(old))
  options(survey.lonely.psu = "adjust")
  options(dplyr.summarise.inform = FALSE)

  # estimation ----

  if (is.factor(d %>% dplyr::pull(variable))) {
    if (is.null(by.x) & is.null(by.y) & is.null(domain)) {
      estimation <- design_ech %>%
        srvyr::group_by(!!!syms(variable)) %>%
        srvyr::summarise(colname = srvyr::survey_total(vartype = "ci"))
    } else if (is.character(by.x) & is.null(by.y) & is.null(domain)) {
      estimation <- design_ech %>%
        srvyr::group_by(!!!syms(by.x), !!!syms(variable), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_total(vartype = "ci"))
    } else if (is.character(by.x) & is.character(by.y) & is.null(domain)) {
      estimation <- design_ech %>%
        srvyr::group_by(!!!syms(by.x), !!!syms(by.y), !!!syms(variable), .add = T) %>%
        srvyr::summarise(colname = srvyr::survey_total(vartype = "ci"))
    } else if (is.null(by.x) & is.null(by.y) & !is.null(domain)) {
      estimation <- design_ech %>%
        srvyr::filter(!!rlang::parse_expr(domain)) %>%
        srvyr::group_by(!!!syms(variable)) %>%
        srvyr::summarise(colname = srvyr::survey_total(vartype = "ci"))
    } else if (is.character(by.x) & is.null(by.y) & !is.null(domain)) {
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
      if (is.null(by.x) & is.null(by.y) & is.null(domain)) {
        estimation <- design_ech %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      } else if (is.character(by.x) & is.null(by.y) & is.null(domain)) {
        estimation <- design_ech %>%
          srvyr::group_by(!!!syms(by.x), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      } else if (is.character(by.x) & is.character(by.y) & is.null(domain)) {
        estimation <- design_ech %>%
          srvyr::group_by(!!!syms(by.x), !!!syms(by.y), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      } else if (is.null(by.x) & is.null(by.y) & !is.null(domain)) {
        estimation <- design_ech %>%
          srvyr::filter(eval(parse(text = domain))) %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      } else if (is.character(by.x) & is.null(by.y) & !is.null(domain)) {
        estimation <- design_ech %>%
          srvyr::filter(eval(parse(text = domain))) %>%
          srvyr::group_by(!!!syms(by.x), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      } else {
        estimation <- design_ech %>%
          srvyr::filter(eval(parse(text = domain))) %>%
          srvyr::group_by(!!!syms(by.x), !!!syms(by.y), .add = T) %>%
          srvyr::summarise(colname = srvyr::survey_total(!!!syms(variable), vartype = "ci"))
      }
  }

  names(estimation) <- stringr::str_replace_all(names(estimation), "colname", name)

  if (is.null(ids)) {
    message("These confidence intervals are only an approximation of the correct confidence intervals \n  that arise from fully defining the sample design")
    Sys.sleep(1)
    return(estimation)
  }
  return(estimation)

}


#' This function allows you to estimate ratio variables at universe level.
#' @family estimation
#' @param data data frame with ECH microdata
#' @param variable.x data frame column to estimate
#' @param variable.y data frame column to estimate
#' @param by.x data frame column
#' @param by.y data frame column
#' @param domain subpopulation reference setted as character expresion of logical evaluation
#' @param level is household ("h") or individual ("i")
#' @param ids Variable name of cluster
#' @param numero Variable name of household id
#' @param estrato Variable name of strata
#' @param pesoano Variable name of weights
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
  assertthat::assert_that(all(variable.x %in% names(data)), msg = glue::glue("Sorry... :( \n {variable.x} is not in data"))
  assertthat::assert_that(all(variable.y %in% names(data)), msg = glue::glue("Sorry... :( \n {variable.y} is not in data"))
  if (!is.null(by.x)) assertthat::assert_that(by.x %in% names(data), msg = glue::glue("Sorry... :( \n {by.x} is not in data"))
  if (!is.null(by.y)) assertthat::assert_that(by.y %in% names(data), msg = glue::glue("Sorry... :( \n {by.y} is not in data"))
  if (!is.null(level)) assertthat::assert_that(level %in% c("household", "h", "individual", "i"), msg = "Check the level selected")
  if (!is.null(domain)) {
    dom <- strsplit(domain, '[==><!]')[[1]][1] %>% stringr::str_trim()
    assertthat::assert_that(dom %in% names(data), msg = glue::glue("Sorry... :( \n  {dom} is not in data"))
  }
  if (!is.null(ids)) {
    assertthat::assert_that(ids %in% names(data), msg = glue::glue("Sorry... :( \n  {ids} is not in data"))
  }

  # unlabelled
  d <- data %>% dplyr::select(!!!syms(c(by.x, by.y, ids, numero, estrato, pesoano))) %>%
    unlabelled()
  d <- data %>% dplyr::select(!!!syms(c(variable.x, variable.y))) %>%
    haven::zap_labels() %>%
    haven::zap_formats() %>%
    haven::zap_label() %>%
    dplyr::bind_cols(d, .)
  d <- data %>% dplyr::select(if (!is.null(domain)) {dom}) %>% dplyr::bind_cols(d, .)

  # design ----
  design_ech <- ech::set_design(data = d, level = level, numero = numero, ids = ids, estrato = estrato, pesoano = pesoano)

  # supressed warnings ---
  old <- options()
  on.exit(options(old))
  options(survey.lonely.psu = "adjust")
  options(dplyr.summarise.inform = FALSE)

  # estimation ---

  if (is.null(by.x) & is.null(by.y) & is.null(domain)) {
    estimation <- design_ech %>%
      srvyr::summarise(colname = srvyr::survey_ratio(!!!syms(variable.x), !!!syms(variable.y), vartype = "ci"))
  } else if (is.character(by.x) & is.null(by.y) & is.null(domain)) {
    estimation <- design_ech %>%
      srvyr::group_by(!!!syms(by.x)) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(!!!syms(variable.x), !!!syms(variable.y), vartype = "ci"))
  } else if (is.character(by.x) & is.character(by.y) & is.null(domain)) {
    estimation <- design_ech %>%
      srvyr::group_by(!!!syms(by.x), !!!syms(by.y)) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(!!!syms(variable.x), !!!syms(variable.y), vartype = "ci"))
  } else if (is.null(by.x) & is.null(by.y) & is.character(domain)) {
    estimation <- design_ech %>%
      srvyr::filter(!!rlang::parse_expr(domain)) %>%
      srvyr::summarise(colname = srvyr::survey_ratio(!!!syms(variable.x), !!!syms(variable.y), vartype = "ci"))
  } else if (is.character(by.x) & is.null(by.y) & is.character(domain)) {
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

  names(estimation) <- stringr::str_replace_all(names(estimation), "colname", name)

  if (is.null(ids)) {
    message("These confidence intervals are only an approximation of the correct confidence intervals \n  that arise from fully defining the sample design")
    Sys.sleep(1)
    return(estimation)
  }
  return(estimation)

}

#' This function allows you to estimate the Gini coefficient
#' @family estimation
#' @param data ech data frame
#' @param variable Variable name of income without rental value per capita deflated
#' @param by data frame column
#' @param level is household ("h") or individual ("i").
#' @param ids Variable name of cluster
#' @param numero Variable name of household id
#' @param estrato Variable name of strata
#' @param pesoano Variable name of weights
#' @param bootstrap Logical value
#' @param r A number of replicas
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom laeken gini
#' @return table
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' \donttest{
#' toy_ech_2018 <- income_constant_prices(data = ech::toy_ech_2018, index = "IPC", level = "R",
#'                                        base_month = "01", base_year = "2005")
#' get_estimation_gini(data = toy_ech_2018, variable = "y_wrv_pc_d_r", level = "i")
#' }

get_estimation_gini <- function(data = ech::toy_ech_2018,
                                variable = NULL,
                                by = NULL,
                                level = NULL,
                                ids = NULL,
                                numero = "numero",
                                estrato = NULL,
                                pesoano = "pesoano",
                                bootstrap = FALSE,
                                r = NULL){


  assertthat::assert_that(!is.null(data) | !is.null(variable) | !is.null(numero) | !is.null(pesoano) | !is.null(level), msg = "You must indicate a variable")
  assertthat::assert_that(all(variable %in% names(data)), msg = glue::glue("Sorry... :( \n {variable} is not in data"))
  assertthat::assert_that(all(pesoano %in% names(data)), msg = glue::glue("Sorry... :( \n {pesoano} is not in data"))
  assertthat::assert_that(all(numero %in% names(data)), msg = glue::glue("Sorry... :( \n {numero} is not in data"))
  if (!is.null(estrato)) {
    assertthat::assert_that(estrato %in% names(data), msg = glue::glue("Sorry... :( \n  {estrato} is not in data"))
  }
  if (!is.null(ids)) {
    assertthat::assert_that(ids %in% names(data), msg = glue::glue("Sorry... :( \n  {ids} is not in data"))
  }

  if (level == "h") {
    d <- data %>% dplyr::filter(duplicated(numero) == FALSE) %>%
      tidyr::drop_na(dplyr::all_of(variable))
  } else {
    d <- data %>%
      tidyr::drop_na(dplyr::all_of(variable))
  }

  d <- d %>% dplyr::select(!!!syms(c(variable, by, ids, estrato, pesoano))) %>%
    unlabelled()

 d <- as.data.frame(d)
 v <- as.numeric(d[, variable])
 p <- as.integer(d[, pesoano])

  if (is.null(r)) r <- 200

  if (is.null(by) & isFALSE(bootstrap) & is.null(ids)) {
     estimation <- laeken::gini(inc = v, weights = p)
     estimation <- data.frame(estimation[[1]])
     names(estimation) <- "value"
  } else if (is.character(by) & isFALSE(bootstrap) & is.null(ids)) {
    suppressMessages({
    b <- as.factor(d[, by])
    estimation <- laeken::gini(inc = v, weights = p, breakdown = b)
    value <- estimation[[2]]
    value_total <- data.frame(dplyr::bind_cols("Total", estimation[[1]]))
    names(value_total) <- names(value)
    estimation <- dplyr::bind_rows(value, value_total)
    })
  } else if (is.null(by) & isFALSE(bootstrap) & is.character(ids)) {
     e <- as.integer(d[, estrato])
     i <-  as.integer(d[, ids])
     estimation <- laeken::gini(inc = v, weights = p, design = e, cluster = i)
     estimation <- data.frame(estimation[[1]])
     names(estimation) <- "value"
  } else if (is.null(by) & isTRUE(bootstrap) & is.null(ids)) {
    estimation <- laeken::gini(inc = v, weights = p, var = "bootstrap", bootType = "naive", seed = 1234, R = r)
    suppressMessages({
    value <- data.frame(estimation[[1]])
    names(value) <- "value"
    var <- data.frame(estimation[[4]])
    names(var) <- "variance"
    ci <- dplyr::bind_cols(estimation[[6]][1], estimation[[6]][2])
    names(ci) <- c("lower", "upper")
    estimation <- dplyr::bind_cols(value, var, ci)
    })
  } else if (is.character(by) & isFALSE(bootstrap) & is.character(ids)) {
    b <- as.factor(d[, by])
    e <- as.integer(d[, estrato])
    i <-  as.integer(d[, ids])
    estimation <- laeken::gini(inc = v, weights = p, breakdown = b, design = e, cluster = i)
    suppressMessages({
    value <- estimation[[2]]
    value_total <- data.frame(dplyr::bind_cols("Total", estimation[[1]]))
    names(value_total) <- names(value)
    estimation <- dplyr::bind_rows(value, value_total)
    })
  } else if (is.character(by) & isTRUE(bootstrap) & is.null(ids)) {
    b <- as.factor(d[, by])
    estimation <- laeken::gini(inc = v, weights = p, breakdown = b, var = "bootstrap", bootType = "naive", seed = 1234, R = r)
    suppressMessages({
      value <- estimation[[2]]
      value_total <- data.frame(dplyr::bind_cols("Total",estimation[[1]]))
      names(value_total) <- names(value)
      value <- dplyr::bind_rows(value, value_total)
      ###
      var <- estimation[[5]]
      var_total <- data.frame(dplyr::bind_cols("Total",estimation[[4]]))
      names(var_total) <- names(var)
      var <- dplyr::bind_rows(var, var_total)
      ###
      ci <- estimation[[7]]
      ci_total <- data.frame(dplyr::bind_cols("Total",estimation[[6]][1], estimation[[6]][2]))
      names(ci_total) <- names(ci)
      ci <- dplyr::bind_rows(ci, ci_total)
      estimation <- dplyr::bind_cols(value, var[2], ci[, 2:3])
    })
  } else if (is.null(by) & isTRUE(bootstrap) & is.character(ids)) {
    e <- as.integer(d[, estrato])
    i <-  as.integer(d[, ids])
    estimation <- laeken::gini(inc = v, weights = p, design = e, cluster = i, var = "bootstrap", bootType = "naive", seed = 1234, R = r)
    suppressMessages({
    value <- data.frame(estimation[[1]])
    names(value) <- "value"
    var <- data.frame(estimation[[4]])
    names(var) <- "variance"
    ci <- dplyr::bind_cols(estimation[[6]][1], estimation[[6]][2])
    names(ci) <- c("lower", "upper")
    estimation <- dplyr::bind_cols(value, var, ci)
    })
  } else{
    b <-  as.factor((d[, by]))
    e <- as.integer(d[, estrato])
    i <-  as.integer(d[, ids])
    suppressWarnings({
    estimation <- laeken::gini(inc = v, weights = p, design = e, cluster = i,
                               var = "bootstrap", bootType = "naive",
                               breakdown = b, seed = 1234, R = r)
    })
    suppressMessages({
      value <- estimation[[2]]
      value_total <- data.frame(dplyr::bind_cols("Total",estimation[[1]]))
      names(value_total) <- names(value)
      value <- dplyr::bind_rows(value, value_total)
      ###
      var <- estimation[[5]]
      var_total <- data.frame(dplyr::bind_cols("Total",estimation[[4]]))
      names(var_total) <- names(var)
      var <- dplyr::bind_rows(var, var_total)
      ###
      ci <- estimation[[7]]
      ci_total <- data.frame(dplyr::bind_cols("Total",estimation[[6]][1], estimation[[6]][2]))
      names(ci_total) <- names(ci)
      ci <- dplyr::bind_rows(ci, ci_total)
      estimation <- dplyr::bind_cols(value, var[2], ci[, 2:3])
    })
  }

  return(estimation)
}


#' This function allows you to estimate the Gender Pay Wage Gap (GPG)
#' @family estimation
#' @param data data.frame
#' @param variable Variable name of total income per hour
#' @param e26 Variable name of sex
#' @param by data frame column
#' @param ids Variable name of cluster
#' @param estrato Variable name of strata
#' @param pesoano Variable name of weights
#' @param stat Media or Median
#'
#' @return table
#' @export
#'
#' @examples
#' \donttest{
#' toy_ech_2018 <- labor_income_per_hour(data = ech::toy_ech_2018, base_month = 6, base_year = 2018)
#' get_estimation_gpg(data = toy_ech_2018, variable = "total_income_per_hour", e26 = "e26")
#' }

get_estimation_gpg <- function(data = ech::toy_ech_2018,
                               variable = "total_income_per_hour",
                               e26 = "e26",
                               by = NULL,
                               ids = NULL,
                               estrato = NULL,
                               pesoano = "pesoano",
                               stat = "media"){

  assertthat::assert_that(!is.null(data) | !is.null(variable) | !is.null(pesoano) | !is.null(e26), msg = "You must indicate a variable")
  #assertthat::assert_that(!is.null(ids) & is.null(estrato), msg = "You must indicate the ids")
  assertthat::assert_that(all(variable %in% names(data)), msg = glue::glue("Sorry... :( \n {variable} is not in data"))
  assertthat::assert_that(all(pesoano %in% names(data)), msg = glue::glue("Sorry... :( \n {pesoano} is not in data"))
  assertthat::assert_that(all(e26 %in% names(data)), msg = glue::glue("Sorry... :( \n {e26} is not in data"))
  assertthat::assert_that(all(stat %in% c("media", "median")), msg = glue::glue("Sorry... :( \n {stat} can be median or media"))
  if (!is.null(estrato)) {
    assertthat::assert_that(estrato %in% names(data), msg = glue::glue("Sorry... :( \n  {estrato} is not in data"))
  }
  if (!is.null(ids)) {
    assertthat::assert_that(ids %in% names(data), msg = glue::glue("Sorry... :( \n  {ids} is not in data"))
  }
  if (!is.null(by)) {
    assertthat::assert_that(by %in% names(data), msg = glue::glue("Sorry... :( \n  {by} is not in data"))
  }

  d <- data %>% dplyr::select(!!!syms(c(variable, e26, by, ids, estrato, pesoano))) %>%
    unlabelled() %>%
    tidyr::drop_na(dplyr::all_of(variable))

  d <- d %>%  dplyr::mutate(e26 = ifelse(e26 == "Mujer", "female", "male"),
                  e26 = factor(e26, levels = c("female", "male"), labels = c("female", "male")))

  d <- as.data.frame(d)
  v <- as.numeric(d[, variable])
  p <- as.integer(d[, pesoano])
  g <- as.factor(d[, e26])

  if (is.null(by) & is.null(ids)) {
    estimation <- laeken::gpg(inc = v, gender = g, weights = p, method = stat)
    suppressMessages({
      estimation <- data.frame(estimation[[1]])
      names(estimation) <- "value"
    })
  } else if (is.character(by) & is.null(ids)) {
    b <- as.factor(d[, by])
    estimation <- laeken::gpg(inc = v, gender = g, weights = p, breakdown = b, method = stat)
    suppressMessages({
      value <- estimation[[2]]
      value_total <- data.frame(dplyr::bind_cols("Total",estimation[[1]]))
      names(value_total) <- names(value)
      estimation <- dplyr::bind_rows(value, value_total)
    })
  } else if (is.null(by) & is.character(ids)) {
    e <- as.integer(d[, estrato])
    i <-  as.integer(d[, ids])
    estimation <- laeken::gpg(inc = v, gender = g, weights = p, design = e, cluster = i, method = stat)
    suppressMessages({
      estimation <- data.frame(estimation[[1]])
      names(estimation) <- "value"
    })
  } else{
    b <-  as.factor((d[, by]))
    e <- as.integer(d[, estrato])
    i <-  as.integer(d[, ids])
    estimation <- laeken::gpg(inc = v, gender = g, weights = p, breakdown = b, design = e, cluster = i, method = stat)
    suppressMessages({
      value <- estimation[[2]]
      value_total <- data.frame(dplyr::bind_cols("Total",estimation[[1]]))
      names(value_total) <- names(value)
      estimation <- dplyr::bind_rows(value, value_total)
    })
  }
  return(estimation)

}


#' This function allows you to estimate de Income Quintile Share Ratio
#' @family estimation
#' @param data data.frame
#' @param variable Variable name of total income per hour
#' @param by data frame column
#' @param ids Variable name of cluster
#' @param estrato Variable name of strata
#' @param pesoano Variable name of weights
#'
#' @return table
#' @export
#'
#' @examples
#' \donttest{
#' toy_ech_2018 <- income_constant_prices(data = ech::toy_ech_2018, index = "IPC", level = "R",
#'                                        base_month = "01", base_year = "2005")
#' get_estimation_qsr(data = toy_ech_2018, variable = "y_pc_d_r", pesoano = "pesoano")
#' }

get_estimation_qsr <- function(data = ech::toy_ech_2018,
                               variable = "y_pc_d_r",
                               by = NULL,
                               ids = NULL,
                               estrato = NULL,
                               pesoano = "pesoano"){

  #Checks----
  assertthat::assert_that(variable %in% names(data), msg = glue::glue("Sorry... :( \n \t {variable} is not in data, you must use income_constant_prices function"))
  assertthat::assert_that(pesoano %in% names(data), msg = glue::glue("Sorry... :( \n  \t {pesoano} is not in data"))
  if (!is.null(estrato)) {
    assertthat::assert_that(estrato %in% names(data), msg = glue::glue("Sorry... :( \n  \t {estrato} is not in data"))
  }
  if (!is.null(ids)) {
    assertthat::assert_that(ids %in% names(data), msg = glue::glue("Sorry... :( \n  \t {ids} is not in data"))
  }
  if (!is.null(by)) {
    assertthat::assert_that(by %in% names(data), msg = glue::glue("Sorry... :( \n  \t {by} is not in data"))
  }

  d <- data %>% dplyr::select(!!!syms(c(variable, by, ids, estrato, pesoano))) %>%
    unlabelled() %>%
    tidyr::drop_na(dplyr::all_of(variable))
  d <- as.data.frame(d)

  if (is.null(by) & is.null(ids)) {
    estimation <- laeken::qsr(inc = variable, weights = pesoano, data = d)
    suppressMessages({
    estimation <- data.frame(estimation[[1]])
    names(estimation) <- "value"
    })
  } else if (is.character(by) & is.null(ids)) {
     estimation <- laeken::qsr(inc = variable, weights = pesoano, breakdown = by, data = d)
     suppressMessages({
     value <- estimation[[2]]
     value_total <- data.frame(dplyr::bind_cols("Total",estimation[[1]]))
     names(value_total) <- names(value)
     estimation <- dplyr::bind_rows(value, value_total)
    })
  } else if (is.null(by) & is.character(ids)) {
    estimation <- laeken::qsr(inc = variable, weights = pesoano, design = estrato, cluster = ids, data = d)
    suppressMessages({
    estimation <- data.frame(estimation[[1]])
    names(estimation) <- "value"
    })
  } else {
    estimation <- laeken::qsr(inc = variable, weights = pesoano, breakdown = by, design = estrato, cluster = ids, data = d)
    suppressMessages({
    value <- estimation[[2]]
    value_total <- data.frame(dplyr::bind_cols("Total",estimation[[1]]))
    names(value_total) <- names(value)
    estimation <- dplyr::bind_rows(value, value_total)
    })
  }
  return(estimation)

}
