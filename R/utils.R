#' This function allows you to get the IPC data
#' @family dwnld_read
#' @param folder ruta temporal para descargar el archivo
#' @importFrom readxl read_xls
#' @importFrom dplyr slice mutate select everything %>%
#' @importFrom janitor clean_names excel_numeric_to_date
#' @importFrom fs path
#' @importFrom rlang .data
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.

get_ipc <- function(folder = tempdir()){

  assertthat::assert_that(is.character(folder), msg = "Sorry... :( \n \t folder parameter must be character")
  assertthat::assert_that(.x = curl::has_internet(), msg = "No internet access was detected. Please check your connection.")

  u <- "http://www.ine.gub.uy/c/document_library/get_file?uuid=2e92084a-94ec-4fec-b5ca-42b40d5d2826&groupId=10181"
  f <- fs::path(folder, "IPC gral var M_B10.xls")
  if (identical(.Platform$OS.type, "unix")) {
    try(utils::download.file(u, f, mode = 'wb', method = 'wget'))
  } else {
    try(utils::download.file(u, f, mode = 'wb', method = 'libcurl'))
  }
  suppressMessages({
     df <- readxl::read_xls(f) %>%
       dplyr::slice(7, 10:999)
     names(df) <- df[1,]
     df <- df[-1,]
     df <- janitor::clean_names(df) %>%
      dplyr::mutate(fecha = janitor::excel_numeric_to_date(as.numeric(as.character(.data$mes_y_ano)), date_system = "modern")) %>%
      dplyr::select(.data$fecha, dplyr::everything(), -.data$mes_y_ano)
    ipc_base2010 <- df
  })
}

#' This function allows you to organize dates
#' @family utils
#' @param data data frame with an 'yy' variable for the year, and a 'mm' variable for the month
#' @importFrom dplyr mutate case_when
#' @return data.frame
#' @export
# @examples
# df <- dates_ech(data = )

dates_ech <- function(data) {
  data %>% dplyr::mutate(
    mm = dplyr::case_when(mm == "enero" ~ "01",
                          mm == "febrero" ~ "02",
                          mm == "marzo" ~ "03",
                          mm == "abril" ~ "04",
                          mm == "mayo" ~ "05",
                          mm == "junio" ~ "06",
                          mm == "julio" ~ "07",
                          mm == "agosto" ~ "08",
                          mm == "setiembre" ~ "09",
                          mm == "octubre" ~ "10",
                          mm == "noviembre" ~ "11",
                          TRUE ~ "12"),
    dd = "01",
    fecha = as.Date(paste(yy, mm, dd, sep = "-")))
}


#' This function allows you to get the IPC data
#' @family dwnld_read
#' @param folder temporal folder
#' @param region Montevideo ("M") or Interior ("I")
#' @param sheet sheet number. Default 1.
#'
#' @importFrom readxl read_xls
#' @importFrom dplyr slice mutate select everything filter_all slice any_vars bind_rows %>%
#' @importFrom tidyr gather separate
#' @importFrom janitor clean_names excel_numeric_to_date
#' @importFrom fs path
#' @return data.frame
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.

get_ipc_region <- function(folder = tempdir(), region, sheet = 1){
  assertthat::assert_that(.x = curl::has_internet(), msg = "No internet access was detected. Please check your connection.")
  assertthat::assert_that(is.character(folder), msg = "Sorry... :( \n \t folder parameter must be character")
  assertthat::assert_that(region %in% c("M", "I"), msg = "Sorry... :( \n \t region parameter must be 'M' for Montevideo or 'I' for Interior")

  if (region == "M") {
    u <- "http://www.ine.gub.uy/c/document_library/get_file?uuid=c7628833-9b64-44a4-ac97-d13353ee79ac&groupId=10181"
    f <- fs::path(folder, "IPC 3.1 indvarinc_ div M_B10_Mon.xls")
  }
  else {
    u <- "http://www.ine.gub.uy/c/document_library/get_file?uuid=61f9e884-781d-44be-9760-6d69f214b5b3&groupId=10181"
    f <- fs::path(folder, "IPC 3.2 indvarinc_ div M_B10_Int.xls")
  }
  if (identical(.Platform$OS.type, "unix")) {
    try(utils::download.file(u, f, mode = 'wb', method = 'wget'))
  } else {
    try(utils::download.file(u, f, mode = 'wb', method = 'libcurl'))
  }
  suppressMessages({
    df <- readxl::read_xls(f, sheet = sheet)
    df <- df[,-1] %>% janitor::remove_empty("rows")
    df <- dplyr::bind_rows(dplyr::slice(df, 1), dplyr::filter_all(df, dplyr::any_vars(grepl('ndice General', .))))
    names(df) <- df[1,]
    df <- df[-1,]
    df <- df %>% dplyr::select(dplyr::contains("20")) %>%
      janitor::clean_names()
    df <- df %>% tidyr::gather(fecha, indice, names(df)[1]:names(df)[ncol(df)], factor_key = TRUE) %>%
      tidyr::separate(fecha, into = c("mm", "yy"), sep = "_") %>%
      ech::dates_ech() %>%
      dplyr::select(fecha, indice)

    if (region == "M") {
      ipc_base2010_mdeo <- df
    }
    else{
      ipc_base2010_int <- df
    }
  })
}

#' This function allows you to get the CBA and CBNA data
#' @family dwnld_read
#' @param folder temporal folder
#' @param region Montevideo ("M"), Interior Urbano ("I"), Interior Rural ("R")
#' @param sheet sheet number. Default 1.
#'
#' @importFrom readxl read_xls
#' @importFrom dplyr slice mutate bind_cols %>%
#' @importFrom janitor clean_names excel_numeric_to_date remove_empty
#' @importFrom fs path
#' @importFrom purrr map_df
#' @importFrom curl has_internet
#'
#' @return data.frame
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.

get_cba_cbna <- function(folder = tempdir(), region, sheet = 1){
  assertthat::assert_that(.x = curl::has_internet(), msg = "No internet access was detected. Please check your connection.")
  assertthat::assert_that(is.character(folder), msg =  "Sorry... :( \n \t folder parameter must be character")
  assertthat::assert_that(region %in% c("M", "I", "R"), msg =  "Sorry... :( \n \t region parameter must be 'M' for Montevideo, 'I' for Interior urbano or 'R' for Interior rural")

  u <- "http://www.ine.gub.uy/c/document_library/get_file?uuid=1675e7d0-6fe0-49bd-bf3f-a46bd6334c0c&groupId=10181"
  f <- fs::path(folder, "CBA_LP_LI_M.xls")
  if (identical(.Platform$OS.type, "unix")) {
    try(utils::download.file(u, f, mode = 'wb', method = 'wget'))
  } else {
    try(utils::download.file(u, f, mode = 'wb', method = 'libcurl'))
  }
  suppressMessages({
    df <- readxl::read_xls(f, sheet = sheet)
    date <- df[9:nrow(df),1]
    names(date) <- "fecha"
    date <- date %>%
      dplyr::mutate(fecha = janitor::excel_numeric_to_date(as.numeric(as.character(fecha)), date_system = "modern")) %>%
      janitor::remove_empty("rows")
    df <- df[,-1] %>% janitor::remove_empty("rows") %>% janitor::remove_empty("cols")

    if (region == "M") {
      cba_mdeo <- df[, 1:3]
      names(cba_mdeo) <- df[2, 1:3]
      cba_mdeo <- cba_mdeo %>%
        dplyr::slice(-1:-3) %>%
        janitor::clean_names() %>%
        purrr::map_df(as.numeric) %>%
        dplyr::bind_cols(date,.)
    } else if (region == "I") {
      cba_int_urb <- df[, 4:6]
      names(cba_int_urb) <- df[2, 4:6]
      cba_int_urb <- cba_int_urb %>%
        dplyr::slice(-1:-3) %>%
        janitor::clean_names() %>%
        purrr::map_df(as.numeric) %>%
        dplyr::bind_cols(date,.)
    } else {
      cba_int_rur <- df[, 7:9]
      names(cba_int_rur) <- df[2, 7:9]
      cba_int_rur <- cba_int_rur %>%
        dplyr::slice(-1:-3) %>%
        janitor::clean_names() %>%
        purrr::map_df(as.numeric) %>%
        dplyr::bind_cols(date,.)
    }
  })
}


#' This function allows you to get the IPAB (Indice de precios de alimentos y bebidas) data
#' @family dwnld_read
#' @param folder temporal folder
#' @param sheet sheet number. Default 1
#' @importFrom readxl read_xls
#' @importFrom janitor remove_empty
#' @importFrom dplyr bind_rows slice filter_all bind_cols any_vars mutate_all
#' @importFrom tidyr fill
#' @importFrom curl has_internet
#'
#' @return data.frame
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.

get_ipab <- function(folder = tempdir(), sheet = 1){
  assertthat::assert_that(.x = curl::has_internet(), msg = "No internet access was detected. Please check your connection.")
  assertthat::assert_that(is.character(folder), msg =  "Sorry... :( \n \t folder parameter must be character")

  u <- "http://www.ine.gub.uy/c/document_library/get_file?uuid=c4b5efaa-cdd4-497a-ab78-e3138e4f08dc&groupId=10181"
  f <- fs::path(folder, "IPC Div M_B10.xls")
  if (identical(.Platform$OS.type, "unix")) {
    try(utils::download.file(u, f, mode = 'wb', method = 'wget'))
  } else {
    try(utils::download.file(u, f, mode = 'wb', method = 'libcurl'))
  }
  suppressMessages({
    df <- readxl::read_xls(f, sheet = sheet)
    df <- df[,-1:-2] %>% janitor::remove_empty("rows")
    df <- dplyr::bind_rows(dplyr::slice(df, 1), dplyr::filter_all(df, dplyr::any_vars(grepl(c('Divisiones'), .))), dplyr::filter_all(df, dplyr::any_vars(grepl(c('Alimentos y Bebidas No Alcoh'), .))))
    df[,1] <- c("yy", "mm", "indice")
    df <- dplyr::bind_cols(t(df[1,]), t(df[2,]), t(df[3,]))
    names(df) <- df[1,]
    df <- df %>% dplyr::slice(-1) %>%
      janitor::remove_empty("rows") %>%
      tidyr::fill(yy) %>%
      dplyr::mutate_all(tolower) %>%
      ech::dates_ech() %>%
      dplyr::select(fecha, indice)
  })
}

#' This function allows you to get the IPAB (Indice de precios de alimentos y bebidas) data
#' @family dwnld_read
#' @param folder temporal folder
#' @param region Montevideo ("M"), Interior Urbano ("I")
#' @param sheet sheet number. Default 1
#' @importFrom readxl read_xls
#' @importFrom janitor remove_empty
#' @importFrom dplyr bind_rows slice filter_all bind_cols any_vars mutate
#' @importFrom tidyr drop_na separate
#' @return data.frame

get_ipab_region <- function(folder = tempdir(), region, sheet = 1){
  assertthat::assert_that(.x = curl::has_internet(), msg = "No internet access was detected. Please check your connection.")
  assertthat::assert_that(is.character(folder), msg = "Sorry... :( \n \t folder parameter must be character")
  assertthat::assert_that(region %in% c("M", "I"), msg = "Sorry... :( \n \t region parameter must be 'M' for Montevideo or 'I' for Interior")

  if (region == "M") {
    u <- "http://ine.gub.uy/c/document_library/get_file?uuid=c7628833-9b64-44a4-ac97-d13353ee79ac&groupId=10181"
    f <- fs::path(folder, "IPC 3.1 indvarinc_ div M_B10_Mon.xls")
  } else {
    u <- "http://ine.gub.uy/c/document_library/get_file?uuid=61f9e884-781d-44be-9760-6d69f214b5b3&groupId=10181"
    f <- fs::path(folder, "IPC 3.2 indvarinc_ div M_B10_Int.xls")
  }
  if (identical(.Platform$OS.type, "unix")) {
    try(utils::download.file(u, f, mode = 'wb', method = 'wget'))
  } else {
    try(utils::download.file(u, f, mode = 'wb', method = 'libcurl'))
  }
  suppressMessages({
    df <- readxl::read_xls(f, sheet = sheet)
    df <- df[,-1] %>% janitor::remove_empty("rows")
    df <- dplyr::bind_rows(dplyr::slice(df, 1), dplyr::filter_all(df, dplyr::any_vars(grepl(c('Alimentos y Bebidas No Alcoh'), .))))
    names(df) <- df[1,]
    df <- df %>% janitor::remove_empty("cols")
    df <- df[,-1]
    df <- t(df)
    df <- data.frame(df) %>%
      tidyr::drop_na() %>%
      tidyr::separate(X1, sep = " ", into = c("mm", "yy"))
    names(df) <- c("mm", "yy", "indice")
    df <- df %>% dplyr::mutate_all(tolower) %>%
      ech::dates_ech() %>%
      dplyr::select(fecha, indice)

    if (region == "M") {
      ipab_base2010_mdeo <- df
    }
    else{
      ipab_base2010_int <- df
    }
  })
}

#' This function allows you to calculate a deflator coefficient
#' @family income
#' @param base_month baseline month
#' @param base_year baseline year
#' @param index IPC or IPAB
#' @param level General index ('G'), Montevideo index ('M') or Interior index ('I')
#' @param df_year ECH year
#' @importFrom dplyr select slice mutate
#' @importFrom rlang .data
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.

deflate <- function(base_month = NULL,
                    base_year = NULL,
                    index = "IPC",
                    level = "G",
                    df_year = NULL) {

  assertthat::assert_that(level %in% c("G", "M", "I"), msg = "Sorry... :( \n \t level parameter must be 'G' for General, 'M' for Montevideo or 'I' for Interior")
  assertthat::assert_that(index %in% c("IPC", "IPAB"), msg = "Sorry... :( \n \t index parameter must be 'IPC' or 'IPAB'")

  if (nchar(as.character(base_month)) == 1) {
    base_month <- paste0("0", base_month)
  }

  if (index == "IPC" & level == "G") {
    df <- ech::ipc_base2010
  }  else if (index == "IPC" & level == "M") {
    df <- ech::ipc_base2010_mdeo
  } else if (index == "IPC" & level == "I") {
    df <- ech::ipc_base2010_int
  } else if (index == "IPAB" & level == "G") {
    df <- ech::ipab_base2010
  }  else if (index == "IPAB" & level == "M") {
    df <- ech::ipab_base2010_mdeo
  } else {
    df <- ech::ipab_base2010_int
  }

  mes_base <- df %>%
    dplyr::filter(fecha == paste0(base_year, "-", base_month, "-01")) %>%
    dplyr::select(indice) %>%
    as.numeric()

  rows1 <- which(df$fecha == paste0(as.numeric(df_year) - 1, "-",12, "-01"))
  rows2 <- which(df$fecha == paste0(df_year, "-",11, "-01"))

  indice <- df %>%
    dplyr::slice(rows1:rows2) %>%
    dplyr::select(indice)

  indice <- as.numeric(indice$indice)

  deflator <- dplyr::bind_cols(deflator = mes_base/indice, mes = 1:12)

}

#' This function allows you to get the Basket goods
#' @family income
#' @param data data.frame with the price of the basket of goods from Montevideo, Interior or Rural region
#' @param year the ECH year
#' @return data.frame
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#'
#' @examples
#' df <- basket_goods(data = ech::cba_cbna_mdeo, year = 2018)

basket_goods <- function(data = ech::cba_cbna_mdeo,
                         year = NULL){

  assertthat::assert_that(is.data.frame(data), msg = "Sorry... :( \n \t data parameter must be data.frame")

  #ech::cba_cbna_int, ech::cba_cbna_rur
    rows1 <- which(data$fecha == paste0(as.numeric(year) - 1, "-",12, "-01"))
    rows2 <- which(data$fecha == paste0(year, "-",11, "-01"))

    df <- data %>%
      dplyr::slice(rows1:rows2)

}

#' This function allows you to labelled variables
#' @family utils
#' @param data data frame
#' @importFrom dplyr select mutate
#' @importFrom haven is.labelled
#' @importFrom labelled to_factor
#' @return data.frame
#' @export
#' @examples
#' df <- unlabelled(data = ech::toy_ech_2018)

unlabelled <- function(data = NULL){
  assertthat::assert_that(is.data.frame(data), msg = "Sorry... :( \n \t data parameter must be data.frame")
  d <- data %>% dplyr::mutate_if(haven::is.labelled, labelled::to_factor) #%>%
    #dplyr::mutate_if(is.factor, as.character)

}

#' This function allows you to calculate age groups
#' @family demographic
#' @param data data.frame
#' @param cut breaks points to cut a numeric variable
#' @param e27 Variable name of age
#' @return data.frame
#' @importFrom dplyr mutate
#' @importFrom haven labelled
#' @importFrom labelled var_label
#' @export
#'
#' @examples
#' toy_ech_2018 <- age_groups(data = ech::toy_ech_2018, cut = c(0, 4, 11, 17, 24))

age_groups <- function(data = ech::toy_ech_2018,
                       cut = c(0, 4, 11, 17, 24),
                       e27 = "e27") {

  assertthat::assert_that(is.data.frame(data), msg = glue:glue("Sorry... :( \n \t data parameter must be data.frame"))
  assertthat::assert_that(is.numeric(cut), msg = glue:glue("Sorry... :( \n \t cut parameter must be a numeric vector"))
  assertthat::assert_that(e27 %in% names(data), msg =  glue:glue("Sorry... :( \n \t {e27} is not in data"))

  if (min(dplyr::pull(data[, e27])) < min(cut)) {
    cut <- c(min(dplyr::pull(data[, e27])), cut)
  }
  if (max(dplyr::pull(data[, e27])) > max(cut)) {
    cut <- c(cut, max(dplyr::pull(data[, e27])))
  }

  data <- data %>% dplyr::mutate(age_groups = cut(e27, breaks = cut, include.lowest = TRUE, ordered_result = TRUE, labels = FALSE),
                                 age_groups = haven::labelled(age_groups, label = "Grupos de edad"))

  return(data)
}

#'  This function allows you to fix ht11 from 2013 to 2015
#' @family income
#' @param data data.frame
#' @param year survey year
#' @param ht11 Variable name of ht11
#' @param numero Variable name of numero
#'
#' @return data.frame
#' @export
#'
#' @examples
#' toy_ech_2018 <- organize_ht11(data = ech::toy_ech_2018, year = 2018)

organize_ht11 <- function(data, year, ht11 = "ht11", numero = "numero") {

  assertthat::assert_that(is.data.frame(data), msg = glue:glue("Sorry... :( \n \t data parameter must be data.frame"))
  assertthat::assert_that(is.numeric(year), msg = glue:glue("Sorry... :( \n \t year parameter must be a numeric value"))
  assertthat::assert_that(ht11 %in% names(data), msg =  glue:glue("Sorry... :( \n \t {ht11} is not in data"))
  assertthat::assert_that(numero %in% names(data), msg =  glue:glue("Sorry... :( \n \t {numero} is not in data"))

  if (year %in% 2013:2015) {
    data <- data %>%
      dplyr::group_by(numero) %>%
      dplyr::mutate(ht11 = max(ht11)) %>%
      dplyr::ungroup()
  }
  return(data)
}

# #' This function allows you to get the CIIU data
# #' @family dwnld_read
# #' @param folder temp folder
# #' @param version by default the last ciiu version
# #' @importFrom utils read.csv
# #' @importFrom pdftables convert_pdf
# #' @importFrom rstudioapi askForSecret
# #' @export
# #' @details
# #' Disclaimer: This script is not an official INE product.
# #' Aviso: El script no es un producto oficial de INE.
#
# get_ciiu <- function(folder = tempdir(),
#                      version = 4){
#   assertthat::assert_that(is.character(folder), msg = "Sorry... :( \n \t folder parameter must be character")
#   assertthat::assert_that(is.numeric(version), msg = "Sorry... :( \n \t version parameter must be numeric")
#   assertthat::assert_that(.x = curl::has_internet(), msg = "No internet access was detected. Please check your connection.")
#   u <- "http://www.ine.gub.uy/documents/10181/33330/CORRESPONDENCIA+CIUU4+A+CIUU3.pdf/623c43cb-009c-4da9-b48b-45282745063b"
#   f <- fs::path(folder, "ciiu4.pdf")
#   if (identical(.Platform$OS.type, "unix")) {
#     try(utils::download.file(u, f, mode = 'wb', method = 'wget'))
#   } else {
#     try(utils::download.file(u, f, mode = 'wb', method = 'libcurl'))
#   }
#   key <- rstudioapi::askForSecret("api_key")
#   pdftables::convert_pdf(f, "ciiu4.csv",api_key = key)
#   df <- read.csv("ciiu4.csv")
#   df <- df[,-3]
#   names(df) <- c("ciiu_4","description", "ciiu_3")
#   df <- df[-1,]
#   df[] <- lapply(df, textclean::replace_non_ascii)
#   ciiu4 <- df
# }


#' Pipe operator
#' See \code{dplyr::\link[dplyr]{\%>\%}} for details.
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL

#' add_geom
#' See \code{geouy::\link[geouy]{add_geom}} for details.
#' @name add_geom
#' @rdname add_geom
#' @keywords internal
#' @export
#' @importFrom geouy add_geom
NULL

#' plot_geouy
#' See \code{geouy::\link[geouy]{plot_geouy}} for details.
#' @name plot_geouy
#' @rdname plot_geouy
#' @keywords internal
#' @export
#' @importFrom geouy plot_geouy
NULL
