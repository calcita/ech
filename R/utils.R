#' get_ipc
#'
#' @param folder ruta temporal para descargar el archivo
#' @importFrom readxl read_xls
#' @importFrom dplyr slice mutate select everything
#' @importFrom janitor clean_names excel_numeric_to_date
#' @importFrom fs path
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @examples
#' \donttest{
#' get_ipc(folder = tempdir())
#' }

get_ipc <- function(folder = tempdir()){
  u <- "http://www.ine.gub.uy/c/document_library/get_file?uuid=2e92084a-94ec-4fec-b5ca-42b40d5d2826&groupId=10181"
  f <- fs::path(folder, "IPC gral var M_B10.xls")
  try(utils::download.file(u, f, mode = "wb", method = "libcurl"))
  df <- readxl::read_xls(f)
  df <- df %>% dplyr::slice(7, 10:999)
  names(df) <- df[1,]
  df <- df[-1,]
  df <- janitor::clean_names(df)
  df <- df %>%
     dplyr::mutate(fecha = janitor::excel_numeric_to_date(as.numeric(as.character(.data$mes_y_ano)), date_system = "modern"))
  df <- df %>% dplyr::select(.data$fecha, dplyr::everything(), -.data$mes_y_ano)
  ipc_base2010 <- df
}

#' Title
#'
#' @param folder ruta temporal para descargar el archivo
#' @param region Montevideo ("M") o Interior ("I")
#' @param sheet numero de hoja de la planilla
#'
#' @importFrom readxl read_xls
#' @importFrom dplyr slice mutate select everything filter_all slice any_vars bind_rows
#' @importFrom tidyr gather separate
#' @importFrom janitor clean_names excel_numeric_to_date
#' @importFrom fs path
#' @importFrom magrittr %>%
#' @return data.frame
#' @export
#'
#' @examples
#' \donttest{
#' get_ipc_region(folder = tempdir(), region = "M")
#' }
#'
get_ipc_region <- function(folder = tempdir(), region = "M", sheet = NULL){

  if (region == "M") {
    u <- "http://www.ine.gub.uy/c/document_library/get_file?uuid=c7628833-9b64-44a4-ac97-d13353ee79ac&groupId=10181"
    f <- fs::path(folder, "IPC 3.1 indvarinc_ div M_B10_Mon.xls")
  }
  else {
    u <- "http://www.ine.gub.uy/c/document_library/get_file?uuid=61f9e884-781d-44be-9760-6d69f214b5b3&groupId=10181"
    f <- fs::path(folder, "IPC 3.2 indvarinc_ div M_B10_Int.xls")
  }

  try(utils::download.file(u, f, mode = "wb", method = "libcurl"))
  df <- readxl::read_xls(f, sheet = sheet)
  df <- df[,-1] %>% janitor::remove_empty("rows")
  df <- dplyr::bind_rows(slice(df, 1), dplyr::filter_all(df, dplyr::any_vars(grepl('ndice General', .))))
  names(df) <- df[1,]
  df <- df[-1,]
  df <- df %>% dplyr::select(dplyr::contains("20"))
  df <- janitor::clean_names(df)
  df <- tidyr::gather(df, fecha, indice, names(df)[1]:names(df)[ncol(df)], factor_key = TRUE)
  df <- df %>%
    tidyr::separate(fecha, into = c("mm", "yy"), sep ="_") %>%
    dplyr::mutate(
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
    fecha = as.Date(paste(yy, mm, dd, sep ="-"))
  )
  df <- df %>% dplyr::select(fecha, indice)

  if (region == "M") {
    ipc_base2010_mdeo <- df
  }
  else{
    ipc_base2010_int <- df
  }
}

#' deflate
#'
#' @param base_month mes base
#' @param base_year anio base
#' @param ipc IPC a nivel nacional ('G'), IPC para Montevideo ('M') e IPC para Interior ('I')
#' @param df_year anio del data frame
#'
#' @importFrom dplyr select slice mutate
#' @importFrom rlang .data
#' @export
#' @examples
#' \donttest{
#' deflate(base_month = "06", base_year = "2016", df_year = "2018")
#' }

deflate <- function(base_month = NULL,
                    base_year = NULL,
                    ipc = "G",
                    df_year = NULL) {

  if (ipc == "G") {
     df <- ech::ipc_base2010
   }  else if (ipc == "M"){
    df <- ech::ipc_base2010_mdeo
   } else {
     df <- ech::ipc_base2010_int
  }

     mes_base <- df %>%
       dplyr::filter(.data$fecha == paste0(base_year, "-", base_month, "-01")) %>%
       dplyr::select(.data$indice) %>% as.numeric

     rows1 <- which(df$fecha == paste0(as.numeric(df_year) - 1, "-",12, "-01"))
     rows2 <- which(df$fecha == paste0(df_year, "-",11, "-01"))

     deflate <- df %>%
       dplyr::slice(rows1:rows2) %>%
       dplyr::mutate(deflate = mes_base/as.numeric(.data$indice),
              mes = 1:12
       ) %>%
       dplyr::select(.data$deflate, .data$mes)
}

#' get_ciiu
#'
#' @param folder temp folder
#' @param version by default the last ciiu version
#' @importFrom utils read.csv
#' @importFrom pdftables convert_pdf
#' @importFrom rstudioapi askForSecret
#' @export

get_ciiu <- function(folder = tempdir(), version = 4){
  u <- "http://www.ine.gub.uy/documents/10181/33330/CORRESPONDENCIA+CIUU4+A+CIUU3.pdf/623c43cb-009c-4da9-b48b-45282745063b"
  f <- fs::path(folder, "ciiu4.pdf")
  try(utils::download.file(u, f, mode = "wb", method = "libcurl"))
  key <- rstudioapi::askForSecret("api_key")
  pdftables::convert_pdf(f, "ciiu4.csv",api_key = key)
  df <- read.csv("ciiu4.csv")
  df <- df[,-3]
  names(df) <- c("ciiu_4","description", "ciiu_3")
  df <- df[-1,]
  df[] <- lapply(df, textclean::replace_non_ascii)
  ciiu4 <- df
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# #' to_ascii
# #'
# #' @param x a column
# #' @param upper logic
# #'
# #' @importFrom stringr str_replace_all
# #' @return
# #' @export
# #' @examples
# #' \donttest{
# #' d <- lapply(dic, to_ascii)
# #' }
# to_ascii <- function(x, upper = T ){
#   x <- x %>% as.character() %>%
#     toupper() %>%
#     stringr::str_replace_all("Ñ", "NI") %>%
#     stringr::str_replace_all("Ó", "O") %>%
#     stringr::str_replace_all("Á", "A") %>%
#     stringr::str_replace_all("É", "E") %>%
#     stringr::str_replace_all("Í", "I") %>%
#     stringr::str_replace_all("Ú", "U")
#   if (!upper == T) x <- tolower(x)
#   x
# }
