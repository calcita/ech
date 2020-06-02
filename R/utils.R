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
  saveRDS(df, "ipc_base2010.rds")
}

#' deflate
#'
#' @param base.month mes base
#' @param base.year anio base
#' @param ipc IPC a nivel nacional o Mdeo-Interior
#' @importFrom dplyr select slice mutate %>%
#' @importFrom rlang .data
#' @export
#' @examples
#' \donttest{
#' deflate(base.month = 6, base.year = 2016)
#' }

deflate <- function(base.month = base.month,
                     base.year = base.year,
                     ipc = "country") {
   system.file("R", "sysdata.rda", package = "ech")
#   if (ipc = "country") {
     mes_base <- ipc_base2010 %>%
       filter(.data$fecha == paste0(base.year, "-",base.month, "-01")) %>%
       select(.data$indice) %>% as.numeric

     rows1 <- which(ipc_base2010$fecha == paste0(base.year - 1, "-",12, "-01"))
     rows2 <- which(ipc_base2010$fecha == paste0(base.year, "-",11, "-01"))

     # Calcula el deflactor
     deflate <- ipc_base2010 %>%
       slice(rows1:rows2) %>%
       mutate(deflate = mes_base/as.numeric(.data$indice),
              mes = 1:12
       ) %>%
       select(.data$deflate, .data$mes)

#   } else {
#     # ipc mont int
#   }

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
