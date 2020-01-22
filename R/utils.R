#' get_ipc
#'
#' @importFrom readxl read_xls
#' @import dplyr janitor
#' @return
#' @export
#'
#' @examples
get_ipc < function(){
  url <- "http://www.ine.gub.uy/c/document_library/get_file?uuid=2e92084a-94ec-4fec-b5ca-42b40d5d2826&groupId=10181"
  df <- readxl::read_xls("/home/calcita/Descargas/IPC gral var M_B10.xls")
  df <- df %>% dplyr::slice(7, 10:999)
  names(df) <- df[1,]
  df <- df[-1,]
  df <- janitor::clean_names(df)
  df <- df %>%
    dplyr::mutate(fecha = janitor::excel_numeric_to_date(as.numeric(as.character(mes_y_ano)), date_system = "modern"))
  df <- df %>% dplyr::select(fecha, dplyr::everything(), -mes_y_ano)
  ipc_base2010 <- df
#  saveRDS(df, "ipc_base2010.rds")
  }

# deflate <- function(base.month = base.month,
#                     base.year = base.year,
#                     ipc = "country") {
#   load("R/sysdata.rda")
#   if (ipc = "country") {
#     mes_base <- ipc_base2010 %>%
#       filter(fecha == paste0(base.year, "-",base.month, "-01")) %>%
#       select(indice) %>% as.numeric
#
#     rows1 <- which(ipc_base2010$fecha == paste0(base.year-1, "-",12, "-01"))
#     rows2 <- which(ipc_base2010$fecha == paste0(base.year, "-",11, "-01"))
#
#     # Calcula el deflactor
#     deflate <- ipc_base2010 %>%
#       slice(rows1:rows2) %>%
#       mutate(deflate = mes_base/as.numeric(indice),
#              mes = 1:12
#       ) %>%
#       select(deflate, mes)
#
#   } else {
#     # ipc mont int
#   }
#
# }
