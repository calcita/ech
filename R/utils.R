#' get_ipc
#'
#' @return
#' @export
#'
#' @examples
get_ipc < function(){
  url <- "http://www.ine.gub.uy/c/document_library/get_file?uuid=2e92084a-94ec-4fec-b5ca-42b40d5d2826&groupId=10181"
  df <- readxl::read_xls("/home/calcita/Descargas/IPC gral var M_B10.xls")
  df <- df %>% slice(7, 10:999)
  names(df) <- df[1,]
  df <- df[-1,]
  df <- janitor::clean_names(df)
  df <- df %>%
    mutate(fecha = janitor::excel_numeric_to_date(as.numeric(as.character(mes_y_ano)), date_system = "modern"))
  df <- df %>% select(fecha, everything(), -mes_y_ano)
  ipc_base2010 <- df
  saveRDS(df, "ipc_base2010.rds")
  }
