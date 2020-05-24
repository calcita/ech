#' Read ECH from a local folder
#' @param path Folder where are the files or be download
#' @importFrom fs path
#' @importFrom haven read_sav read_dta
#' @return an object called df
#' @export
#' @examples
#' \donttest{
#' read_microdata(folder = "/home/usuarie/Escritorio/HyP_2017_Terceros.sav")
#' }

read_microdata <- function(path = NULL){

  #stopifnot(is.character(filename))

  if (!is.null(path)) {
    format = tolower(fs::path_ext(path))
    if (!format %in% c("sav", "dta", "rds", "rdata"))
      stop(glue::glue("No es posible abrir un archivo con formato {format}"))
  } else {
    stop(glue::glue("No es posible abrir {path}"))
  }

  # read file
  if(format == "sav"){
    df <- haven::read_sav(path)
      #haven::zap_labels() %>%  haven::zap_formats() %>%  haven::zap_label()
  } else if(format == "dta"){
    df <- haven::read_dta(path)
      #haven::zap_labels() %>%  haven::zap_formats() %>%  haven::zap_label()
  } else if (format == "rds"){
    df <- readRDS(path)
  } else{
    load(archivo)
  }

}
