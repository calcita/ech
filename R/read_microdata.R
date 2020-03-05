#' Read ECH from a local folder
#' @param folder Folder where are the files or be download
#' @param filename File name
#' @param format allows read data in rds, rdata, sav and dta
#' @importFrom fs path
#' @importFrom haven read_sav read_dta
#' @return unrar files from INE web and the respective data frame in tibble format
#' @export
#' @examples
#' read_microdata(folder = "/home/calcita/Escritorio/pruebaech/", filename = "HyP_2017_Terceros.sav")

read_microdata <- function(folder = NULL, filename = NULL, format = "rds"){

  # stopifnot(format %in% c("rds", "rdata", "sav", "dta"))
  # #stopifnot(is.character(filename))
  #
  # if (!is.null(folder)) {
  #   archivo <- fs::path(folder, paste0(filename, ".",format))
  # } else {
  #   archivo <- ech::toy_ech_2017_income
  # }
  #
  # # read spss file ---
  # if(format == "sav"){
  #   df <- haven::read_sav(archivo)
  # } else if(format == "dta"){
  #   df <- haven::read_dta(archivo)
  # } else if (format == "rds"){
  #   df <- readRDS(archivo)
  # } else{
  #   load(archivo)
  # }

}
