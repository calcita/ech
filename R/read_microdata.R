#' Read ECH from a local folder
#' @param folder Folder where are the files or be download
#' @param filename File name
#' @param format allows read data in rds, rdata, sav and dta
#' @importFrom glue glue
#' @importFrom fs file_exists path_ext
#' @importFrom haven read_sav read_dta
#' @importFrom janitor clean_names
#' @return unrar files from INE web and the respective data frame in tibble format
#' @export
#' @examples
#' read_microdata()

read_microdata <- function(folder = getwd(), filename = NULL, format = "rds"){

  stopifnot(format %in% c("rds", "rdata", "sav", "dta"))
  stopifnot(is.character(filename))

  archivo <- fs::path(folder, paste0(filename, ".",format))
  # read spss file ---
  if(format == "sav"){
    df <- haven::read_sav(archivo)
  } else if(format == "dta"){
    df <- haven::read_dta(archivo)
  } else if (format == "rds"){
    df <- readRDS(archivo)
  } else{
    load(archivo)
  }

}
