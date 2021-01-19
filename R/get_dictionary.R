#' This function allows you to download ECH dictionaries from INE website
#' @family dwnld_read
#' @param year allows download data from 2011 to 2019. Default the last year
#' @param folder Folder where are the files or be download
#' @importFrom utils download.file
#' @importFrom glue glue
#' @importFrom fs file_exists path_ext
#' @importFrom haven read_sav
#' @importFrom janitor clean_names
#' @importFrom dplyr filter mutate
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#' @return unrar files from INE web and the respective data frame in tibble format
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#' @export

get_dictionary <- function(year = NULL,
                          folder = tempdir()){

  # checks ----
  assertthat::assert_that(.x = curl::has_internet(), msg = "No internet access was detected. Please check your connection.")
  stopifnot(is.numeric(year) | is.null(year) | length(year) <= 1)
  if (!is.character(folder) | length(folder) != 1) {
    stop(glue::glue("Sorry... ;( \n \t You must enter a directory..."))
  }

  # download ----
  try(dir.create(folder))

  all_years <- 2011:2019

  if (!is.null(year) & any(year %in% all_years) == FALSE) {
    stop("Sorry... ;( \n \t At the moment ech only works for the period 2011 to 2019")
  }

  if (is.null(year)) {
    year <- max(all_years)
  }

  links <- ech::urls_ine %>% mutate(file = paste0(folder, "/dic_", all_years, ".xls")) %>% dplyr::filter(.data$yy %in% year)

  u1 <- links$dic
  f1 <- links$file
  y <- links$yy

  if (!file.exists(f1)) {
    message(glue::glue("Trying to download ECH dictionary {y}..."))
    try(utils::download.file(u1, f1, mode = "wb", method = "libcurl"))
  } else {
    message(glue::glue("ECH dictionary {y} already exists, the download is omitted"))
  }
}
