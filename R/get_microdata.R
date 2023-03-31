#### Archive extract ####

#' This is used so that unit tests can override this using \code{testthat::with_mock}.
#' @keywords internal
isWindows <- function() identical(.Platform$OS.type, "windows")

#' Tests if unrar or 7zip exist
#' @return
#' unrar or 7zip path if exist, or NULL
#' @author Tati Micheletti
#' @keywords internal
#' @rdname archiveExtractBinary
#' @name archiveExtractBinary

.archiveExtractBinary <- function() {
  possPrograms <- c("7z", "unrar")
  possPrograms <- unique(unlist(lapply(possPrograms, Sys.which)))
  extractSystemCallPath <- if (!all(possPrograms == "")) {
    possPrograms[nzchar(possPrograms)][1] # take first one if there are more than one
  } else {
    ""
  }
  if (!(isWindows())) {
    if (grepl("7z", extractSystemCallPath)) {
      SevenZrarExists <- system("apt -qq list p7zip-rar", intern = TRUE, ignore.stderr = TRUE)
      SevenZrarExists <- SevenZrarExists[grepl("installed", SevenZrarExists)]
      if (length(SevenZrarExists) == 0)
        message("It looks like you are using a non-Windows system; to extract .rar files, ",
                "you will need p7zip-rar, not just p7zip-full. Try: \n",
                "--------------------------\n",
                "sudo apt install p7zip-rar\n",
                "--------------------------\n"
        )
    }
  }

  if (identical(extractSystemCallPath, "")) {
    if (isWindows()) {
      extractSystemCallPath <- Sys.which("7z.exe")
      if (extractSystemCallPath == "") {
        message("prepInputs is looking for 'unrar' or '7z' in your system...")
        extractSystemCallPath <- list.files("C:/Program Files",
                                            pattern = "unrar.exe|7z.exe",
                                            recursive = TRUE,
                                            full.names = TRUE)
        if (extractSystemCallPath == "" || length(extractSystemCallPath) == 0) {
          extractSystemCallPath <- list.files(dirname(Sys.getenv("SystemRoot")),
                                              pattern = "unrar.exe|7z.exe",
                                              recursive = TRUE,
                                              full.names = TRUE)
          if (extractSystemCallPath == "" || length(extractSystemCallPath) == 0) {
            extractSystemCallPath <- NULL
            message(missingUnrarMess)
          } else {
            message("The extracting software was found in an unusual location: ", extractSystemCallPath, ".",
                    "If you receive an error when extracting the archive, please install '7zip' or 'unrar'",
                    " in 'Program Files' folder")
          }
        }
        extractSystemCallPath <- extractSystemCallPath[1]
      }
    } else {
      message(missingUnrarMess,
              "Try installing with, say: \n",
              "--------------------------\n",
              "sudo apt-get install p7zip p7zip-rar p7zip-full -y\n",
              "yum install p7zip p7zip-plugins -y\n",
              "--------------------------"
      )

    }
  }
  if (!exists("extractSystemCallPath", inherits = FALSE)) extractSystemCallPath <- NULL
  if (!nzchar(extractSystemCallPath)) extractSystemCallPath <- NULL

  return(extractSystemCallPath)
}

#' The known path for unrar or 7z
#' @family utils
#' @rdname unrarPath
#' @name unrarPath
.unrarPath <- NULL

missingUnrarMess <- "The archive is a 'rar' archive; your system does not have unrar or 7zip;\n"

knownInternalArchiveExtensions <- c("zip", "tar", "tar.gz", "gz")
knownSystemArchiveExtensions <- c("rar", "7z")
knownArchiveExtensions <- c(knownInternalArchiveExtensions, knownSystemArchiveExtensions)

#' Extract compressed archives
#' @family utils
#' @param archive.path Ruta de origen del archivo comprimido
#' @param dest.path Ruta destino del archivo descomprimido
#' @return No return value, called for side effects
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.

archive_extract <- function(archive.path = NULL, dest.path = NULL) {
  # busca archivo binario
  archiveExtractBinary <- .archiveExtractBinary()
  if (is.null(archiveExtractBinary))
    stop("unrar is not on this system; please install it")


  #z7path = shQuote('C:\\Program Files\\FusionInventory-Agent\\perl\\bin\\7z.exe')

  exe.path <- shQuote(archiveExtractBinary)

  file = paste('"', archive.path, '"',sep = '')
  cmd = paste(exe.path, ' e ', file, ' -ir!*.* -o', '"', dest.path, '"', sep = '')

  # unpack file
  system(cmd)
  message("El archivo fue descomprimido correctamente")
}



#### get_microdata ####

#' This function allows you to download and read ECH from INE website
#' @family dwnld_read
#' @param year allows download data from 2011 to 2019. Default the last year
#' @param folder Folder where are the files or be download
#' @param toR write data frame in R format and delete download file and unpack files
#' @importFrom utils download.file
#' @importFrom glue glue
#' @importFrom fs file_exists path_ext
#' @importFrom haven read_sav
#' @importFrom janitor clean_names
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#' @return unrar files from INE web and the respective data frame in tibble format
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#' @export

get_microdata <- function(year = NULL,
                          folder = tempdir(),
                          toR = TRUE){

  # checks ----
  assertthat::assert_that(.x = curl::has_internet(), msg = "No internet access was detected. Please check your connection.")
  stopifnot(is.numeric(year) | is.null(year) | length(year) <= 1)
  if (!is.character(folder) | length(folder) != 1) {
    message(glue::glue("Sorry... ;( \n \t You must enter a directory..."))
  }

  # download ----
  try(dir.create(folder))

  all_years <- 2011:2022

  if (!is.null(year) & any(year %in% all_years) == FALSE) {
    stop("Sorry... ;( \n \t At the moment ech only works for the period 2011 to 2019")
  }

  if (is.null(year)) {
    year <- max(all_years)
  }

    if (year %in% c(2021, 2022)) {
      links <- ech::urls_ine %>% dplyr::filter(yy == year)
      u1 <- links$md_sav
      y <- links$yy
      d <- utils::read.csv(u1)
    } else {
      urls <- ech::urls_ine %>% dplyr::mutate(file = paste0(folder, "/ech_", all_years, "_sav.rar"),
                                              file_extra = paste0(folder, "/upm_", all_years, "_sav.rar"))
      links <- urls %>% dplyr::filter(.data$yy %in% year)

      u1 <- links$md_sav
      f1 <- links$file
      y <- links$yy
      u2 <- links$upm_sav
      f2 <- links$file_extra



      if (!file.exists(f1)) {
        message(glue::glue("Trying to download ECH {y}..."))
        try(utils::download.file(u1, f1, mode = "wb", method = "libcurl"))
      } else {
        message(glue::glue("ECH {y} already exists, the download is omitted"))
      }

      # read----
      archivo <- fs::dir_ls(folder, regexp = "\\.rar$")
      archivo <- archivo[which.max(file.info(archivo)$mtime)]
      ext <- fs::path_ext(archivo)
      compressed_formats <- c("zip", "rar")
      uncompressed_formats <- "sav"

      if (ext %in% c(compressed_formats, uncompressed_formats) != TRUE) {
        formats_string <- paste(c(compressed_formats, uncompressed_formats[length(uncompressed_formats) - 1]),
                                collapse = ", ")
        formats_string <- paste(c(formats_string, uncompressed_formats[length(uncompressed_formats)]),
                                collapse = " o ")
        stop(glue::glue("The metadata in {archivo} indicates that this file is not useful. \n \t Make sure the format is {formats_string}."))
      }

      if (ext %in% compressed_formats) {
        message(glue::glue("The metadata in {archivo} indicates that this file is useful. \n \t Trying to read..."))
        try(archive_extract(archive.path = archivo, dest.path = folder))
        descomprimido <- fs::dir_ls(folder, regexp = "\\.sav$")
        descomprimido <- descomprimido[(stringr::str_detect(descomprimido, "HyP") == T |
                                          stringr::str_detect(descomprimido, "HYP") == T |
                                          stringr::str_detect(descomprimido, "FUSIONADO") == T |
                                          stringr::str_detect(descomprimido, "Fusionado") == T) &
                                         stringr::str_detect(descomprimido, as.character(year)) == T]
        d <- try(haven::read_sav(descomprimido))
      }

      if (ext %in% uncompressed_formats) {
        message(glue::glue("The metadata in {archivo} indicates that the uncompressed format is suitable,  \n \t Trying to read..."))
        d <- haven::read_sav(archivo)
      }

      if (any(class(d) %in% "data.frame")) {
        message(glue::glue("{archivo} could be read as tibble :-)"))
        d <- janitor::clean_names(d)
      } else {
        stop(glue::glue("{archivo} could not be read as tibble :-("))
      }
    }
  # standarize names
  names(d) <- tolower(names(d))

  if(y %in% 2018:2019){
    try(utils::download.file(u2, f2, mode = "wb", method = "libcurl"))

    # read----
    archivo <- fs::dir_ls(folder, regexp = "\\.rar$")
    archivo <- archivo[which.max(file.info(archivo)$mtime)]
    ext <- fs::path_ext(archivo)

    if (ext %in% c(compressed_formats, uncompressed_formats) != TRUE) {
      formats_string <- paste(c(compressed_formats, uncompressed_formats[length(uncompressed_formats) - 1]),
                              collapse = ", ")
      formats_string <- paste(c(formats_string, uncompressed_formats[length(uncompressed_formats)]),
                              collapse = " o ")
      stop(glue::glue("The metadata in {archivo} indicates that this file is not useful. \n \t Make sure the format is {formats_string}."))
    }

    if (ext %in% compressed_formats) {
      try(archive_extract(archive.path = archivo, dest.path = folder))
      descomprimido <- fs::dir_ls(folder, regexp = "\\.sav$")
      descomprimido <- descomprimido[stringr::str_detect(descomprimido, "UPM") == T]
      upm <- try(haven::read_sav(descomprimido))
    }

    if (ext %in% uncompressed_formats) {
      message(glue::glue("The metadata in {archivo} indicates that the uncompressed format is suitable,  \n \t Trying to read..."))
      upm <- haven::read_sav(archivo)
    }

    if (any(class(upm) %in% "data.frame")) {
      upm <- janitor::clean_names(upm)
    } else {
      stop(glue::glue("{archivo} could not be read as tibble :-("))
    }

    # standarize names
    names(upm) <- tolower(names(upm))

    d <- upm %>% dplyr::select(numero, dplyr::starts_with("upm"), dplyr::starts_with("estrato")) %>%
      dplyr::left_join(d, ., by = "numero")
  }

  # delete save, dbf y rar files
  sav <- fs::dir_ls(folder, regexp = "\\.sav$")
  try(fs::file_delete(sav))
  rar <- fs::dir_ls(folder, regexp = "\\.rar$")
  try(fs::file_delete(rar))
  dbf <- fs::dir_ls(folder, regexp = "\\.dbf$")
  try(fs::file_delete(dbf))

  # save ----
  if (isTRUE(toR)) {
    saveRDS(d, file = fs::path(folder, paste0("ECH_", year, ".Rds")))
    message(glue::glue("The ech {year} has been saved in R format"))
  }

#  try(fs::dir_delete(folder))

  return(d)

}



#### read_microdata ####

#' This function allows you to read ECH from a local folder
#' @family dwnld_read
#' @param path Folder where are the files or be download
#' @importFrom fs path
#' @importFrom haven read_sav read_dta
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @return an object called df
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
# #' @examples
# #' path <- system.file("data", "toy_ech_2018.rda", package = "ech")
# #' read_microdata(path)


read_microdata <- function(path = NULL){

  assertthat::assert_that(is.character(path), msg = "Sorry... :( \n \t folder parameter must be character")

  if (!is.null(path)) {
    format = tolower(fs::path_ext(path))
    if (!format %in% c("sav", "dta", "rds", "rda", "rdata"))
      stop(glue::glue("It is not possible to open a file with the format {format}"))
  } else {
    stop(glue::glue("It is not possible to open {path}"))
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
    load(path)
  }

  # standarize names
  names(df) <- tolower(names(df))

}




#### get_marco ####

#' This unction allows you to download and read the sampling frame from INE website
#' @family dwnld_read
#' @param year allows download data of 2011, 2004, 1996 or 1985. Default 2011
#' @param folder Folder where are the files or be download
#' @param toR write data frame in R format and delete download file and unpack files
#' @importFrom utils download.file
#' @importFrom glue glue
#' @importFrom fs file_exists path_ext
#' @importFrom haven read_sav
#' @importFrom janitor clean_names
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#' @return unrar files from INE web and the respective data frame in tibble format
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.

# get_marco <- function(year = NULL, folder = tempdir(), toR = TRUE){
#
#   # checks ----
#   stopifnot(is.numeric(year) | is.null(year) | length(year) <= 1)
#   if (!is.character(folder) | length(folder) != 1) {
#     message(glue::glue("Sorry... ;( \n \t You must enter a directory..."))
#   }
#
#   # download ----
#   try(dir.create(folder))
#
#   all_years <- c(2011, 2004, 1996, 1985)
#
#   if (!is.null(year) & any(year %in% all_years) == FALSE) {
#     stop("At the moment there are only census data for 2011, 2004, 1996 or 1985")
#   }
#
#   if (is.null(year)) {
#     year <- max(all_years)
#   }
#
#   urls <- data.frame(yy = c(2011, 2004, 1996, 1985),
#                      marco = fs::path("www.ine.gub.uy/c/document_library",
#                                        c("get_file?uuid=cc26162b-0b18-4ab2-a8ce-a5cac1679a3b&groupId=10181",
#                                          "get_file?uuid=4861b0f3-4ed6-4a37-9f30-ac77fb9f66e5&groupId=10181",
#                                          "get_file?uuid=2d13324d-ffe0-4b32-9692-3147d03335bd&groupId=10181",
#                                          "get_file?uuid=13e2aa6e-c6fc-4f40-afe5-bcb9c60b1527&groupId=10181")),
#                                      file = paste0(folder, "/marco_", all_years, "_sav.zip"),
#                      stringsAsFactors = FALSE)
#   links <- urls %>% dplyr::filter(.data$yy %in% year)
#
#   u <- links$marco
#   f <- links$file
#   y <- links$yy
#
#   if (!file.exists(f)) {
#     message(glue::glue("Trying to download Census data for year {y}..."))
#     try(utils::download.file(u, f, mode = "wb", method = "libcurl"))
#   } else {
#     message(glue::glue("Census data for year {y} already exists, the download is omitted"))
#   }
#
#   # read----
#   archivo <- fs::dir_ls(folder, regexp = "\\.zip$")
#   archivo <- archivo[which.max(file.info(archivo)$mtime)]
#   try(archive_extract(archive.path = archivo, dest.path = folder))
#   if(y %in% c(2004, 1996, 1985)){
#     descomprimido <- fs::dir_ls(folder, regexp = "\\.xls$")
#     d <- try(readxl::read_xls(descomprimido))
#   } else {
#     archivo <- fs::dir_ls(folder, regexp = "\\.zip$")
#     archivo <- archivo[which.max(file.info(archivo)$mtime)]
#     try(archive_extract(archive.path = archivo, dest.path = folder))
#     descomprimido <- fs::dir_ls(folder, regexp = "\\.sav$")
#     d <- try(haven::read_sav(descomprimido))
#   }
#
#
#
#   if (any(class(d) %in% "data.frame")) {
#     message(glue::glue("{archivo} could be read as tibble :-)"))
#     d <- janitor::clean_names(d)
#   } else {
#     stop(glue::glue("{archivo} could not be read as tibble :-("))
#   }
#
#   # standarize names
#   names(d) <- tolower(names(d))
#
#   # save ----
#   if (isTRUE(toR)) {
#     saveRDS(d, file = fs::path(folder, paste0("marco_", year, ".Rds")))
#     message(glue::glue("The file has been saved in R format"))
#     sav <- fs::dir_ls(folder, regexp = "\\.sav$")
#     fs::file_delete(archivo)
#     fs::file_delete(sav)
#   }
#
#   return(d)
#   }

#' @rdname get_marco
#' @export
# get_sampling_frame <- get_marco
