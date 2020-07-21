#### Archive extract ####

#' This is used so that unit tests can override this using \code{testthat::with_mock}.
#' @keywords internal
isWindows <- function() identical(.Platform$OS.type, "windows")

#' Tests if unrar or 7zip exist
#'
#' @return
#' unrar or 7zip path if exist, or NULL
#'
#' @author Tati Micheletti
#'
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
#' @rdname unrarPath
#' @name unrarPath
.unrarPath <- NULL

missingUnrarMess <- "The archive is a 'rar' archive; your system does not have unrar or 7zip;\n"

knownInternalArchiveExtensions <- c("zip", "tar", "tar.gz", "gz")
knownSystemArchiveExtensions <- c("rar", "7z")
knownArchiveExtensions <- c(knownInternalArchiveExtensions, knownSystemArchiveExtensions)

#' archive_extract
#'
#' @param archive.path Ruta de origen del archivo comprimido
#' @param dest.path Ruta destino del archivo descomprimido
#'
#' @export
#' @examples
#' \donttest{
#' archive_extract()
#' }

archive_extract <- function(archive.path = NULL, dest.path = NULL) {
  # busca archivo binario
  archiveExtractBinary <- .archiveExtractBinary()
  if (is.null(archiveExtractBinary))
    stop("unrar is not on this system; please install it")


  #z7path = shQuote('C:\\Program Files\\FusionInventory-Agent\\perl\\bin\\7z.exe')

  exe.path <- shQuote(archiveExtractBinary)
  # file = paste('"', 'U:/base femicidio.rar', '"',sep = '')

  file = paste('"', archive.path, '"',sep = '')
  cmd = paste(exe.path, ' e ', file, ' -ir!*.* -o', '"', dest.path, '"', sep = '')

  # unpack file
  system(cmd)
  print("El archivo fue descomprimido correctamente")
}



#### get_microdata ####

#' get_microdata
#'
#' Download and read ECH from INE website
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
#' @export

get_microdata <- function(year = NULL,
                          folder = tempdir(),
                          toR = TRUE){

  # checks ----
  stopifnot(is.numeric(year) | is.null(year) | length(year) <= 1)
  if (!is.character(folder) | length(folder) != 1) {
    message(glue::glue("Debe ingresar un directorio..."))
  }
  if (length(fs::dir_ls(folder, regexp = "\\.rar$")) != 0) {
    message(glue::glue("Existen en la carpeta otros archivos .rar que se van a leer..."))
  }

  # download ----
  try(dir.create(folder))

  all_years <- 2011:2019

  if (!is.null(year) & any(year %in% all_years) == FALSE) {
    stop("Por el momento ech solo funciona con microdatos de 2011 a 2019")
  }

  if (is.null(year)) {
    year <- max(all_years)
  }

  urls <- data.frame(yy = 2011:2019,
                     md_sav = fs::path("www.ine.gub.uy/c/document_library",
                                       c("get_file?uuid=cc986929-5916-4d4f-a87b-3fb20a169879&groupId=10181",
                                         "get_file?uuid=144daa3d-0ebf-4106-ae11-a150511addf9&groupId=10181",
                                         "get_file?uuid=9ddf38cc-99bb-4196-992b-77530b025237&groupId=10181",
                                         "get_file?uuid=68cc1d11-e017-4a6d-a749-5a1e1a4a5306&groupId=10181",
                                         "get_file?uuid=7c62ef78-0cc6-4fba-aae4-921ff5ceddd6&groupId=10181",
                                         "get_file?uuid=715c873b-539f-4e92-9159-d38063270951&groupId=10181",
                                         "get_file?uuid=e38ea53c-7253-4007-9f67-2f5f161eea91&groupId=10181",
                                         "get_file?uuid=b63b566f-8d11-443d-bcd8-944f137c5aaf&groupId=10181",
                                         "get_file?uuid=8c934d2a-ad67-4208-8f21-96989696510e&groupId=10181"
                     )),
                     dic = fs::path("www.ine.gub.uy/c/document_library",
                                    c("get_file?uuid=54523778-5f53-4df1-a265-3ff520941bca&groupId=10181",
                                      "get_file?uuid=8e8963a6-b9f2-47f3-abf5-119f988ad868&groupId=10181",
                                      "get_file?uuid=055d37e5-d587-4ba7-8a0d-358cd99a9e24&groupId=10181",
                                      "get_file?uuid=9c9612fd-3bc3-47c5-a684-06daaf25da6c&groupId=10181",
                                      "get_file?uuid=6287d12b-1003-4402-86ba-a48866743d88&groupId=10181",
                                      "get_file?uuid=54f72e41-e671-4bea-993c-631596e16883&groupId=10181",
                                      "get_file?uuid=b60f247b-03cb-4bb1-b84b-5d7328479fe2&groupId=10181",
                                      "get_file?uuid=73b6cc21-1bb0-483b-a463-819315b5fff3&groupId=10181",
                                      "get_file?uuid=800e3c63-5cbc-4842-ad00-745f801f9220&groupId=10181")),
                     file = paste0(folder, "/ech_", all_years, "_sav.rar"),
                     stringsAsFactors = FALSE)
  links <- urls %>% dplyr::filter(.data$yy %in% year)

  u <- links$md_sav
  f <- links$file
  y <- links$yy

  if (!file.exists(f)) {
    message(glue::glue("Intentando descargar ECH {y}..."))
    try(utils::download.file(u, f, mode = "wb", method = "libcurl"))
  } else {
    message(glue::glue("ECH {y} ya existe, se omite la descarga"))
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
    stop(glue::glue("Los metadatos de {archivo} indica que este archivo no sirve.
                    Asegurate de que el formato es {formats_string}."))
  }

  if (ext %in% compressed_formats) {
    message(glue::glue("Los metadatos de {archivo} indican que el formato comprimido es adecuado,
                       intentando leer..."))
    try(archive_extract(archive.path = archivo, dest.path = folder))
    descomprimido <- fs::dir_ls(folder, regexp = "\\.sav$")
    descomprimido <- descomprimido[(stringr::str_detect(descomprimido, "HyP") == T |
                                    stringr::str_detect(descomprimido, "Fusionado") == T) &
                                    stringr::str_detect(descomprimido, as.character(year)) == T]
    d <- try(haven::read_sav(descomprimido))
  }

  if (ext %in% uncompressed_formats) {
    message(glue::glue("Los metadatos de {archivo} indican que el formato no comprimido es adecuado,
                       intentando leer..."))
    d <- haven::read_sav(archivo)
  }

  if (any(class(d) %in% "data.frame")) {
    message(glue::glue("{archivo} se pudo leer como tibble :-)"))
    d <- janitor::clean_names(d)
  } else {
    stop(glue::glue("{archivo} no se pudo leer como tibble :-("))
  }

  # save ----
  if (isTRUE(toR)) {
    saveRDS(d, file = fs::path(folder, paste0("ECH_", year, ".Rds")))
    message(glue::glue("Se ha guardado el archivo en formato R"))
    sav <- fs::dir_ls(folder, regexp = "\\.sav$")
    fs::file_delete(archivo)
    fs::file_delete(sav)
  }

  return(d)
}



#### read_microdata ####

#' Read ECH from a local folder
#' @param path Folder where are the files or be download
#' @importFrom fs path
#' @importFrom haven read_sav read_dta
#' @details
#' Disclaimer: El script no es un producto oficial de INE.
#' @return an object called df
#' @export
# #' @examples
# #' path <- system.file("data", "toy_ech_2018.rda", package = "ech")
# #' read_microdata(path)


read_microdata <- function(path = NULL){

  #stopifnot(is.character(filename))

  if (!is.null(path)) {
    format = tolower(fs::path_ext(path))
    if (!format %in% c("sav", "dta", "rds", "rda", "rdata"))
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
    load(path)
  }
}
