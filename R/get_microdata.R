#' Download and read ECH from INE website
#' @param year allows download data from 2011 to 2018. Default the last year
#' @param folder Folder where are the files or be download
#' @param toR write data frame in R format and delete download file
#' @importFrom utils download.file
#' @importFrom glue glue
#' @importFrom fs file_exists path_ext
#' @importFrom haven read_sav read_dta
#' @importFrom janitor clean_names
#' @importFrom archive archive_read
#' @return unrar files from INE web and the respective data frame in tibble format
#' @export
#' @examples
#' # download and read all surveys available
#' # Download and read ECH 2017
#' get_microdata(2017)

get_microdata <- function(year = NULL,
                          folder = getwd(),
                          toR = TRUE){

# download_ech

  # checks ----
  stopifnot(is.numeric(year) | is.null(year) | length(year) <= 1)
  #stopifnot(is.character(folder), length(folder) == 1)
  if (!is.character(folder) | length(folder) != 1) {
    message(glue::glue("Debe ingresar un directorio..."))
  }
  # warnings ----
  if (length(fs::dir_ls(folder, regexp = "\\.rar$")) != 0) {
    message(glue::glue("Existen en la carpeta otros archivos .rar que serán leídos..."))
  }
  # download ----
  try(dir.create(folder))

  all_years <- 2011:2018

  if (!is.null(year) & any(year %in% all_years) == FALSE) {
    stop("Por el momento ech solo funciona con microdatos de 2011 a 2018")
  }

  if (is.null(year)) {
    year <- max(all_years)
  }

  urls <- data.frame(yy = 2011:2018,
                           md_sav = fs::path("www.ine.gub.uy/c/document_library", c("get_file?uuid=cc986929-5916-4d4f-a87b-3fb20a169879&groupId=10181",
                                                                                       "get_file?uuid=144daa3d-0ebf-4106-ae11-a150511addf9&groupId=10181",
                                                                                       "get_file?uuid=9ddf38cc-99bb-4196-992b-77530b025237&groupId=10181",
                                                                                       "get_file?uuid=68cc1d11-e017-4a6d-a749-5a1e1a4a5306&groupId=10181",
                                                                                       "get_file?uuid=7c62ef78-0cc6-4fba-aae4-921ff5ceddd6&groupId=10181",
                                                                                       "get_file?uuid=715c873b-539f-4e92-9159-d38063270951&groupId=10181",
                                                                                       "get_file?uuid=e38ea53c-7253-4007-9f67-2f5f161eea91&groupId=10181",
                                                                                       "get_file?uuid=b63b566f-8d11-443d-bcd8-944f137c5aaf&groupId=10181"
                           )),
                           dic = fs::path("www.ine.gub.uy/c/document_library", c("get_file?uuid=54523778-5f53-4df1-a265-3ff520941bca&groupId=10181",
                                                                                    "get_file?uuid=8e8963a6-b9f2-47f3-abf5-119f988ad868&groupId=10181",
                                                                                    "get_file?uuid=055d37e5-d587-4ba7-8a0d-358cd99a9e24&groupId=10181",
                                                                                    "get_file?uuid=9c9612fd-3bc3-47c5-a684-06daaf25da6c&groupId=10181",
                                                                                    "get_file?uuid=6287d12b-1003-4402-86ba-a48866743d88&groupId=10181",
                                                                                    "get_file?uuid=54f72e41-e671-4bea-993c-631596e16883&groupId=10181",
                                                                                    "get_file?uuid=b60f247b-03cb-4bb1-b84b-5d7328479fe2&groupId=10181",
                                                                                    "get_file?uuid=73b6cc21-1bb0-483b-a463-819315b5fff3&groupId=10181")),
                           file = paste0(folder, "ech_", all_years, "_sav.rar"),
                     stringsAsFactors = FALSE)
  links <- urls %>% filter(yy %in% year)

    u <- links$md_sav
    f <- links$file
    y <- links$yy

    if (!file.exists(f)) {
      message(glue::glue("Intentando descargar ECH {y}..."))
      try(utils::download.file(u, f, mode = "wb", method = "libcurl"))
    } else {
      message(glue::glue("ECH {y} ya existe, se omite la descarga"))
    }

# read_ech
  archivo <- fs::dir_ls(folder, regexp = "\\.rar$")
  archivo <- archivo[which.max(file.info(archivo)$mtime)]
  ext <- fs::path_ext(archivo)
  compressed_formats <- c("zip", "rar")
  uncompressed_formats <- "sav"

  if (ext %in% c(compressed_formats, uncompressed_formats) != TRUE) {
    formats_string <- paste(c(compressed_formats, uncompressed_formats[length(uncompressed_formats) - 1]), collapse = ", ")
    formats_string <- paste(c(formats_string, uncompressed_formats[length(uncompressed_formats)]), collapse = " o ")
    stop(glue::glue("Los metadatos de {archivo} indica que este archivo no sirve. Asegurate de que el formato es {formats_string}."))
  }

  if (ext %in% compressed_formats) {
    message(glue::glue("Los metadatos de {archivo} indican que el formato comprimido es adecuado, intentando leer..."))
    d <- try(haven::read_sav(archive::archive_read(archivo)))
  }

  if (ext %in% uncompressed_formats) {
    message(glue::glue("Los metadatos de {archivo} indican que el formato no comprimido es adecuado, intentando leer..."))
    d <- haven::read_sav(archivo)
    }

  if (any(class(d) %in% "data.frame")) {
    message(glue::glue("{archivo} se pudo leer como tibble :-)"))
    d <- janitor::clean_names(d)
    return(d)
   } else {
    stop(glue::glue("{archivo} no se pudo leer como tibble :-("))
   }
  if (isTRUE(toR)) {
    # saveRDS(d, file = paste0(tools::file_path_sans_ext(archivo), ".Rds"))
    saveRDS(d, file = paste0("ECH_", year, ".Rds"))
    fs::file_delete(archivo)
  }
}
