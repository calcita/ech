#' Download ECH from INE website
#' @param years si no se indica un anio, descarga todos los anios disponibles
#' @param folder si no se indica una carpeta, descarga en la carpeta de
#' trabajo o la raiz del proyecto
#' @param format si no se indica se descarga en formato .sav (SPSS), la otra
#' opción sería "dat"
#' @importFrom utils download.file
#' @importFrom glue glue
#' @return los archivos comprimidos de la encuesta ECH descargados desde el
#' sitio web del INE
#' @export
#' @examples
#' # descargar todas las encuestas disponibles
#' # download_ech()
#'
#' # descargar ECH 2017 en carpeta especifica
#' # download_ech(2017, "data-raw")
download_ech <- function(years = NULL, folder = getwd(), format = "sav") {

  # checks ----
  stopifnot(is.numeric(years) | is.null(years))
  stopifnot(is.character(folder), length(folder) == 1)

  # download ----
  try(dir.create(folder))

  all_years <- 2011:2018

  if (!is.null(years) & any(years %in% all_years) == FALSE) {
    stop("Por el momento ech solo funciona con microdatos de 2011 a 2017")
  }

  if (is.null(years)) {
    all_years <- years
  }

  urls <- data.frame(year = 2011:2018,
                           md_sav = fs::path("www.ine.gub.uy/c/document_library", c("get_file?uuid=cc986929-5916-4d4f-a87b-3fb20a169879&groupId=10181",
                                                                                       "get_file?uuid=144daa3d-0ebf-4106-ae11-a150511addf9&groupId=10181",
                                                                                       "get_file?uuid=9ddf38cc-99bb-4196-992b-77530b025237&groupId=10181",
                                                                                       "get_file?uuid=68cc1d11-e017-4a6d-a749-5a1e1a4a5306&groupId=10181",
                                                                                       "get_file?uuid=7c62ef78-0cc6-4fba-aae4-921ff5ceddd6&groupId=10181",
                                                                                       "get_file?uuid=715c873b-539f-4e92-9159-d38063270951&groupId=10181",
                                                                                       "get_file?uuid=e38ea53c-7253-4007-9f67-2f5f161eea91&groupId=10181",
                                                                                       "get_file?uuid=b63b566f-8d11-443d-bcd8-944f137c5aaf&groupId=10181"
                           )),
                           md_dat = fs::path("www.ine.gub.uy/c/document_library", c("get_file?uuid=7eecaefc-be12-4f5f-93a4-d53cc2a20070&groupId=10181",
                                                                                       "get_file?uuid=47ff59b2-ed30-408d-a94c-05220e75d39b&groupId=10181",
                                                                                       "get_file?uuid=de9bb13a-9740-4732-8826-10b3489999b7&groupId=10181",
                                                                                       "get_file?uuid=7aa5ba66-dc54-418d-85d9-9985c88ffde8&groupId=10181",
                                                                                       "get_file?uuid=ecdb10a5-c2a6-46ba-bc49-d539e0264bbd&groupId=10181",
                                                                                       "get_file?uuid=2043b2a5-d328-48a6-a65e-27be2a974924&groupId=10181",
                                                                                       "get_file?uuid=e30dd18d-c9df-4740-952d-f7ab9ef4f8e4&groupId=10181",
                                                                                       "get_file?uuid=32a696cf-38d9-46cb-ad16-8fea69262581&groupId=10181")),
                           dic = fs::path("www.ine.gub.uy/c/document_library", c("get_file?uuid=54523778-5f53-4df1-a265-3ff520941bca&groupId=10181",
                                                                                    "get_file?uuid=8e8963a6-b9f2-47f3-abf5-119f988ad868&groupId=10181",
                                                                                    "get_file?uuid=055d37e5-d587-4ba7-8a0d-358cd99a9e24&groupId=10181",
                                                                                    "get_file?uuid=9c9612fd-3bc3-47c5-a684-06daaf25da6c&groupId=10181",
                                                                                    "get_file?uuid=6287d12b-1003-4402-86ba-a48866743d88&groupId=10181",
                                                                                    "get_file?uuid=54f72e41-e671-4bea-993c-631596e16883&groupId=10181",
                                                                                    "get_file?uuid=b60f247b-03cb-4bb1-b84b-5d7328479fe2&groupId=10181",
                                                                                    "get_file?uuid=73b6cc21-1bb0-483b-a463-819315b5fff3&groupId=10181")),
                           file = paste0(folder, "/ech_", all_years, "_", format,".rar"),
                     stringsAsFactors = FALSE)
  links <- urls[urls$year %in% years, ]

  for (j in 1:nrow(links)) {
    u <- links[ifelse(format == "sav", 2, 3)][[j]]
    f <- links$file[[j]]
    y <- links$year[[j]]

    if (!file.exists(f)) {
      message(glue::glue("Intentando descargar ECH {y}..."))
      try(utils::download.file(u, f, mode = "wb"))
    } else {
      message(glue::glue("ECH {y} ya existe, se omite la descarga"))
    }
  }
}


#' Lee los archivos descargados de la ECH
#' @param archivo cadena de texto con la ruta de la encuesta en formato zip/rar o sav/dat
#' @importFrom glue glue
#' @importFrom fs file_exists path_ext
#' @importFrom haven read_sav read_dta
#' @importFrom janitor clean_names
#' @importFrom archive archive_read
#' @return la encuesta ECH en formato tibble
#' @examples
#' # datos de ejemplo en el paquete
#' read_ech(system.file(package = "ech", "extdata", "ech_2017_spss.rar"))
#'
#' # descargando los datos
#' # download_ech(2017, "data-raw")
#' # read_ech("data-raw/ech_2017_spss.rar")
#' @export


read_ech <- function(folder) {
  # checks ----
  if (!is.character(folder) | length(folder) != 1) {
     message(glue::glue("Debe ingresar un directorio..."))
   }
  archivo <- fs::dir_ls(folder)
  ext <- fs::path_ext(archivo)
  compressed_formats <- c("zip", "rar")
  uncompressed_formats <- c("sav", "dat")

  # read ----
  if (any(ext %in% c(compressed_formats, uncompressed_formats)) != TRUE) {
    formats_string <- paste(c(compressed_formats, uncompressed_formats[length(uncompressed_formats) - 1]), collapse = ", ")
    formats_string <- paste(c(formats_string, uncompressed_formats[length(uncompressed_formats)]), collapse = " o ")
    stop(glue::glue("Los metadatos de {archivo} indica que este archivo no sirve. Asegurate de que el formato es {formats_string}."))
  }

  if (any(ext %in% compressed_formats)) {
    message(glue::glue("Los metadatos de {archivo} indican que el formato comprimido es adecuado, intentando leer..."))
    d <- try(haven::read_sav(archive::archive_read(archivo)))
  }

  if (any(ext %in% uncompressed_formats)) {
    message(glue::glue("Los metadatos de {archivo} indican que el formato no comprimido es adecuado, intentando leer..."))
    if (ext == uncompressed_formats[[1]]) {
      d <- haven::read_sav(archivo)
    }
    if (ext == uncompressed_formats[[2]]) {
      d <- haven::read_dta(archivo)
    }
  }

  if (any(class(d) %in% "data.frame")) {
    message(glue::glue("{archivo} se pudo leer como tibble :-)"))
    d <- janitor::clean_names(d)
    return(d)
  } else {
    stop(glue::glue("{archivo} no se pudo leer como tibble :-("))
  }
}


#' Descarga y lee los archivos de la ECH
#' @param years si no se indica un anio, descarga todos los anios disponibles
#' @param folder si no se indica una carpeta, descarga en la carpeta temp
#' @param format si no se indica se descarga en formato .sav (SPSS), la otra
#' opción sería "dat"
#' @importFrom utils download.file
#' @importFrom glue glue
#' @importFrom fs file_exists path_ext
#' @importFrom haven read_sav read_dta
#' @importFrom janitor clean_names
#' @return la encuesta ECH en formato tibble
#' @examples
#' # Obtener la ECH 2017
#' get_microdata(2017)
#' @export

get_microdata <- function(years = NULL, folder = tempdir(), format = "sav"){
  read_ech(download_ech())
}

