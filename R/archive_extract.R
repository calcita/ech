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

