#' @importFrom glue collapse single_quote
collapse_quote_transformer <- function(code, envir) {
  collapse_re <- "[*]$"
  quote_re <- "^[|]"
  should_collapse <- grepl(collapse_re, code)
  should_quote <- !grepl(quote_re, code)
  code <- sub(collapse_re, "", sub(quote_re, "", code))
  res <- eval(parse(text = code, keep.source = FALSE), envir)
  if (should_quote) {
    res <- glue::single_quote(res)
  }
  if (should_collapse) {
    res <- glue::collapse(res, sep = ", ", last = " and ")
  }
  res
}


#' Error messages
#' @importFrom glue glue
#' @keywords internal
#' @export
assert <- function(msg, ...) {
  tests <- unlist(list(...))

  if (!all(tests)) {
    stop(structure(list(
      message = glue::glue(msg, .envir = parent.frame()),
      .call = sys.call(-1)
    ), class = c("error", "condition")))
  }
}

is_number <- function(x) {
  is.numeric(x) && length(x) == 1
}

is_string <- function(x) {
  is.character(x) && length(x) == 1
}

is_readable <- function(path) {
  is_string(path) &&
    file.exists(path)
  # file.access fails on some NFS, such as shared folders on virtualbox
  # https://stat.ethz.ch/pipermail/r-devel/2008-December/051461.html
  # file.access(path, mode = 4)[[1]] == 0
}



#' Construct a new archive
#'
#' This function retrieves metadata about files in an archive, it can be passed
#' to `archive_con()` to create a connection to read a specific file from the
#' archive.
#'
#' @param path File path to the archive.
#' @seealso [archive_read()], [archive_write()] to read and write archive files
#' using R connections, [archive_extract()], [archive_write_files()],
#' [archive_write_dir()] to add or extract files from an archive.
#' @examples
#' a <- archive(system.file(package = "archive", "extdata", "data.zip"))
#' a
#' @export
archive <- function(path) {
  assert("{path} is not a readable file path",
         is_readable(path))

  path <- normalizePath(path)

  res <- archive_metadata(path)
  class(res) <- c("archive", class(res))
  res
}

as_archive <- function(x) {
  if (inherits(x, "archive")) {
    return(x)
  }
  archive(x)
}



#' archive_read ech from zip/rar file
#' @param mode how to open the connection as used in \code{base::connections()}
#' @keywords internal
#' @export
archive_read <- function(archive, file = 1L, mode = "r", format = NULL, filter = NULL) {
  archive <- as_archive(archive)
  if (is_number(file)) {
    file <- archive$path[[file]]
  }

  assert(
    "`file` must be a length one character vector or numeric",
    length(file) == 1 && (is.character(file) || is.numeric(file))
  )

  assert(
    paste0("`file` {file} not found in `archive` {archive}"),
    file %in% archive$path
  )

  read_connection(attr(archive, "path"), mode = mode, file, archive_formats()[format], archive_filters()[filter])
}


