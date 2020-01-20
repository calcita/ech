#include "r_archive.h"
#include <Rcpp.h>

static int copy_data(struct archive* ar, struct archive* aw) {
  int r;
  const void* buff;
  size_t size;
  int64_t offset;

  for (;;) {
    r = archive_read_data_block(ar, &buff, &size, &offset);
    if (r == ARCHIVE_EOF) {
      return (ARCHIVE_OK);
    }
    if (r != ARCHIVE_OK) {
      Rcpp::stop("archive_read_data_block(): %s", archive_error_string(ar));
    }
  }
}

bool any_matches(const char* filename, Rcpp::CharacterVector filenames) {
  for (int i = 0; i < filenames.size(); ++i) {
    if (strcmp(filename, filenames[i]) == 0) {
      return true;
    }
  }
  return false;
}

// [[Rcpp::export]]
void archive_extract_(
    const std::string& archive_filename,
    Rcpp::CharacterVector filenames,
    size_t sz = 16384) {
  struct archive* a;
  struct archive* ext;
  struct archive_entry* entry;
  int flags;
  int r;

  /* Select which attributes we want to restore. */
  flags = ARCHIVE_EXTRACT_TIME;
  flags |= ARCHIVE_EXTRACT_PERM;
  flags |= ARCHIVE_EXTRACT_ACL;
  flags |= ARCHIVE_EXTRACT_FFLAGS;

  a = archive_read_new();
  archive_read_support_format_all(a);
  archive_read_support_filter_all(a);
  if ((r = archive_read_open_filename(a, archive_filename.c_str(), sz))) {
    Rcpp::stop("Could not open '%s'", archive_filename.c_str());
  }
  for (;;) {
    r = archive_read_next_header(a, &entry);
    if (r == ARCHIVE_EOF)
      break;
    if (r != ARCHIVE_OK) {
      Rcpp::stop("archive_read_next_header(): %s", archive_error_string(a));
    }
  }
  archive_read_close(a);
  archive_read_free(a);
}
