# No Remotes ----
# Attachments ----
to_install <- c("assertthat", "curl", "dplyr", "fs", "geouy", "glue", "haven", "janitor", "labelled", "laeken", "purrr", "readxl", "rlang", "srvyr", "statar", "stringr", "survey", "tidyr")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }

