## code to prepare `cba_cbna_rur` dataset goes here

cba_cbna_rur <- ech:::get_cba_cbna(folder = tempdir(), region = "R", sheet = 1)

usethis::use_data(cba_cbna_rur, overwrite = TRUE)
