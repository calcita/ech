## code to prepare `cba_cbna_int` dataset goes here

cba_cbna_int <- ech:::get_cba_cbna(folder = tempdir(), region = "I", sheet = 1)

usethis::use_data(cba_cbna_int, overwrite = TRUE)
