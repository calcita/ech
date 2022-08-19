## code to prepare `cba_cbna_mdeo` dataset goes here

cba_cbna_mdeo <- ech:::get_cba_cbna(folder = tempdir(), region = "M", sheet = 1)
usethis::use_data(cba_cbna_mdeo, overwrite = TRUE)
