## code to prepare `ipab_base2010` dataset goes here

df1 <- get_ipab(folder = tempdir(), sheet = 1)
df2 <- get_ipab(folder = tempdir(), sheet = 2)

ipab_base2010 <- dplyr::bind_rows(df1, df2)

usethis::use_data(ipab_base2010, overwrite = TRUE)
