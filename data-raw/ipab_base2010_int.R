## code to prepare `ipab_base2010_int` dataset goes here

df1 <- ech:::get_ipab_region(folder = tempdir(), sheet = 1, region = "I")
df2 <- ech:::get_ipab_region(folder = tempdir(), sheet = 2, region = "I") %>% dplyr::slice(-37:-42)
df3 <- ech:::get_ipab_region(folder = tempdir(), sheet = 3, region = "I")

ipab_base2010_int <- dplyr::bind_rows(df1, df2, df3)
ipab_base2010_int[45, "fecha"] = "2014-09-01"
ipab_base2010_int[23, "fecha"] = "2012-11-01"
ipab_base2010_int[24, "fecha"] = "2012-12-01"

usethis::use_data(ipab_base2010_int, overwrite = TRUE)
