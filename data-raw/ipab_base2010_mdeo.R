## code to prepare `ipab_base2010_mdeo` dataset goes here

df1 <- get_ipab_region(folder = tempdir(), sheet = 1, region = "M")
df2 <- get_ipab_region(folder = tempdir(), sheet = 2, region = "M") %>% dplyr::slice(-37:-42)
df3 <- get_ipab_region(folder = tempdir(), sheet = 3, region = "M")

ipab_base2010_mdeo <- dplyr::bind_rows(df1, df2, df3)
ipab_base2010_mdeo[45, "fecha"] = "2014-09-01"

usethis::use_data(ipab_base2010_mdeo, overwrite = TRUE)
