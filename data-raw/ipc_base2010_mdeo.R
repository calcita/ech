## code to prepare `ipc_base2010_mdeo` dataset goes here

df1 <- get_ipc_region(folder = tempdir(), region = "M", sheet = 1)
df2 <- get_ipc_region(folder = tempdir(), region = "M", sheet = 2)
df3 <- get_ipc_region(folder = tempdir(), region = "M", sheet = 3)

ipc_base2010_mdeo <- dplyr::bind_rows(df1, df2, df3)
usethis::use_data(ipc_base2010_mdeo, overwrite = TRUE)
