## code to prepare `ipc_base2010_mdeo` dataset goes here

df1 <- get_ipc_region(folder = tempdir(), region = "M", sheet = 1)
df2 <- get_ipc_region(folder = tempdir(), region = "M", sheet = 2)
df3 <- get_ipc_region(folder = tempdir(), region = "M", sheet = 3)

ipc_base2010_mdeo <- dplyr::bind_rows(df1, df2, df3)

ipc_base2010_mdeo <- ech::ipc_base2010 %>%
  dplyr::filter(fecha >= "2005-01-01" & fecha <= "2010-12-01") %>%
  dplyr::select(fecha, indice) %>%
  dplyr::bind_rows(., ipc_base2010_mdeo)

usethis::use_data(ipc_base2010_mdeo, overwrite = TRUE)
