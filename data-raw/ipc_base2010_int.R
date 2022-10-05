## code to prepare `ipc_base2010_int` dataset goes here

df1 <- ech:::get_ipc_region(folder = tempdir(), region = "I", sheet = 1)
df2 <- ech:::get_ipc_region(folder = tempdir(), region = "I", sheet = 2) %>% dplyr::slice(1:36)
df3 <- ech:::get_ipc_region(folder = tempdir(), region = "I", sheet = 3)

ipc_base2010_int <- dplyr::bind_rows(df1, df2, df3)

ipc_base2010_int <- ech::ipc_base2010 %>%
  dplyr::filter(fecha >= "2005-01-01" & fecha <= "2010-12-01") %>%
  dplyr::select(fecha, indice) %>%
  dplyr::bind_rows(., ipc_base2010_int)

ipc_base2010_int[117, "fecha"] = as.Date("2014-09-01")

usethis::use_data(ipc_base2010_int, overwrite = TRUE)
