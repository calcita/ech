# code to prepare `toy_ech_2018_income` dataset goes here

usethis::use_data("toy_ech_2018_income")

# random
toy_ech_2018_income <- ech::toy_ech_2018 %>% dplyr::select(numero, mes, ht11, ysvl, ht13, ht19, dpto, pesoano, estred13)

usethis::use_data(toy_ech_2018_income, overwrite = TRUE)
