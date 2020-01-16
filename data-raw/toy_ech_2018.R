## code to prepare `toy_ech_2018` dataset goes here

usethis::use_data("toy_ech_2018")

# random
d <- haven::read_sav("/home/calcita/Desktop/pruebaech/HyP_2018_Terceros.sav")
x <- sample(1:108608, 1000)
toy_ech_2018 <- d %>% slice(x)

usethis::use_data(toy_ech_2018)
