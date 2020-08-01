## code to prepare `toy_ech_2018` dataset goes here

usethis::use_data("toy_ech_2018")

# random
d <- ech::get_microdata(year = 2018)
i <- unique(d$numero)
x <- sample(1:length(i), 300)
toy_ech_2018 <- d %>% filter(numero %in% x)

usethis::use_data(toy_ech_2018, overwrite = TRUE)
