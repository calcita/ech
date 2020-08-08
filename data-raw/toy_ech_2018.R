## code to prepare `toy_ech_2018` dataset goes here

usethis::use_data("toy_ech_2018")

# random
d <- ech::get_microdata(year = 2018)
i <- unique(d$numero)
x <- sample(i, 500)
toy_ech_2018 <- d[d$numero %in% x,]

usethis::use_data(toy_ech_2018, overwrite = TRUE)
