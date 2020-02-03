library(hexSticker) # https://github.com/GuangchuangYu/hexSticker

# add special text font
library(sysfonts)
font_add_google(name = "BOLD", family = "BEBAS NEUE")

library(extrafont)
font_import()
loadfonts(device = "win")

### .png
sticker("U:/carpincha1.png",
        package = "ech", p_size = 20, s_x = 1, s_y = .75, s_width = .6, s_height = 0.7, h_color = "gray",
        h_fill = "#74a9cf", url = "https://github.com/holatam/ech", u_color = "black",
        u_size = 3, filename = "./man/figures/ech_logo.svg")
