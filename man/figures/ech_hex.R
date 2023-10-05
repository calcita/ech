library(hexSticker) # https://github.com/GuangchuangYu/hexSticker
library(magick)     # Advanced image processing
library(sysfonts)   # font selection
library(showtext)
font_add_google("Dela Gothic One", "letra")
showtext_auto()

### .png
sticker("man/figures/capybara.png",
        package = "ech", p_size = 40, p_family = "letra", s_x = 1, s_y = .75,
        s_width = .6, s_height = .4, h_color = "#a1c6c7",
        h_fill = "#74a9cf", url = "https://github.com/rlabuy/ech", u_color = "black",
        u_size = 5, filename = "./man/figures/ech_logo.png")
