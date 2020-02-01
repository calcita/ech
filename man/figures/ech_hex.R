library(hexSticker) # https://github.com/GuangchuangYu/hexSticker

# add special text font
library(sysfonts)
font_add_google(name = "Roboto", family = "Roboto")

library(extrafont)
font_import()
loadfonts(device = "win")

### .png
sticker("https://cdn.imgbin.com/17/13/2/imgbin-capybara-rodent-rat-wikia-rat-0SuAbJNicCjwzZ0Ci7DtL7yQh.jpg", package = "ech",
        s_x = 1.03, s_y = 1, s_width = 1.5, s_height = 1.5, # ggplot image size and position
        h_fill = "#4dc0d1", h_color = "#4dc0d1", # hexagon
        url = "https://github.com/holatam/ech", u_color = "black", u_size = 6,
        spotlight = T, l_height = 17,
        filename = "./man/figures/ech_logo.png", dpi = 400)  # output name and resolution


### .svg
sticker("https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcTK-_QKgH49LY-a9Pj3yZdn8SewwMiJP65MmGqiD5L-whxXLlsl", package = "ech",
        s_x = 1.03, s_y = 1, s_width = 1.5, s_height = 1.5, # ggplot image size and position
        h_fill = "#4dc0d1", h_color = "#4dc0d1", # hexagon
        url = "https://github.com/holatam/ech", u_color = "black",
        spotlight = T, l_height = 17,
        filename = "./man/figures/ech_logo.svg")  # output name and resolution


