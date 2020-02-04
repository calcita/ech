library(hexSticker) # https://github.com/GuangchuangYu/hexSticker

# plot uy
library(geouy); library(sf); library(ggplot2)
d <- load_geouy("Uruguay", crs = 4326)
d_s <- st_simplify(d, preserveTopology = TRUE, dTolerance = .1)

plot_d <-
  ggplot() +
  # geom_sf(data = d_s, fill = NA, size = .3, color = "#5796c905") +
  geom_sf(data = d_s, fill = NA, size = .4, color = "#4dc0d1") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  #  theme(legend.position = "none") +
  annotate("text", x = -54.7, y = -30.8, label = "ech", color = "#4dc0d1",
           size = 25, family = "Roboto", fontface = "bold", angle = -35) # (.png  size = 25)(.svg  size = 6)


# add special text font
library(sysfonts)
font.families.google()
font_add_google(name = "Gochi Hand", family = "gochi")
font_add_google(name = "Sanchez")

### .png
sticker("U:/carpincha2.png",
        package = "ech", p_size = 40, p_family = "Sanchez", s_x = 1, s_y = .75,
        s_width = .6, s_height = .4, h_color = "#a1c6c7",
        h_fill = "#74a9cf", url = "https://github.com/holatam/ech", u_color = "black",
        u_size = 5, filename = "./man/figures/ech_logo.png")
