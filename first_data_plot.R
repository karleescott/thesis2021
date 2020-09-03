library(tidyverse)
data <- read.csv("C:/Users/Karlee Scott/OneDrive - West Point/AY 21-1/Thesis/states_2020-07-13-00.csv")
data <- na.omit(data)
data1 <- data %>%
  filter(icao24 == "00d170")

library("ggplot2")

col_scale <- function(palette = "Spectral", name = "", limits = NULL) {
  scale_colour_distiller(palette = palette,   # spectral colour scale
                         guide = "colourbar", # continuous colour bar
                         name = name,
                         limits = limits)
}

plot <- ggplot(data1) + # plot points
  geom_point(aes(x = lon,y = lat, # lon and lat
                 colour = time), # attribute color
             size = 2) + # make all points larger
  col_scale(name = "time") + # attach color scale
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  coord_fixed(xlim = c(152.7, 153.2),
              ylim = c(-26.5, -27)) + # zoom in
  theme_bw() # B&W theme
plot

