library(tidyverse)
data <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-00.csv")
data <- na.omit(data)
View(data)
data1 <- data %>%
  filter(icao24 == "00d170")
data2 <- data %>%
  filter(icao24 == "e80450")
data3 <- data %>%
  filter(icao24 == "e495fc")



library("ggplot2")

col_scale <- function(palette = "Spectral", name = "", limits = NULL) {
  scale_colour_distiller(palette = palette,   # spectral colour scale
                         guide = "colourbar", # continuous colour bar
                         name = name,
                         limits = limits)
}

plot1 <- ggplot(data1) + # plot points
  geom_point(aes(x = lon,y = lat, # lon and lat
                 colour = time), # attribute color
             size = 2) + # make all points larger
  col_scale(name = "time") + # attach color scale
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  coord_fixed(xlim = c(152.7, 153.2),
              ylim = c(-26.5, -27)) + # zoom in
  theme_bw() # B&W theme
plot1

plot2 <- ggplot(data2) + # plot points
  geom_point(aes(x = lon,y = lat, # lon and lat
                 colour = time), # attribute color
             size = 2) + # make all points larger
  col_scale(name = "time") + # attach color scale
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  coord_fixed(xlim = c(174.2, 174.6),
              ylim = c(-35.0, -35.5)) + # zoom in
  theme_bw() # B&W theme
plot2

plot3 <- ggplot(data3) + # plot points
  geom_point(aes(x = lon,y = lat, # lon and lat
                 colour = time), # attribute color
             size = 2) + # make all points larger
  col_scale(name = "time") + # attach color scale
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  coord_fixed(xlim = c(-47.1, -47.6),
              ylim = c(-22.9, -23.4)) + # zoom in
  theme_bw() # B&W theme
plot3
