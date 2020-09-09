library(tidyverse)

usdata <- read.csv("/lfs/karlee_combined_data.csv")
usdata1 <- usdata %>%
  filter(time == 1594598410)

#https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2.html
intplot1 <- ggplot(usdata1, aes(x=lon, y=lat) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  geom_path(data = map_data("state"), # add US states map
            aes(x = long, y = lat, group = group)) +
  coord_fixed(xlim = c(-125, -65),
              ylim = c(25, 50)) + # zoom in
  theme(legend.position='none')
intplot1

combinedplot <- ggplot(usdata1, aes(x=lon, y=lat) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  geom_path(data = map_data("state"), # add US states map
            aes(x = long, y = lat, group = group)) +
  coord_fixed(xlim = c(-125, -65),
              ylim = c(25, 50)) + # zoom in
  theme(legend.position='none') +
  geom_point()
combinedplot

