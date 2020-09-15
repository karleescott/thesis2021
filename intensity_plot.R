library(tidyverse)
library(spatstat)
usdata <- read.csv("/lfs/karlee_combined_data.csv")
usdata1 <- usdata %>%
  filter(time == 1594598410)

x <- as.vector(usdata1$lon)
y <- as.vector(usdata1$lat)
P <- ppp(x, y, c(-125, -65), c(25, 50), unitname=c("degrees","degrees"))
P
summary(P)
plot(P)
#bw.nrd0 implements a rule-of-thumb for choosing the bandwidth of a Gaussian kernel density estimator. 
#It defaults to 0.9 times the minimum of the standard deviation and the interquartile range divided by 
#1.34 times the sample size to the negative one-fifth power (= Silverman's ‘rule of thumb’, Silverman 
#(1986, page 48, eqn (3.31))) unless the quartiles coincide when a positive result will be guaranteed.
plot(density(P, bw = "nrd0", kernel = "gaussian"), main = "Intensity Plot", xlab = "Longitude (degrees)", ylab = "Latitude (degrees)")

library(rgdal)
library(raster)
library(rgeos)
# unzip the file

unzip(zipfile = "C:/Users/Karlee Scott/OneDrive - West Point/AY 21-1/Thesis/states_21basic.zip", 
      exdir = 'states_21basic')

# load the data 
map <- readOGR("states_21basic/states.shp")

plot(map)
summary(map)

out <- crop(map, extent(-125, -65, 25, 50))
plot(out)


#https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2.html
intplot1 <- ggplot(usdata1, aes(x=lon, y=lat)) +
#default gaussian with bandwidth.nrd(x) "rule of thumb"   
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral", direction=-1) +
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
  scale_fill_distiller(palette= "Spectral", direction=-1) +
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

