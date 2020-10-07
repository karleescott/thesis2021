library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)

usdata <- read.csv("/lfs/karlee_combined_data.csv")
View(usdata)
list <- c("3965a3","45c822","a04641","a3e88a","a4ec28","a67181")
startusdata <- usdata %>%
  filter(icao24 %in% list) %>%
  arrange(time)
View(startusdata)

firstdata <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-00.csv")
firstdata <- na.omit(firstdata)
firstdata <- firstdata %>%
  filter(lon >= -125 & lon <= -65 & lat >= 25 & lat <= 50)

# unzip the zipfile
unzip(zipfile = "thesis2021/states_21basic.zip", 
      exdir = 'states_21basic')

# load the shapefile 
map <- readOGR("states_21basic/states.shp")

#crop the portion needed
out <- crop(map, extent(-125, -65, 25, 50))

conversion <- fortify(out)
View(conversion)

library(ggplot2)

#flights that started at t = 0
ggplot(startusdata, aes(lon, lat, color=icao24)) +  
  ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, -65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)

#flights that occurred in the first hour
ggplot(firstdata, aes(lon, lat, color=icao24)) +  
  ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, -65) + ylim(25, 50) + geom_path() +
  theme(legend.position="none") + geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
