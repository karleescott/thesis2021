library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)

usdata <- read.csv("/lfs/karlee_combined_data.csv")
View(usdata)

list = c()
for (row in 1:nrow(usdata)){
  if(usdata[row,5] > (-78.7880 - 5) & usdata[row,5] < (-78.7880 + 5) & usdata[row,4] < (35.8801 + 5) & usdata[row,4] > (35.8801 - 5))
    list = list + usdata[row,3]
}

# unzip the zipfile
unzip(zipfile = "thesis2021/states_21basic.zip", 
      exdir = 'states_21basic')

# load the shapefile 
map <- readOGR("states_21basic/states.shp")

#crop the portion needed
out <- crop(map, extent(-125, -65, 25, 50))

conversion <- fortify(out)
View(conversion)

ggplot(startusdata, aes(lon, lat, color=icao24)) +  
  ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, -65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)