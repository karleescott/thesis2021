library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)

usdata <- read.csv("/lfs/karlee_combined_data.csv")

# unzip the zipfile
unzip(zipfile = "thesis2021/states_21basic.zip", 
      exdir = 'states_21basic')

# load the shapefile 
map <- readOGR("states_21basic/states.shp")

#crop the portion needed
out <- crop(map, extent(-125, -65, 25, 50))

conversion <- fortify(out)

usdata_time <- usdata %>%
  filter(time == 1594598410)

plot1 <- ggplot(usdata_time, aes(lon, lat, color= factor(icao24))) +  
  ggtitle("Flight Locations on July 13th 2020 @ 0010") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_point() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
plot1

usdata_id <- usdata %>%
  filter(icao24 == 	"a35a92")

plot2 <- ggplot(usdata_id, aes(lon, lat,color= factor(time))) +  
  ggtitle("Flight Route of id a35a92") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_point(show.legend = FALSE) +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
plot2

lat <- 35.8801
lon <- -78.7880
data <- na.omit(usdata)
data <- data.frame(data)
data <- data %>%
  filter(lon >= -125 & lon <= -65 & lat >= 25 & lat <= 50)

#distance from RDU
distance <- c()
for(row in 1:nrow(data)){
  distance[row] <- sqrt((as.numeric(as.character(data[row,"lon"]))-(lon))^2 + (as.numeric(as.character(data[row,"lat"]))-(lat))^2)
}

data <- cbind(data, distance)

#find ids that pass through RDU (1 degree buffer)
list = c()
i = 1
for (row in 1:nrow(data)){
  if(data[row,"distance"] <= 1) {
    list[i] <- as.character(data[row,"icao24"])
    i <- i + 1
  }
}

#remove repeat ids, filter data to paths within 1 degree of RDU
res <- c()
i = 1
for (j in list){ 
  if (j %in% res){ 
  }
  else{
    res[i] <- j
    i <- i + 1
  }
}

res <- sort(res)
data <- data %>%
  filter(icao24 %in% res)

plot3 <- ggplot(data, aes(lon, lat, color= factor(icao24))) +  
  ggtitle("Flights within 1 degree of RDU") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path(show.legend = FALSE) +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
plot3
