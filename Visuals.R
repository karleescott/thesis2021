#Visuals
library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)

usdata <- read.csv("/lfs/karlee_combined_data.csv")

# unzip the zipfile
unzip(zipfile = "thesis2021/states_21basic.zip", 
      exdir = 'states_21basic')

# load the shapefile 
map <- readOGR("states_21basic/states.shp")

#crop the portion needed
out <- crop(map, extent(-125, -65, 25, 50))

conversion <- fortify(out)

#######all points at a specific time
time_data <- usdata %>%
  filter(time == 1594598410)

ggplot(time_data, aes(lon, lat)) +  
  xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + 
  xlim(-125, - 65) + ylim(25, 50) + geom_point(size = 1) +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)

######one plane over time
plane_data <- usdata %>%
  filter(icao24 == "a35a92")

ggplot(plane_data, aes(lon, lat, color= time)) +  
  xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + 
  xlim(-125, - 65) + ylim(25, 50) + geom_path(size = 1) +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)

######planes that pass through RDU
data <- usdata
lon <- -78.788
lat <- 35.8801
distance <- c()
for(row in 1:nrow(data)){
  distance[row] <- sqrt((as.numeric(as.character(data[row,"lon"]))-(lon))^2 + (as.numeric(as.character(data[row,"lat"]))-(lat))^2)
}

data <- cbind(data, distance)

#make list of ids that pass through airport of interest (1 degree buffer)
list = c()
i = 1
for (row in 1:nrow(data)){
  if(data[row,"distance"] <= .25) {
    list[i] <- as.character(data[row,"icao24"])
    i <- i + 1
  }
}

#remove repeat ids
res <- unique(list)

#only select the airplanes/ids that pass through airport of interest
res <- sort(res)
data <- data %>%
  filter(icao24 %in% res)

#filter to morning only
startTime <- 2
data1 <- data %>%
  filter(time >= (1594598400+3600*6*(startTime)) & time < (1594598400+3600*6*(startTime+1)))
ggplot(data1, aes(lon, lat, group = icao24)) +  
  xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + 
  xlim(-125, - 65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
