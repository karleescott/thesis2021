library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)

usdata <- read.csv("/lfs/karlee_combined_data.csv")
View(usdata)

firstdata <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-00.csv")
firstdata <- na.omit(firstdata)
firstdata <- firstdata %>%
  filter(lon >= -125 & lon <= -65 & lat >= 25 & lat <= 50)

#finds list of ids that pass through RDU (1 degree buffer)
list = c()
for (row in 1:nrow(firstdata)){
  if(firstdata[[row,4]] > (-78.7880 - 1) & firstdata[[row,4]] < (-78.7880 + 1) & firstdata[[row,3]] < (35.8801 + 1) & firstdata[[row,3]] > (35.8801 - 1)) {
    list[row] = as.character(firstdata[[row,2]])
  }
}

firstdata1 <- firstdata %>%
  filter(icao24 %in% list)

# unzip the zipfile
unzip(zipfile = "thesis2021/states_21basic.zip", 
      exdir = 'states_21basic')

# load the shapefile 
map <- readOGR("states_21basic/states.shp")

#crop the portion needed
out <- crop(map, extent(-125, -65, 25, 50))

conversion <- fortify(out)

ggplot(firstdata1, aes(lon, lat, color=icao24)) +  
  ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + theme(legend.position="none") + xlim(-90, - 65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)

firstdata1 <- firstdata1 %>%
  arrange(time)
View(firstdata1)

#distance from RDU
distance <- c()
for(row in 1:nrow(firstdata1)){
  distance[row] <- sqrt((firstdata[row,4]-(-78.7880))^2 + (firstdata[row,3]-(35.8801))^2)
}

firstdata1 <- cbind(firstdata1, distance)

start <- data.frame()
i = 1
for(j in list){
  data <- firstdata1 %>%
  filter(icao24 == j)
  mindistance <- min(data$distance)
  data <- data %>%
    filter(data$distance == mindistance)
  start[i,1] <- j
  start[i,2] <- data$time
  i <- i + 1
}

#add start time to firstdata1
st <- c()
for(r in 1:nrow(firstdata1)){
  id <- firstdata1[r,2]
  for(q in 1:nrow(start)){
    if(start[q,1] == id){
      st[r] <- start[q,2]
    }
  }
}
View(start)

#remove all times prior to start time

#iterate through and change times to start at 1

