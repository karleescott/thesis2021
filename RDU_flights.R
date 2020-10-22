library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)

firstdata <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-00.csv")
firstdata <- na.omit(firstdata)
firstdata <- firstdata %>%
  filter(lon >= -125 & lon <= -65 & lat >= 25 & lat <= 50)

#distance from RDU
distance <- c()
for(row in 1:nrow(firstdata)){
  distance[row] <- sqrt((firstdata[row,4]-(-78.7880))^2 + (firstdata[row,3]-(35.8801))^2)
}

firstdata1 <- cbind(firstdata, distance)

#find ids that pass through RDU (1 degree buffer)
list = c()
i = 1
for (row in 1:nrow(firstdata1)){
  if(firstdata1[row,17] <= 1) {
    list[i] <- as.character(firstdata1[row,2])
    i <- i + 1
  }
}

#remove repeat ids
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

firstdata1 <- firstdata1 %>%
  filter(icao24 %in% res)

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

#find start time based on smallest distance from RDU
start <- data.frame()
i = 1
for(j in res){
  data <- firstdata1 %>%
  filter(icao24 == j)
  mindistance <- min(data$distance)
  data <- data %>%
    filter(distance == mindistance) %>%
    arrange(time)
  start[i,1] <- j
  start[i,2] <- data[1,1]
  i <- i + 1
}

#add start time to firstdata1
firstdata1 <- firstdata1 %>%
  arrange(icao24)

start <- start %>%
  arrange(V1)

st <- c()
i = 1
for(r in 1:nrow(firstdata1)){
  if(as.character(firstdata1[r,2]) == start[i,1]){
    st[r] <- start[i,2]
  }
  else{
    i <- i + 1
    st[r] <- start[i,2]
  }
}

firstdata1 <- cbind(firstdata1, st)

#remove all times prior to start time
#if the plane takes more than one route through the area, this approach might delete previous routes
newdata <- firstdata1 %>%
  filter(time>=st)

#change times to start at 1
newdata <- newdata %>%
  arrange(icao24, time)

tz <- c(1)
i = 1
for(r in 2:nrow(newdata)){
  if(newdata[r-1,2] == newdata[r,2]){
    i <- i + 1
    tz[r] <- i
  }
  else{
    i = 1
    tz[r] <- i
  }
}

newdata <- cbind(newdata, tz)
View(newdata)


# finds the average distance from res[1] to each of the other ids
res <- sort(res)

avdis <- c()

res1data <- newdata %>%
  filter(icao24 == res[1])
view(res1data)

for (i in 2:length(res)){
  dis <- c()
  k <- 0
  data <- newdata %>%
    filter(icao24 == res[i])
  for (j in 1:min(nrow(data),nrow(res1data))){
    eudis <- sqrt((res1data[j,3]-data[j,3])^2+(res1data[j,4]-data[j,4])^2)
    dis[j] <- eudis
    k <- k + 1
  }
  avdis[i-1] <- sum(dis)/k
}

avdistance <- cbind(res[2:length(res)],avdis)
View(avdistance)

#find id with the smallest average distance from res[1]'s path
smallestdis <- as.numeric(min(avdistance[,2]))

for (i in 1:nrow(avdistance)){
  if (avdistance[i,2] == smallestdis){
    close <- (avdistance[i,1])
  }
}

res2data <- newdata %>%
  filter(icao24 == close)

closestpath <- rbind(res1data,res2data)

#graph to confirm
ggplot(closestpath, aes(lon, lat, color=icao24)) +  
  ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-90, - 65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)

ggplot(newdata, aes(lon, lat, color=icao24)) +  
  ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + theme(legend.position="none") + xlim(-90, - 65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)

#potential problems: planes aren't moving