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
  ggtitle("Flight Locations on July 13th 2020 @ 0010") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_point(show.legend = FALSE) +
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


#cluster routes
data <- read.csv("thesis2021/airport_data_karlee.csv")
CHI_arrive_morning <- data %>%
  filter(time_of_day == 1, airport == "CHI", arrive_depart == "arrive")
ggplot(MIA_arrive, aes(lon, lat, color= factor(group))) +  
  ggtitle("Cluster Routes Arriving at Chicago in the Morning") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path(show.legend = FALSE) +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)

#all routes + selected routes
library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)

closest_path <- function(fun,lat,lon){
  dis <- 10000
  for(r in 1:nrow(fun)){
    dis1 <- sqrt((fun[r,"lon"]-lon)^2+(fun[r,"lat"]-lat)^2)
    if(dis1<dis){
      dis <- dis1
      loc <- r
    }
  }
  info <- c(dis,loc)
}

totalPath <- function(df1,df2,lat,lon){
  
  #which path from starting airport gets closest to end airport
  numclusters <- max(df1$group)
  fun <- df1 %>%
    filter(group == 1)
  info <- closest_path(fun,lat,lon)
  for(n in 2:numclusters){
    fun1 <- df1 %>%
      filter(group == n)
    info1 <- closest_path(fun1,lat,lon)
    if(info1[1] < info[1]){
      fun <- fun1
      info <- info1
    }
  }
  
  #which path from ending airport gets closest to the point above
  fun1 <- df2 %>%
    filter(group == min(df2$group))
  lon1 <- fun[info[2],3]
  lat1 <- fun[info[2],4]
  info1 <- closest_path(fun1,lat1,lon1)
  for(n in min(df2$group):max(df2$group)){
    fun2 <- df2 %>%
      filter(group == n)
    info2 <- closest_path(fun2,lat1,lon1)
    if(info2[1] < info1[1]){
      fun1 <- fun2
      info1 <- info2
    }
  }
  
  fun <- fun[1:info[2],]
  fun1 <- fun1[1:info1[2],]
  fun1 <- fun1 %>%
    arrange(desc(tz))
  
  for(r in 1:nrow(fun1)){
    fun1[r,2] <- info[2]+r
  }
  
  route <- rbind(fun,fun1)
  return(route)
}

#returns full data of all routes, returns data with "best route", returns plots of all routes and best route
totalFunction <- function(starting_airport,ending_airport,startingTime){
  airport_data <- read.csv("thesis2021//airport_data_karlee.csv")
  airport_data <- airport_data[,-1]
  df1 <- airport_data %>%
    filter(airport == starting_airport & time_of_day == startingTime & arrive_depart == "depart")
  df1 <- df1[,-5]
  df1 <- df1[,-5]
  
  df2 <- airport_data %>%
    filter(airport == ending_airport & time_of_day == startingTime & arrive_depart == "arrive")
  df2 <- df2[,-5]
  df2 <- df2[,-5]
  
  numclusters <- length(unique(df1$group))
  
  for(i in 1:nrow(df2)){
    df2[i,1] <- df2[i,1] + numclusters
  }
  
  totalData <- rbind(df1,df2)
  
  #plot all possible flight routes
  
  # unzip the zipfile
  unzip(zipfile = "thesis2021/states_21basic.zip", 
        exdir = 'states_21basic')
  
  # load the shapefile 
  map <- readOGR("states_21basic/states.shp")
  
  #crop the portion needed
  out <- crop(map, extent(-125, -65, 25, 50))
  
  conversion <- fortify(out)
  
  plot1 <- ggplot(data.frame(totalData), aes(lon, lat, color= factor(group))) +  
    ggtitle("All Departing CHI and Arriving MIA Afternoon Flight Routes") + labs(color = "cluster")+ xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
    geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
  
  location <- read.csv("thesis2021//location_data_karlee.csv")
  location <- location[,-1]
  location <- location %>%
    filter(airport == ending_airport)
  lat <- as.numeric(location[,"lat"])
  lon <- location[,"lon"]
  
  CF <- data.frame(totalPath(df1,df2,lat,lon))
  
  plot2 <- ggplot(CF, aes(lon, lat, color= factor(group))) +  
    ggtitle("CHI to MIA Afternoon Flight Path") +  labs(color = "cluster")+ xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
    geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
  
  finalAnswer <- list(CF, totalData, plot1, plot2)
  
  return(finalAnswer)
}

plots <- totalFunction("CHI","MIA",2)
plots[3]
