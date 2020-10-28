library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)

firstdata <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-00.csv")
firstdata <- na.omit(firstdata)
firstdata <- firstdata %>%
  filter(onground == "False")
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

#assign 1, 2, 3 group
groups <- c(rep(1,28),rep(2,28),rep(3,28))
fundata <- data.frame(res,groups)
newdata <- newdata %>%
  arrange(icao24)
fundata <- fundata %>%
  arrange(res)

group <- c()
i = 1
for(r in 1:nrow(newdata)){
  if(as.character(newdata[r,2]) == as.character(fundata[i,1])){
    group[r] <- fundata[i,2]
  }
  else{
    i <- i + 1
    group[r] <- fundata[i,2]
  }
}

newdata <- cbind(newdata, group)

#create mean functions
group1 <- newdata %>%
  filter(group == 1)
group2 <- newdata %>%
  filter(group == 2)
group3 <- newdata %>%
  filter(group == 3)
fun1 <- data.frame()
fun2 <- data.frame()
fun3 <- data.frame()

for(i in 1:max(group1$tz)){
  data <- group1 %>%
    filter(tz == i)
  fun1[i,1] <- i
  fun1[i,2] <- mean(data$lon)
  fun1[i,3] <- mean(data$lat)
  colnames(fun1) = c("tz", "lon", "lat")
}

for(i in 1:max(group2$tz)){
  data <- group2 %>%
    filter(tz == i)
  fun2[i,1] <- i
  fun2[i,2] <- mean(data$lon)
  fun2[i,3] <- mean(data$lat)
  colnames(fun2) = c("tz", "lon", "lat")
}

for(i in 1:max(group3$tz)){
  data <- group3 %>%
    filter(tz == i)
  fun3[i,1] <- i
  fun3[i,2] <- mean(data$lon)
  fun3[i,3] <- mean(data$lat)
  colnames(fun3) = c("tz", "lon", "lat")
}

#find average distance to each mean function
avdisfun <- data.frame()
for(i in 1:length(res)){
  dis1 <- c()
  dis2 <- c()
  dis3 <- c()
  data <- newdata %>%
  filter(icao24 == res[i])
  for(j in 1:min(nrow(data),nrow(fun1))){
    eudis <- sqrt((fun1[j,2]-data[j,4])^2+(fun1[j,3]-data[j,3])^2)
    dis1[j] <- eudis
  }
  for(j in 1:min(nrow(data),nrow(fun2))){
    eudis <- sqrt((fun2[j,2]-data[j,4])^2+(fun2[j,3]-data[j,3])^2)
    dis2[j] <- eudis
  }
  for(j in 1:min(nrow(data),nrow(fun3))){
    eudis <- sqrt((fun3[j,2]-data[j,4])^2+(fun3[j,3]-data[j,3])^2)
    dis3[j] <- eudis
  }
  avdisfun[i,1] <- res[i]
  avdisfun[i,2] <- sum(dis1)/length(dis1)
  avdisfun[i,3] <- sum(dis2)/length(dis2)
  avdisfun[i,4] <- sum(dis3)/length(dis3)
 }
colnames(avdisfun) <- c("icao24", "fun1", "fun2", "fun3")
View(avdisfun)


#change group to smallest average distance
newgroup <- c()
k <- 1
for(i in 1:length(res)){
  data <- newdata %>%
    filter(icao24 == res[i])
  for(j in 1:nrow(data)){
    if(avdisfun[i,2] >= avdisfun[i,3] & avdisfun[i,2] >= avdisfun[i,4]){
      newgroup[k] <- 1
      k <- k + 1
    }
    else if(avdisfun[i,3] >= avdisfun[i,2] & avdisfun[i,3] >= avdisfun[i,4]){
      newgroup[k] <- 2
      k <- k + 1
    }
    else{
      newgroup[k] <- 3
      k <- k + 1
    }
  }
}
group <- newdata$group
newdata <- cbind(newdata, newgroup)
newdata <- subset(newdata, select = -c(group))
names(newdata)[names(newdata) == "newgroup"] <- "group"

#continue until groups are not changing
#function
clustermaker <- function(newdata, res) {
#make mean functions
  group1 <- newdata %>%
    filter(group == 1)
  group2 <- newdata %>%
    filter(group == 2)
  group3 <- newdata %>%
    filter(group == 3)
  fun1 <- data.frame()
  fun2 <- data.frame()
  fun3 <- data.frame()

  for(i in 1:max(group1$tz)){
    data <- group1 %>%
      filter(tz == i)
    fun1[i,1] <- i
    fun1[i,2] <- mean(data$lon)
    fun1[i,3] <- mean(data$lat)
    colnames(fun1) = c("tz", "lon", "lat")
  }

  for(i in 1:max(group2$tz)){
    data <- group2 %>%
      filter(tz == i)
    fun2[i,1] <- i
    fun2[i,2] <- mean(data$lon)
    fun2[i,3] <- mean(data$lat)
    colnames(fun2) = c("tz", "lon", "lat")
  }

  for(i in 1:max(group3$tz)){
    data <- group3 %>%
      filter(tz == i)
    fun3[i,1] <- i
    fun3[i,2] <- mean(data$lon)
    fun3[i,3] <- mean(data$lat)
    colnames(fun3) = c("tz", "lon", "lat")
  }

#find average distance from each function
  avdisfun <- data.frame()
  for(i in 1:length(res)){
    dis1 <- c()
    dis2 <- c()
    dis3 <- c()
    data <- newdata %>%
      filter(icao24 == res[i])
    for(j in 1:min(nrow(data),nrow(fun1))){
      eudis <- sqrt((fun1[j,2]-data[j,4])^2+(fun1[j,3]-data[j,3])^2)
      dis1[j] <- eudis
    }
    for(j in 1:min(nrow(data),nrow(fun2))){
      eudis <- sqrt((fun2[j,2]-data[j,4])^2+(fun2[j,3]-data[j,3])^2)
      dis2[j] <- eudis
    }
    for(j in 1:min(nrow(data),nrow(fun3))){
      eudis <- sqrt((fun3[j,2]-data[j,4])^2+(fun3[j,3]-data[j,3])^2)
      dis3[j] <- eudis
    }
    avdisfun[i,1] <- res[i]
    avdisfun[i,2] <- sum(dis1)/length(dis1)
    avdisfun[i,3] <- sum(dis2)/length(dis2)
    avdisfun[i,4] <- sum(dis3)/length(dis3)
  }
  colnames(avdisfun) <- c("icao24", "fun1", "fun2", "fun3")
  return(avdisfun)
}

while (identical(unlist(group), unlist(newgroup)) == FALSE) {
  avdisfun <- clustermaker(newdata, res)
  View(avdisfun)
  #change group to closest mean function
  newgroup <- c()
  k <- 1
  for(i in 1:length(res)){
    data <- newdata %>%
      filter(icao24 == res[i])
    for(j in 1:nrow(data)){
      if(avdisfun[i,2] >= avdisfun[i,3] && avdisfun[i,2] >= avdisfun[i,4]){
        newgroup[k] <- 1
        k <- k + 1
      }
      else if(avdisfun[i,3] >= avdisfun[i,2] && avdisfun[i,3] >= avdisfun[i,4]){
        newgroup[k] <- 2
        k <- k + 1
      }
      else{
        newgroup[k] <- 3
        k <- k + 1
      }
    }
  }
  group <- newdata$group
  newdata <- cbind(newdata, newgroup)
  newdata <- subset(newdata, select = -c(group))
  names(newdata)[names(newdata) == "newgroup"] <- "group"
}
