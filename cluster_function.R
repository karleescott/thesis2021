library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)

firstdata <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-00.csv")

makeData <- function(data, numclusters){ 
  #filter data
  data <- na.omit(data)
  data <- data %>%
    filter(onground == "False")
  data <- data %>%
    filter(lon >= -125 & lon <= -65 & lat >= 25 & lat <= 50)
  data <- cbind(data$time,as.character(data$icao24),data$lon,data$lat)
  data <- data.frame(data)
  colnames(data) <- c("time", "icao24", "lon", "lat")
  
  #distance from RDU
  distance <- c()
  for(row in 1:nrow(data)){
    distance[row] <- sqrt((as.numeric(as.character(data[row,3]))-(-78.7880))^2 + (as.numeric(as.character(data[row,4]))-(35.8801))^2)
  }
  
  data <- cbind(data, distance)
  
  #find ids that pass through RDU (1 degree buffer)
  list = c()
  i = 1
  for (row in 1:nrow(data)){
    if(data[row,5] <= 1) {
      list[i] <- as.character(data[row,2])
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
  
  #find start time based on smallest distance from RDU
  start <- data.frame()
  i = 1
  for(j in res){
    data1 <- data %>%
      filter(icao24 == j)
    mindistance <- min(data1$distance)
    data1 <- data1 %>%
      filter(distance == mindistance) %>%
      arrange(time)
    start[i,1] <- j
    start[i,2] <- data1[1,1]
    i <- i + 1
  }
  
  #add start time to data
  data <- data %>%
    arrange(icao24)
  start <- start %>%
    arrange(V1)
  
  st <- c()
  i = 1
  for(r in 1:nrow(data)){
    if(as.character(data[r,2]) == as.character(start[i,1])){
      st[r] <- as.numeric(as.character(start[i,2]))
    }
    else{
      i <- i + 1
      st[r] <- as.numeric(as.character(start[i,2]))
    }
  }
  
  data <- cbind(data, st)
  
  #remove all times prior to start time
  #if the plane takes more than one route through the area, this approach might delete previous routes
  data[,1] <- as.numeric(as.character(data[,1]))
  data <- data %>%
    filter(time>=st)
  
  #change times to start at 1
  data <- data %>%
    arrange(icao24, time)
  
  tz <- c(1)
  i = 1
  for(r in 2:nrow(data)){
    if(as.character(data[r-1,2]) == as.character(data[r,2])){
      i <- i + 1
      tz[r] <- i
    }
    else{
      i = 1
      tz[r] <- i
    }
  }
  data <- cbind(data, tz)
  
  #assign initial random group
  groups <- data.frame()
  l = 1
  for (n in 1:length(res)){
    groups[n,1] <- res[n]
    groups[n,2] <- l
    if(l == numclusters){
      l <- 1
    }
    else{
      l <- l + 1
    }
  }
  colnames(groups) = c("res", "group")
  data <- data %>%
    arrange(icao24)
  fundata <- groups %>%
    arrange(res)
  
  group <- c()
  i = 1
  for(r in 1:nrow(data)){
    if(as.character(data[r,2]) == as.character(fundata[i,1])){
      group[r] <- fundata[i,2]
    }
    else{
      i <- i + 1
      group[r] <- fundata[i,2]
    }
  }
  
  data <- cbind(data, group)
  data[,2] <- as.character(data[,2])
  data[,3] <- as.numeric(as.character(data[,3]))
  data[,4] <- as.numeric(as.character(data[,4]))
  return(data)
}


makeCluster <- function(data, numclusters) {
  #create mean functions
  fun <- data.frame()
  data <- data %>%
    arrange(icao24, tz)
  j <- 0
  for(n in 1:numclusters){
    data1 <- data %>%
      filter(group == n)
    for(i in 1:max(data1$tz)){
      data2 <- data1 %>%
        filter(tz == i)
      data2[,3] <- as.numeric(as.character(data2[,3]))
      data2[,4] <- as.numeric(as.character(data2[,4]))
      fun[i + j,1] <- n
      fun[i + j,2] <- i
      fun[i + j,3] <- mean(data2$lon)
      fun[i + j,4] <- mean(data2$lat)
    }
    j <- j + i
  }
  colnames(fun) = c("group", "tz", "lon", "lat")
  
  #find squared distance between each point and the mean functions at every tz (functions are different lengths?)
  for (r in 1:nrow(data)){
    for (n in 1:numclusters){
      data1 <- fun %>%
        filter(group == n)
      if(max(data1$tz) < data[r,7]){
        data[r,8+n] <- "NA"
      }
      else{
        data2 <- data1 %>%
          filter(tz == data[r,7])
        data[r,8+n] <- (as.numeric(as.character(data[r,3]))-as.numeric(as.character(data2[1,3])))^2 + (as.numeric(as.character(data[r,4]))-as.numeric(as.character(data2[1,4])))^2
      }
    }  
  }
  
  #re-define res
  list <- data[,2]
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
  
  #find average distance from each mean function
  avdis <- data.frame()
  j <- 1
  for(i in 1:length(res)){
    data1 <- data %>%
      filter(icao24 == res[i])
    for(n in 1:numclusters){
      data2 <- as.numeric(as.character(data1[,8+n]))
      data2 <- na.omit(data2)
      avdis[j,1] <- res[i]
      avdis[j,2] <- n
      avdis[j,3] <- sqrt(mean(data2))
      j <- j + 1
    }
  }
  colnames(avdis) <- c("icao24", "cluster", "avdis")
  
  #find sd of each mean function
  sd <- data.frame()
  for(n in 1:numclusters){
    data1 <- avdis %>%
      filter(cluster == n)
    sd[n,1] <- n
    sd[n,2] <- sqrt(sum((data1$avdis)^2)/length(data1$avdis))
  }
  colnames(sd) <- c("cluster", "sd")
  
  #find likelihoods
  likelihoods <- data.frame()
  j <- 1
  for (i in 1:length(res)){
    for (n in 1:numclusters){
      data1 <- avdis %>%
        filter(icao24 == res[i] & cluster == n)
      likelihoods[j,1] <- res[i]
      likelihoods[j,2] <- n
      likelihoods[j,3] <- (1/(sqrt(2*pi*sd[n,2])))*exp((-1/(2*sd[n,2]^2))*(data1[1,3]^2))
      j <- j + 1
    }
  }
  colnames(likelihoods) <- c("icao24", "cluster", "likelihood")
  
  #select which function has highest likelihood for each path
  highli <-data.frame()
  for(i in 1:length(res)){
    highli[i,1] <- res[i]
    data1 <- likelihoods %>%
      filter(icao24 == res[i])
    HL <- max(data1[,3])
    for(n in 1:numclusters){
      if(data1[n,3] == HL){
        highli[i,2] <- n
      }
    }
  }
  colnames(highli) <- c("icao24", "newgroup")
  
  #change group to newgroup
  data <- data %>%
    arrange(icao24)
  
  highli <- highli %>%
    arrange(icao24)
  
  group <- c()
  i = 1
  for(r in 1:nrow(data)){
    if(data[r,2] == highli[i,1]){
      group[r] <- highli[i,2]
    }
    else{
      i <- i + 1
      group[r] <- highli[i,2]
    }
  }
  data[,8] <- group
  everything <- list(data, fun, sd, likelihoods)
  return(everything)
}

compareMean <- function(fun1, fun2, numclusters, threshold){
  #find squared distance between each time of the mean functions at every tz (functions are different lengths?)
  sqdist <- data.frame()
  for (r in 1:nrow(fun1)){
    for (n in 1:numclusters){
      sqdist[r,1] <- fun1[r,1]
      sqdist[r,2] <- fun1[r,2]
      data1 <- fun2 %>%
        filter(group == n)
      if(max(data1$tz) < fun1[r,2]){
        sqdist[r,2+n] <- "NA"
      }
      else{
        data2 <- data1 %>%
          filter(tz == fun1[r,2])
        sqdist[r,2+n] <- (as.numeric(as.character(fun1[r,3]))-as.numeric(as.character(data2[1,3])))^2 + (as.numeric(as.character(fun1[r,4]))-as.numeric(as.character(data2[1,4])))^2
      }
    }  
  }
  names(sqdist)[1] <- "fun1_cluster"
  names(sqdist)[2] <- "tz"
  
  #find dissimilarity using squared distance from above
  dis <- data.frame()
  j <- 1
  for(n in 1:numclusters){
    for(m in 1:numclusters){
      data1 <- sqdist %>%
        filter(fun1_cluster == n)
      data2 <- as.numeric(as.character(data1[,2+m]))
      data2 <- na.omit(data2)
      dis[j,1] <- n
      dis[j,2] <- m
      dis[j,3] <- sqrt(mean(data2))
      j <- j + 1
    }
  }
  colnames(dis) <- c("fun1_cluster", "fun2_cluster", "dis")
  
  #check if new functions are within threshold
  goodFun <- data.frame()
  i <- 0
  for(n in 1:numclusters){
    data1 <- dis %>%
      filter(fun1_cluster == n)
    for(m in 1:numclusters){
      if(data1[i+m,3] <= threshold){
        goodFun[n,m] <- 1
      }
      else{
        goodFun[n,m] <- 0
      }
    }
  }
  
  for(n in 1:numclusters){
    if(sum(goodFun[,n])==0){
      return("False")
    }
  }
  return("True")
}

totalFunction <- function(data,numclusters,threshold){
  data <- makeData(data,numclusters)
  everything <- makeCluster(data,numclusters)
  everything1 <- makeCluster(data.frame(everything[1]),numclusters)
  fun1 <- data.frame(everything[2])
  fun2 <- data.frame(everything1[2])
  while(compareMean(fun1,fun2,numclusters,threshold) == "False"){
    fun1 <- fun2
    everything1 <- makeCluster(data.frame(everything1[1]),numclusters)
    fun2 <- data.frame(everything1[2])
  }
  return(everything1)
}

# unzip the zipfile
unzip(zipfile = "thesis2021/states_21basic.zip", 
      exdir = 'states_21basic')

# load the shapefile 
map <- readOGR("states_21basic/states.shp")

#crop the portion needed
out <- crop(map, extent(-125, -65, 25, 50))

conversion <- fortify(out)

ggplot(data.frame(everything5[2]), aes(lon, lat, color= factor(group))) +  
  ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-90, - 65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)

#Coersed NA on purpose, ignore errors. Takes about 5 minutes to run

#2 clusters
everything2 <- totalFunction(firstdata,2,1)
j2 = 0
likelihood2 = data.frame(everything2[4])
i = 1
while(i < nrow(likelihood2)){
  j2 = j2 + log(max(likelihood2[i,3],likelihood2[i+1,3]))
  i = i + 2
}

#3 clusters
everything3 <- totalFunction(firstdata,3,1)
j3 = 0
likelihood3 = data.frame(everything3[4])
i = 1
while(i < nrow(likelihood3)){
  j3 = j3 + log(max(likelihood3[i,3],likelihood3[i+1,3],likelihood3[i+2,3]))
  i = i + 3
}

#4 clusters
everything4 <- totalFunction(firstdata,4,1)
j4 = 0
likelihood4 = data.frame(everything4[4])
i = 1
while(i < nrow(likelihood4)){
  j4 = j4 + log(max(likelihood4[i,3],likelihood4[i+1,3],likelihood4[i+2,3],likelihood4[i+3,3]))
  i = i + 4
}

#5 clusters
everything5 <- totalFunction(firstdata,5,1)
j5 = 0
likelihood5 = data.frame(everything5[4])
i = 1
while(i < nrow(likelihood5)){
  j5 = j5 + log(max(likelihood5[i,3],likelihood5[i+1,3],likelihood5[i+2,3],likelihood5[i+3,3],likelihood5[i+4,3]))
  i = i + 5
}  

#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/AIC
AIC2 <- 2*j2-2*2
BIC2 <- 2*j2-log(10531)*2

AIC3 <- 2*j3-2*3
BIC3 <- 2*j3-log(10531)*3

AIC4 <- 2*j4-2*4
BIC4 <- 2*j4-log(10531)*4

AIC5 <- 2*j5-2*5
BIC5 <- 2*j5-log(10531)*5
