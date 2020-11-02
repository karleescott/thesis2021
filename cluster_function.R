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
  colnames(data) <- c("time", "icao24", "lon", "lat")
  #distance from RDU
  distance <- c()
  for(row in 1:nrow(data)){
    distance[row] <- sqrt((as.numeric(data[row,3])-(-78.7880))^2 + (as.numeric(data[row,4])-(35.8801))^2)
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
  
  #add start time to firstdata1
  data <- data %>%
    arrange(icao24)
  
  start <- start %>%
    arrange(V1)
  
  st <- c()
  i = 1
  for(r in 1:nrow(data)){
    if(as.character(data[r,1]) == start[i,1]){
      st[r] <- start[i,2]
    }
    else{
      i <- i + 1
      st[r] <- start[i,2]
    }
  }
  
  data <- cbind(data, st)
  
  #remove all times prior to start time
  #if the plane takes more than one route through the area, this approach might delete previous routes
  data <- data %>%
    filter(time>=st)
  
  #change times to start at 1
  data <- data %>%
    arrange(icao24, time)
  
  tz <- c(1)
  i = 1
  for(r in 2:nrow(data)){
    if(data[r-1,1] == data[r,1]){
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
  groups <- c()
  l = 1
  for (n in 1:len(res)){
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
    if(as.character(data[r,1]) == as.character(fundata[i,1])){
      group[r] <- fundata[i,2]
    }
    else{
      i <- i + 1
      group[r] <- fundata[i,2]
    }
  }
  
  data <- cbind(data, group)
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
      fun <- data.frame()
      data2 <- data1 %>%
        filter(tz == i)
      fun[i + j,1] <- n
      fun[i + j,2] <- i
      fun[i + j,3] <- mean(data2$lon)
      fun[i + j,4] <- mean(data2$lat)
    }
    j <- j + i
  }
  colnames(fun) = c("group", "tz", "lon", "lat")
  
  #find distance between each path and the mean functions at every tz
  for (r in 1:nrow(data)){
    for (n in numclusters){
      data1 <- fun %>%
        filter(group == n, tz == data[r,tz])
      data[r,8+n] <- sqrt((data[r,2]-data1[1,3])^2 + (data[r,3]-data1[1,4])^2)
    }  
  }
  
  #find average distance from each mean function
  avdis <- data.frame()
  for(i in 1:len(res)){
    for(n in 1:numclusters)
      data1 <- data %>%
        filter(icao24 == res[i])
    avdis[i,1] <- res[i]
    avdis[i,2] <- n
    avdis[i,3] <- mean(data1[,8+n])
  }
  colnames(avdis) <- c("icao24", "cluster", "avdis")
  
  #find sd of each mean function
  sd <- data.frame()
  for(n in 1:numclusters){
    data1 <- avdis %>%
      filter(cluster == n)
    sd[n,1] <- n
    sd[n,2] <- mean(data1$avdis)
  }
  colnames(sd) <- c("cluster", "sd")
  
  #find likelihoods
  likelihoods <- data.frame()
  for (i in 1:len(res)){
    for (n in 1:numclusters){
      data1 <- avdis %>%
        filter(icao24 == res[i] & cluster == n)
      sd1 <- sd %>%
        filter(cluster == n)
      likelihoods[i,1] <- res[i]
      likelihoods[i,2] <- n
      likelihoods[i,3] <- (1/(sqrt(2*pi*sd1[1,2])))*exp((-1/(sd1[1,2]^2))*(data1[1,3]^2))
    }
  }
  colnames(likelihoods) <- c("icao24", "cluster", "likelihood")
  
  #select which function has highest likelihood for each path
  highli <-data.frame()
  for(i in 1:len(res)){
    data1 <- likelihood %>%
      filter(icao24 == res[i])
    highli[i,1] <- res[i]
    hili <- max(data1[,3])
    for(n in 1:numclusters){
      if(data1[n,3] == hili)
        highli[i,2] <- n 
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
    if(as.character(data[r,1]) == highli[i,1]){
      group[r] <- highli[i,2]
    }
    else{
      i <- i + 1
      group[r] <- highli[i,2]
    }
  }
  
  data[,8] <- group
  return(data)
}

data <- makeData(firstdata,2)
