library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)


usdata <- read.csv("/lfs/karlee_combined_data.csv")

#makes the data to be used in total Function. startTime is the hour of day (0-23)
makeData <- function(lat,lon,startTime){ 
  #lat <- 35.8801
  #lon <- -78.7880
  #startTime <- 0
  print(startTime)
  data <- usdata
  data <- data %>%
    filter(time >= (1594598400+3600*6*(startTime)) & time < (1594598400+3600*6*(startTime+1)))
  numclusters = 4
  #filter data
  data <- na.omit(data)
  data <- data %>%
    filter(lon >= -125 & lon <= -65 & lat >= 25 & lat <= 50)
  data <- cbind(data$time,as.character(data$icao24),data$lon,data$lat,as.character(data$onground))
  data <- data.frame(data)
  colnames(data) <- c("time", "icao24", "lon", "lat","onground")
  
  #distance from RDU
  distance <- c()
  for(row in 1:nrow(data)){
    distance[row] <- sqrt((as.numeric(as.character(data[row,3]))-(lon))^2 + (as.numeric(as.character(data[row,4]))-(lat))^2)
  }
  
  data <- cbind(data, distance)
  
  #find ids that pass through RDU (1 degree buffer)
  list = c()
  i = 1
  for (row in 1:nrow(data)){
    if(data[row,6] <= 1) {
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
  
  tz_count <- table(data$tz)
  tz_count <- as.data.frame(tz_count)
  tz_count <- tz_count %>%
    filter(Freq < 5)
  cap <- as.numeric(as.character(tz_count[1,1]))
  data <- data %>%
    filter(tz < cap)
  
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
  
  #remove data that is a second flight form same icao24 (aka lands and then re take's off)
  grounddata <- data
  
  grounddata_length <- nrow(grounddata)
  remove_list <- c()
  i <- 1
  while(i <= grounddata_length){
    id_name <- grounddata[i,2]
    if(grounddata[i,5] == "True" & grounddata[i,6] > 1){
      remove_list <- append(i,remove_list)
      i <- i + 1
      id_name1 <- grounddata[i,2]
      while(id_name == id_name1){
        remove_list <- append(i,remove_list)
        id_name <- grounddata[i,2]
        i <- i + 1
        id_name1 <- grounddata[i,2]
      }
    }
    i <- i + 1
  }
  
  if(is.null(remove_list) == "FALSE"){  
    data <- grounddata[-(remove_list),]
  }
  data <- data[,-5]
  
  return(data)
}

makeCluster <- function(data) {
  numclusters <- length(unique(data$group))
  groups <- sort(unique(data$group))
  for(r in 1:nrow(data)){
    data[r,8] <- match(data[r,8],groups)
  }
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


#####roadblock 4 clusters turned into 3 (no data was assigned to group 1), now theres no function to compare it to.
compareMean <- function(fun1, fun2, threshold){
  numclusters = length(unique(fun2$group))
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

#all specified airport data for 13 July. Must be filtered by hour to use. 
combineData <- function(lat,lon,threshold){
  data <- makeData(lat,lon,0)
  everything <- makeCluster(data)
  everything1 <- makeCluster(data.frame(everything[1]))
  fun1 <- data.frame(everything[2])
  fun2 <- data.frame(everything1[2])
  j <- 0
  while(compareMean(fun1,fun2,threshold) == "False"|| j == 10){
    fun1 <- fun2
    everything1 <- makeCluster(data.frame(everything1[1]))
    fun2 <- data.frame(everything1[2])
    j <- j + 1
  }
  
  data <- cbind(as.data.frame(everything1[2]),time_of_day = 0)
  
  for (i in 1:3){
    data1 <- makeData(lat,lon,i)
    everything <- makeCluster(data1)
    everything2 <- makeCluster(data.frame(everything[1]))
    fun1 <- data.frame(everything[2])
    fun2 <- data.frame(everything2[2])
    j = 0
    while(compareMean(fun1,fun2,threshold) == "False" || j == 10){
      fun1 <- fun2
      everything2 <- makeCluster(data.frame(everything2[1]))
      fun2 <- data.frame(everything2[2])
      j <- j + 1
    }
    
    data1 <- cbind(as.data.frame(everything2[2]),time_of_day = i)
    
    data <- rbind(data,data1)

  }
  return(data)
}

#Coersed NA on purpose, ignore errors. Takes about 5 minutes to run

closest_path <- function(fun,lat,lon){
  dis <- 10000
  for(r in 1:nrow(fun)){
    dis1 <- sqrt((fun[r,3]-lon)^2+(fun[r,4]-lat)^2)
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
totalFunction <- function(starting_airport,ending_airport,startingTime,threshold){
  airport_data <- read.csv("thesis2021//airport_data_karlee.csv")
  airport_data <- airport_data[,-1]
  df1 <- airport_data %>%
    filter(airport == starting_airport & time_of_day == startingTime)
  df1 <- df1[,-5]
  df1 <- df1[,-5]
  
  df2 <- airport_data %>%
    filter(airport == ending_airport & time_of_day == startingTime)
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
    ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
    geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
  
  location <- read.csv("thesis2021//location_data_karlee.csv")
  location <- location[,-1]
  location <- location %>%
    filter(airport == ending_airport)
  lat <- as.numeric(location[,2])
  lon <- location[,3]
  
  CF <- data.frame(totalPath(df1,df2,lat,lon))
  
  plot2 <- ggplot(CF, aes(lon, lat, color= factor(group))) +  
    ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
    geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
  
  finalAnswer <- list(CF, totalData, plot1, plot2)
  
  return(finalAnswer)
}


RDU <- combineData(35.8801,-78.7880,1)
MIA <- combineData(25.7617,-80.1918,1)
RDU <- cbind(RDU,airport = "RDU")
MIA <- cbind(MIA,airport = "MIA")
airport_data <- rbind(RDU,MIA)
write.csv(airport_data,"thesis2021//airport_data_karlee.csv")

airport <- c("RDU","MIA")
lat <- c(35.8801,25.7617)
lon <- c(-78.7880,-80.1918)
location <- as.data.frame(cbind(airport, lat, lon))
location <- transform(location, airport = as.character(airport), lat = as.numeric(as.character(lat)), lon = as.numeric(as.character(lon)))
write.csv(location,"thesis2021//location_data_karlee.csv")

finalAnswer <- totalFunction("MIA","RDU",0,1)

View(data.frame(finalAnswer[1]))
finalAnswer[4]
