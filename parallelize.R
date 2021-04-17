library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)
library(foreach)
library(doMC)

registerDoMC(14)


usdata <- read.csv("/lfs/karlee_combined_data.csv")

#makes the data to be used in total Function. startTime is the hour of day (0-23)
makeData <- function(lat,lon,startTime){ 
  #lat <- 35.8801
  #lon <- -78.7880
  #startTime <- 0
  print(startTime)
  
  #filters the data's time to startTime provided by the user: 0(night) = 0000-0600, 1(morning) = 0600-1200, 2(afternoon) = 1200-1800, 3(evening) = 1800-0000
  data <- usdata
  data <- data %>%
    filter(time >= (1594598400+3600*6*(startTime)) & time < (1594598400+3600*6*(startTime+1)))
  
  #number of mean routes to be created
  numclusters = 4
  
  #filter data by continental US, select needed columns, remove rows with NA
  data <- data %>%
    filter(lon >= -125 & lon <= -65 & lat >= 25 & lat <= 50)
  data <- cbind(data$time,as.character(data$icao24),data$lon,data$lat,as.character(data$onground))
  data <- na.omit(data)
  data <- data.frame(data)
  colnames(data) <- c("time", "icao24", "lon", "lat","onground")
  
  #create column that determines distance from airport of interest
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
    start[i,2] <- data1[1,"time"]
    i <- i + 1
  }
  colnames(start) <- c("icao24","time")
  
  #add start time to data
  data <- data %>%
    arrange(icao24)
  start <- start %>%
    arrange(icao24)
  
  st <- c()
  i = 1
  for(r in 1:nrow(data)){
    if(as.character(data[r,"icao24"]) == as.character(start[i,"icao24"])){
      st[r] <- as.numeric(as.character(start[i,"time"]))
    }
    else{
      i <- i + 1
      st[r] <- as.numeric(as.character(start[i,"time"]))
    }
  }
  
  data <- cbind(data, st)
  
  #remove all times prior to start time
  data[,"time"] <- as.numeric(as.character(data[,"time"]))
  data <- data %>%
    filter(time>=st)
  
  #remove data that is a second flight from same icao24 (aka lands and then re take's off)
  data <- data %>%
    arrange(icao24, time)
  
  for(i in res){
    data1 <- data %>%
      filter(icao24 == i)
    j <- 1
    while(j <= nrow(data1)){
      if(as.character(data1[j,"onground"]) == "True" && data1[j,"distance"] > .25){
        last_time <- data1[j,"time"] 
        j <- nrow(data1) + 1
      } else{
        last_time <- data1[j,"time"] 
        j <- j + 1
      }
    }
    data <- data[!(as.character(data$icao24) == i & as.numeric(as.character(data$time)) > last_time),]
  }
  
  #change times to start at 1
  data <- data %>%
    arrange(icao24, time)
  
  tz <- c(1)
  i = 1
  for(r in 2:nrow(data)){
    if(as.character(data[r-1,"icao24"]) == as.character(data[r,"icao24"])){
      i <- i + 1
      tz[r] <- i
    }
    else{
      i = 1
      tz[r] <- i
    }
  }
  data <- cbind(data, tz)
  
  #assign initial random group/ cluster
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
  colnames(groups) = c("icao24", "group")
  data <- data %>%
    arrange(icao24)
  fundata <- groups %>%
    arrange(icao24)
  
  group <- c()
  i = 1
  for(r in 1:nrow(data)){
    if(as.character(data[r,"icao24"]) == as.character(fundata[i,"icao24"])){
      group[r] <- fundata[i,"icao24"]
    }
    else{
      i <- i + 1
      group[r] <- fundata[i,"group"]
    }
  }
  
  data <- cbind(data, group)
  data[,"icao24"] <- as.character(data[,"icao24"])
  data[,"lon"] <- as.numeric(as.character(data[,"lon"]))
  data[,"lat"] <- as.numeric(as.character(data[,"lat"]))
  
  return(data)
}

makeData2 <- function(lat,lon,startTime){ 
  #lat <- 35.8801
  #lon <- -78.7880
  #startTime <- 0
  print(startTime)
  #filters the data's time to startTime provided by the user: 0(night) = 0000-0600, 1(morning) = 0600-1200, 2(afternoon) = 1200-1800, 3(evening) = 1800-0000
  data <- usdata
  data <- data %>%
    filter(time >= (1594598400+3600*6*(startTime)) & time < (1594598400+3600*6*(startTime+1)))
  
  #number of mean routes to be created
  numclusters = 4
  
  #filter data by continental US, select needed columns, remove rows with NA
  data <- data %>%
    filter(lon >= -125 & lon <= -65 & lat >= 25 & lat <= 50)
  data <- cbind(data$time,as.character(data$icao24),data$lon,data$lat,as.character(data$onground))
  data <- na.omit(data)
  data <- data.frame(data)
  colnames(data) <- c("time", "icao24", "lon", "lat","onground")
  
  #create column that determines distance from airport of interest
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
    start[i,2] <- data1[1,"time"]
    i <- i + 1
  }
  
  colnames(start) <- c("icao24","time")
  #add start time to data
  data <- data %>%
    arrange(icao24)
  start <- start %>%
    arrange(icao24)
  
  st <- c()
  i = 1
  for(r in 1:nrow(data)){
    if(as.character(data[r,"icao24"]) == as.character(start[i,"icao24"])){
      st[r] <- as.numeric(as.character(start[i,"time"]))
    }
    else{
      i <- i + 1
      st[r] <- as.numeric(as.character(start[i,"time"]))
    }
  }
  
  data <- cbind(data, st)
  
  #remove all times after start time
  #if the plane takes more than one route through the area, this approach might delete previous routes
  data[,"time"] <- as.numeric(as.character(data[,"time"]))
  data <- data %>%
    filter(time<=st)
  
  #remove data that is a second flight form same icao24 (aka lands and then re take's off)
  data <- data %>%
    arrange(icao24, time)
  
  for(i in res){
    data1 <- data %>%
      filter(icao24 == i)
    j <- nrow(data1)
    while(j >= 1){
      if(as.character(data1[j,"onground"]) == "True" && data1[j,"distance"] > .25){
        first_time <- data1[j,"time"] 
        j <- 0
      } else{
        first_time <- data1[j,"time"] 
        j <- j - 1
      }
    }
    data <- data[!(as.character(data$icao24) == i & as.numeric(as.character(data$time)) < first_time),]
  }
  
  #change times to start at 1
  data <- data %>%
    arrange(icao24, desc(time))
  
  tz <- c(1)
  i = 1
  for(r in 2:nrow(data)){
    if(as.character(data[r-1,"icao24"]) == as.character(data[r,"icao24"])){
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
  colnames(groups) = c("icao24", "group")
  data <- data %>%
    arrange(icao24)
  fundata <- groups %>%
    arrange(icao24)
  
  group <- c()
  i = 1
  for(r in 1:nrow(data)){
    if(as.character(data[r,"icao24"]) == as.character(fundata[i,"icao24"])){
      group[r] <- fundata[i,"group"]
    }
    else{
      i <- i + 1
      group[r] <- fundata[i,"group"]
    }
  }
  
  data <- cbind(data, group)
  data[,"icao24"] <- as.character(data[,"icao24"])
  data[,"lon"] <- as.numeric(as.character(data[,"lon"]))
  data[,"lat"] <- as.numeric(as.character(data[,"lat"]))
  
  return(data)
}

makeCluster <- function(data) {
  #if one cluster did not get assigned any flights, this adjusts the clusters to 1,2,3 vs 1,2,4 for example
  numclusters <- as.numeric(length(unique(data$group)))
  groups <- sort(unique(data$group))
  for(r in 1:nrow(data)){
    data[r,"group"] <- match(data[r,"group"],groups)
  }
  
  #Must have at least 5 flights contributing to each point (tz) in mean function
  for(n in 1:numclusters){
    data1 <- data %>%
      filter(group == n)
    tz_count <- table(data1$tz)
    tz_count <- as.data.frame(tz_count)
    tz_count <- tz_count %>%
      filter(Freq < 5)
    cap <- as.numeric(as.character(tz_count[1,1]))
    fun_data <- data[!(data$group==n & data$tz>=cap),]
  }
  
  numclusters <- as.numeric(length(unique(fun_data$group)))
  groups <- sort(unique(fun_data$group))
  for(r in 1:nrow(fun_data)){
    fun_data[r,"group"] <- match(fun_data[r,"group"],groups)
  }
  
  #create mean functions
  fun <- data.frame()
  fun_data <- fun_data %>%
    arrange(icao24, tz)
  j <- 0
  for(n in 1:numclusters){
    data1 <- fun_data %>%
      filter(group == n)
    print(n)
    print(max(data1$tz))
    for(i in 1:max(data1$tz)){
      data2 <- data1 %>%
        filter(tz == i)
      data2[,"lon"] <- as.numeric(as.character(data2[,"lon"]))
      data2[,"lat"] <- as.numeric(as.character(data2[,"lat"]))
      fun[i + j,1] <- n
      fun[i + j,2] <- i
      fun[i + j,3] <- mean(data2$lon)
      fun[i + j,4] <- mean(data2$lat)
    }
    j <- j + i
  }
  colnames(fun) = c("group", "tz", "lon", "lat")
  
  #find squared distance between each point and the mean functions at every tz (functions are different lengths?)
  data_length <- ncol(data)
  for(n in 1:numclusters){
    data1 <- fun %>%
      filter(group == n)
    for (r in 1:nrow(data)){
      if(max(data1$tz) < data[r,"tz"]){
        data[r,data_length+n] <- "NA"
      }
      else{
        data2 <- data1 %>%
          filter(tz == data[r,"tz"])
        data[r,data_length+n] <- (as.numeric(as.character(data[r,"lon"]))-as.numeric(as.character(data2[1,"lon"])))^2 + (as.numeric(as.character(data[r,"lat"]))-as.numeric(as.character(data2[1,"lat"])))^2
      }
    }
  }
  
  #re-define res
  res <- unique(data[,"icao24"])
  
  res <- sort(res)
  
  #find average distance from each mean function
  avdis <- data.frame()
  j <- 1
  for(i in 1:length(res)){
    data1 <- data %>%
      filter(icao24 == res[i])
    for(n in 1:numclusters){
      data2 <- as.numeric(as.character(data1[,data_length+n]))
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
      likelihoods[j,3] <- (1/(sqrt(2*pi)*sd[n,"sd"]))*exp((-1/(2*sd[n,"sd"]^2))*(data1[1,"avdis"]^2))
      j <- j + 1
    }
  }
  colnames(likelihoods) <- c("icao24", "cluster", "likelihood")
  
  #select which function has highest likelihood for each path
  highli <-data.frame()
  likelihoods <- likelihoods %>%
    arrange(icao24,cluster)
  for(i in 1:length(res)){
    highli[i,1] <- res[i]
    data1 <- likelihoods %>%
      filter(icao24 == res[i])
    HL <- max(data1[,"likelihood"])
    for(n in 1:numclusters){
      if(data1[n,"likelihood"] == HL){
        highli[i,"cluster"] <- n
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
    if(data[r,"icao24"] == highli[i,"icao24"]){
      group[r] <- highli[i, "newgroup"]
    }
    else{
      i <- i + 1
      group[r] <- highli[i, "newgroup"]
    }
  }
  data[,data_length] <- group
  data <- data[,1:data_length]
  everything <- list(data, fun, sd, likelihoods)
  return(everything)
}

compareMean <- function(fun1, fun2, threshold){
  numclusters = length(unique(fun2$group))
  #find squared distance between each time of the mean functions at every tz (functions are different lengths?)
  sqdist <- data.frame()
  for (r in 1:nrow(fun1)){
    for (n in 1:numclusters){
      sqdist[r,1] <- fun1[r,"group"]
      sqdist[r,2] <- fun1[r,"tz"]
      data1 <- fun2 %>%
        filter(group == n)
      if(max(data1$tz) < fun1[r,"tz"]){
        sqdist[r,2+n] <- "NA"
      }
      else{
        data2 <- data1 %>%
          filter(tz == fun1[r,"tz"])
        sqdist[r,2+n] <- (as.numeric(as.character(fun1[r,"lon"]))-as.numeric(as.character(data2[1,"lon"])))^2 + (as.numeric(as.character(fun1[r,"lat"]))-as.numeric(as.character(data2[1,"lat"])))^2
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

combineData <- function(lat,lon,arrive_depart,threshold){
  test_results <- foreach (i = 0:3) %dopar% {
    if(arrive_depart == "depart"){
      data1 <- makeData(lat,lon,i)}
    else{
      data1 <- makeData2(lat,lon,i)
    }
    everything <- makeCluster(data1)
    everything2 <- makeCluster(data.frame(everything[1]))
    fun1 <- data.frame(everything[2])
    fun2 <- data.frame(everything2[2])
    j = 0
    while(compareMean(fun1,fun2,threshold) == "False" && j <= 5){
      fun1 <- fun2
      flight_info1 <- everything2[1]
      everything2 <- makeCluster(data.frame(everything2[1]))
      fun2 <- data.frame(everything2[2])
      j <- j + 1
    }
    
    data1 <- cbind(as.data.frame(everything2[2]),time_of_day = i)
    
    flight_info1 <- cbind(as.data.frame(flight_info1),time_of_day = i)
    
    list(data1, flight_info1)
  }
  fun_data <- data.frame()
  flight_info <- data.frame()
  for (i in 1:length(test_results)) {
    iter_res <- test_results[[i]]
    fun_data <- rbind(fun_data, iter_res[[1]])
    flight_info <- rbind(flight_info, iter_res[[2]])
  }
  list(fun_data, flight_info)
}

#Coersed NA on purpose, ignore errors.

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
  fun1 <- fun1[1:info[2],]
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
    filter(airport == starting_airport & time_of_day == startingTime & arrive_depart == "depart")
  
  df2 <- airport_data %>%
    filter(airport == ending_airport & time_of_day == startingTime & arrive_depart == "arrive")
  
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
  lat <- as.numeric(location[,"lat"])
  lon <- location[,"lon"]
  
  CF <- data.frame(totalPath(df1,df2,lat,lon))
  
  plot2 <- ggplot(CF, aes(lon, lat, color= factor(group))) +  
    ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
    geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
  
  finalAnswer <- list(CF, totalData[,c("group","tz","lat","lon")], plot1, plot2)
  
  return(finalAnswer)
}

write.csv(airport_data,"thesis2021//airport_data_karlee.csv")


MIA_arrive <- combineData(25.7617,-80.1918,"arrive",1)
write.csv(MIA_arrive,"thesis2021//MIA_arrive_karlee.csv")

MIA_depart <- combineData(25.7617,-80.1918,"depart",1)
write.csv(MIA_depart,"thesis2021//MIA_depart_karlee.csv")

CHI_arrive <- combineData(41.978611, -87.904724,"arrive",1)
write.csv(CHI_arrive,"thesis2021//CHI_arrive_karlee.csv")

CHI_depart <- combineData(41.978611, -87.904724,"depart",1)
write.csv(CHI_depart,"thesis2021//CHI_depart_karlee.csv")

write.csv(CHI_depart[[1]],"thesis2021//CHI_depart_karlee.csv")
write.csv(CHI_depart[[2]],"thesis2021//CHI_depart_flights_karlee.csv")

RDU_arrive <- combineData(35.8801,-78.7880,"arrive",1)
View(RDU_arrive[[2]])

write.csv(RDU_arrive[[1]],"thesis2021//RDU_arrive_karlee.csv")
write.csv(RDU_arrive[[2]],"thesis2021//RDU_arrive_flights_karlee.csv")

night <- RDU_arrive[[2]] %>%
  filter(time_of_day == 0)

night_fun <- RDU_arrive[[1]] %>%
  filter(time_of_day == 0)

colnames(night)
RDU_arrive_night_flights_grouped <- ggplot(night, aes(lon, lat,group = factor(icao24), color = factor(group))) +  
  ggtitle("Flight Routes that Contribute to the 4 Cluster Routes") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2) + theme(legend.position = "none")

RDU_arrive_night_clusters <- ggplot(night_fun, aes(lon, lat,color = factor(group))) +  
  ggtitle("Flights Arriving into RDU") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)

morning <- RDU_arrive[[2]] %>%
  filter(time_of_day == 1)

morning_fun <- RDU_arrive[[1]] %>%
  filter(time_of_day == 1)

RDU_arrive_morning_flights_grouped <- ggplot(morning, aes(lon, lat,group = factor(icao24), color = factor(group))) +  
  ggtitle("Flight Routes that Contribute to the 4 Cluster Routes") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2) + theme(legend.position = "none")
RDU_arrive_morning_flights_grouped
RDU_arrive_morning_clusters <- ggplot(morning_fun, aes(lon, lat,color = factor(group))) +  
  ggtitle("Flights Arriving into RDU") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
RDU_arrive_morning_clusters

afternoon <- RDU_arrive[[2]] %>%
  filter(time_of_day == 2)

afternoon_fun <- RDU_arrive[[1]] %>%
  filter(time_of_day == 2)

RDU_arrive_afternoon_flights_grouped <- ggplot(afternoon, aes(lon, lat,group = factor(icao24), color = factor(group))) +  
  ggtitle("Flight Routes that Contribute to the 4 Cluster Routes") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2) + theme(legend.position = "none")
RDU_arrive_afternoon_flights_grouped
RDU_arrive_afternoon_clusters <- ggplot(afternoon_fun, aes(lon, lat,color = factor(group))) +  
  ggtitle("Flights Arriving into RDU") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
RDU_arrive_afternoon_clusters

View(everything[1])
