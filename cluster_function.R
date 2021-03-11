library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)

usdata <- read.csv("/lfs/karlee_combined_data.csv")

#makes the data to be used in total Function. startTime is the hour of day (0-23)
makeData <- function(lat,lon,startTime,numclusters){ 
  #lat <- 35.8801
  #lon <- -78.7880
  #startTime <- 0
  print(startTime)
  data <- usdata
  data <- data %>%
    filter(time >= (1594598400+3600*6*(startTime)) & time < (1594598400+3600*6*(startTime+1)))
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
    distance[row] <- sqrt((as.numeric(as.character(data[row,"lon"]))-(lon))^2 + (as.numeric(as.character(data[row,"lat"]))-(lat))^2)
  }
  
  data <- cbind(data, distance)
  
  #find ids that pass through RDU (1 degree buffer)
  list = c()
  i = 1
  for (row in 1:nrow(data)){
    if(data[row,"distance"] <= 1) {
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
    if(as.character(data[r,"icao24"]) == as.character(start[i,1])){
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
  data[,"time"] <- as.numeric(as.character(data[,"time"]))
  data <- data %>%
    filter(time>=st)
  
  #remove data that is a second flight from same icao24 (aka lands and then re take's off)
  data <- data %>%
    arrange(icao24, time)
  
  for(i in 1:length(res)){
    data1 <- data %>%
      filter(icao24 == res[i])
    j <- 1
    while(j <= nrow(data1)){
      if(data1[j,"onground"] == "TRUE" & data1[j,"distance"] > 1){
        last_time <- data1[j,"time"] 
        j <- nrow(data1) + 1
      } else{
        last_time <- data1[j,"time"] 
        j <- j + 1
      }
    }
    data <- data[!(data$icao24 == res[i] & data$time > last_time),]
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
    if(as.character(data[r,"icao24"]) == as.character(fundata[i,1])){
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
  
  
  for(n in 1:numclusters){
    data1 <- data %>%
      filter(group == n)
    tz_count <- table(data1$tz)
    tz_count <- as.data.frame(tz_count)
    tz_count <- tz_count %>%
      filter(Freq < 5)
    cap <- as.numeric(as.character(tz_count[1,1]))
    data <- data[!(data$group==n & data$tz>=cap),]
  }
  
  return(data)
}

makeCluster <- function(data) {
  numclusters <- as.numeric(length(unique(data$group)))
  groups <- sort(unique(data$group))
  for(r in 1:nrow(data)){
    data[r,"group"] <- match(data[r,"group"],groups)
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
  for (r in 1:nrow(data)){
    for (n in 1:numclusters){
      data1 <- fun %>%
        filter(group == n)
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
  list <- data[,"icao24"]
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
    if(data[r,"icao24"] == highli[i,1]){
      group[r] <- highli[i,2]
    }
    else{
      i <- i + 1
      group[r] <- highli[i,2]
    }
  }
  data[,data_length] <- group
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
combineData <- function(lat,lon,arrive_depart,threshold,numclusters){
  if(arrive_depart == "depart"){
    data <- makeData(lat,lon,0,numclusters)}
  else{
    data <- makeData2(lat,lon,0,numclusters)
  }
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
  likelihood <- as.data.frame(everything1[4])
  
  #for (i in 1:3){
    #if(arrive_depart == "depart"){
     # data1 <- makeData(lat,lon,i,numclusters)}
   # else{
      #data1 <- makeData2(lat,lon,i,numclusters)
   # }
    #everything <- makeCluster(data1)
    #everything2 <- makeCluster(data.frame(everything[1]))
    #fun1 <- data.frame(everything[2])
   # fun2 <- data.frame(everything2[2])
   # j = 0
   # while(compareMean(fun1,fun2,threshold) == "False" || j == 10){
     # fun1 <- fun2
     # everything2 <- makeCluster(data.frame(everything2[1]))
     # fun2 <- data.frame(everything2[2])
      #j <- j + 1
  #  }
    
  #  data1 <- cbind(as.data.frame(everything2[2]),as.data.frame(everything2[4]),time_of_day = i)
    
   # data <- rbind(data,data1)
    
  #}
  list <- list(data, likelihood)
  return(list)
}

#2 clusters Chicago night
everything2 <- combineData(41.978611, -87.904724,"depart",1,2)
j2 = 0
likelihood2 <- data.frame(everything2[2])
i = 1
while(i < nrow(likelihood2)){
  j2 = j2 + log(max(likelihood2[i,3],likelihood2[i+1,3]))
  i = i + 2
}

#3 clusters
everything3 <- combineData(41.978611, -87.904724,"depart",1,3)
j3 = 0
likelihood3 = data.frame(everything3[2])
i = 1
while(i < nrow(likelihood3)){
  j3 = j3 + log(max(likelihood3[i,3],likelihood3[i+1,3],likelihood3[i+2,3]))
  i = i + 3
}

#4 clusters
everything4 <- combineData(41.978611, -87.904724,"depart",1,4)
j4 = 0
likelihood4 = data.frame(everything4[2])
i = 1
while(i < nrow(likelihood4)){
  j4 = j4 + log(max(likelihood4[i,3],likelihood4[i+1,3],likelihood4[i+2,3],likelihood4[i+3,3]))
  i = i + 4
}

#5 clusters
everything5 <- combineData(41.978611, -87.904724,"depart",1,5)
j5 = 0
likelihood5 = data.frame(everything5[2])
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
