library(tidyverse)

usdata <- read.csv("/lfs/karlee_combined_data.csv")

#makes the data to be used in total Function. startTime is the hour of day (0-23)
makeData <- function(lat,lon,startTime){ 
  data <- usdata
  data <- data %>%
    filter(time >= (1594598400+3600*startTime) & time < (1594598400+3600*(startTime+1)))
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
  numclusters = 4
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

compareMean <- function(fun1, fun2, threshold){
  numclusters = 4
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
combineData <- function(lat,lon){
  data <- makeData(lat,lon,0)
  everything <- makeCluster(data)
  everything1 <- makeCluster(data.frame(everything[1]))
  fun1 <- data.frame(everything[2])
  fun2 <- data.frame(everything1[2])
  while(compareMean(fun1,fun2,threshold) == "False"){
    fun1 <- fun2
    everything1 <- makeCluster(data.frame(everything1[1]))
    fun2 <- data.frame(everything1[2])
  }
  
  data <- cbind(as.data.frame(everything1[2]),hour = 0)
  
  for (i in range(1,24)){
    data1 <- makeData(lat,lon,i)
    everything <- makeCluster(data1)
    everything2 <- makeCluster(data.frame(everything[1]))
    fun1 <- data.frame(everything[2])
    fun2 <- data.frame(everything2[2])
    while(compareMean(fun1,fun2,threshold) == "False"){
      fun1 <- fun2
      everything2 <- makeCluster(data.frame(everything2[1]))
      fun2 <- data.frame(everything2[2])
    }
    
    data <- cbind(as.data.frame(everything2[2]),hour = i)
    
    data <- rbind(data,data1)

  }
  return(data)
}

#Coersed NA on purpose, ignore errors. Takes about 5 minutes to run

#smallest distance between two functions
minimumDistance <- function(fun1,fun2){
  dis <- 180
  for (i in 1:nrow(fun1)){
    for(j in 1:nrow(fun2)){
      num <- sqrt((fun1[i,3]-fun2[j,3])^2+(fun1[i,4]-fun2[j,4])^2)
      if(num<dis){
        dis <- num
        loc1 <- i
        loc2 <- j
      }
    }
  }
  minDisLoc <- list(dis,loc1,loc2)
  return(minDisLoc)
}

#combines closest functions to form best path
totalPath <- function(df1,df2){
  fun1 <- df1 %>%
    filter(group == 1)
  fun2 <- df1 %>%
    filter(group == 2)
  fun3 <- df1 %>%
    filter(group == 3)
  fun4 <- df1 %>%
    filter(group == 4)
  fun5 <- df2 %>%
    filter(group == 5)
  fun6 <- df2 %>%
    filter(group == 6)
  fun7 <- df2 %>%
    filter(group == 7)
  fun8 <- df2 %>%
    filter(group == 8)
  
  mindis15 <- data.frame(minimumDistance(fun1,fun5))
  colnames(mindis15) <- c("Distance","Loc1","Loc2")
  mindis16 <- data.frame(minimumDistance(fun1,fun6))
  colnames(mindis16) <- c("Distance","Loc1","Loc2")
  mindis17 <- data.frame(minimumDistance(fun1,fun7))
  colnames(mindis17) <- c("Distance","Loc1","Loc2")
  mindis18 <- data.frame(minimumDistance(fun1,fun8))
  colnames(mindis18) <- c("Distance","Loc1","Loc2")
  
  mindis25 <- data.frame(minimumDistance(fun2,fun5))
  colnames(mindis25) <- c("Distance","Loc1","Loc2")
  mindis26 <- data.frame(minimumDistance(fun2,fun6))
  colnames(mindis26) <- c("Distance","Loc1","Loc2")
  mindis27 <- data.frame(minimumDistance(fun2,fun7))
  colnames(mindis27) <- c("Distance","Loc1","Loc2")
  mindis28 <- data.frame(minimumDistance(fun2,fun8))
  colnames(mindis28) <- c("Distance","Loc1","Loc2")
  
  mindis35 <- data.frame(minimumDistance(fun3,fun5))
  colnames(mindis35) <- c("Distance","Loc1","Loc2")
  mindis36 <- data.frame(minimumDistance(fun3,fun6))
  colnames(mindis36) <- c("Distance","Loc1","Loc2")
  mindis37 <- data.frame(minimumDistance(fun3,fun7))
  colnames(mindis37) <- c("Distance","Loc1","Loc2")
  mindis38 <- data.frame(minimumDistance(fun3,fun8))
  colnames(mindis38) <- c("Distance","Loc1","Loc2")
  
  mindis45 <- data.frame(minimumDistance(fun4,fun5))
  colnames(mindis45) <- c("Distance","Loc1","Loc2")
  mindis46 <- data.frame(minimumDistance(fun4,fun6))
  colnames(mindis46) <- c("Distance","Loc1","Loc2")
  mindis47 <- data.frame(minimumDistance(fun4,fun7))
  colnames(mindis47) <- c("Distance","Loc1","Loc2")
  mindis48 <- data.frame(minimumDistance(fun4,fun8))
  colnames(mindis48) <- c("Distance","Loc1","Loc2")
  
  mindis <- rbind(mindis15,mindis16,mindis17,mindis18,mindis25,mindis26,mindis27,mindis28,mindis35,mindis36,mindis37,mindis38,mindis45,mindis46,mindis47,mindis48)
  
  minIndex <- which.min(mindis[,1])
  selMinDis <- mindis[minIndex,]
  
  if(minIndex/4<=1){
    Fun1 <- fun1 %>%
      filter(tz < as.numeric(selMinDis[2]))
    Fun2 <- df2[df2$group==minIndex+4,] %>%
      filter(tz < as.numeric(selMinDis[3]))
    
    combinedFunction <- rbind(Fun1,Fun2)
  } 
  
  else if(minIndex/4<=2){
    Fun1 <- fun2 %>%
      filter(tz < as.numeric(selMinDis[2]))
    Fun2 <- df2[df2$group==minIndex,] %>%
      filter(tz < as.numeric(selMinDis[3]))
    
    combinedFunction <- rbind(Fun1,Fun2)
  }
  
  else if(minIndex/4<=3){
    Fun1 <- fun3 %>%
      filter(tz < as.numeric(selMinDis[2]))
    Fun2 <- df2[df2$group==minIndex-4,] %>%
      filter(tz < as.numeric(selMinDis[3]))
    
    combinedFunction <- rbind(Fun1,Fun2)
  }
  
  else {
    Fun1 <- fun4 %>%
      filter(tz < as.numeric(selMinDis[2]))
    Fun2 <- df2[df2$group==minIndex-8,] %>%
      filter(tz < as.numeric(selMinDis[3]))
    
    combinedFunction <- rbind(Fun1,Fun2)
    
  }
  
  for (i in 1:nrow(combinedFunction)){
    combinedFunction[i,2] <- i
  }
  
  return(combinedFunction)
  
}

#returns full data of all routes, returns data with "best route", returns plots of all routes and best route
totalFunction <- function(starting_airport,ending_airport,startingTime,threshold){
  if(starting_airport == "RDU"){
    df1 <- read.csv("/lfs/karlee_RDU.csv")
    df1 <- df1 %>%
      filter(hour == startingTime)
  }
  else {
    df1 <- read.csv("/lfs/karlee_MIA.csv")
    df1 <- df1 %>%
      filter(hour == startingTime)
  }
  
  if(ending_airport == "RDU"){
    df2 <- read.csv("/lfs/karlee_RDU.csv")
    df2 <- df2 %>%
      filter(hour == startingTime)
  }
  else {
    df2 <- read.csv("/lfs/karlee_MIA.csv")
    df2 <- df2 %>%
      filter(hour == startingTime)
  }
  
  for(i in 1:nrow(df2)){
    df2[i,1] <- df2[i,1] + 4
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
    ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-90, - 65) + ylim(25, 50) + geom_path() +
    geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
  
  CF <- data.frame(totalPath(df1,df2))
  
  plot2 <- ggplot(CF, aes(lon, lat, color= factor(group))) +  
    ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-90, - 65) + ylim(25, 50) + geom_path() +
    geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
  
  finalAnswer <- list(CF, totalData, plot1, plot2)
  
  return(finalAnswer)
}

#finalAnswer <- totalFunction(35.8801,-78.7880,25.7617,-80.1918,1)
finalAnswer <- totalFunction(RDU,MIA,1)

