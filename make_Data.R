library(tidyverse)

#startTime is the hour of day (0-23)
makeData <- function(lat,lon,startTime){ 
  data <- thirteen_July_data
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

write.csv(thirteen_July_data, "/lfs/thirteen_July_data.csv")
