usdata <- read.csv("/lfs/karlee_combined_data.csv")
airport_loc <- read.csv("thesis2021/location_data_karlee.csv")

startTime <- 1
lat <- 41.97861
lon <- -87.90472
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
data <- data %>%
  arrange(icao24, time)
data[,"lon"] <- as.numeric(as.character(data[,"lon"]))
data[,"lat"] <- as.numeric(as.character(data[,"lat"]))
plot5 <- ggplot(data, aes(lon, lat, color= factor(icao24))) +  
  ggtitle("All Flights Arriving at Chicago in the Morning") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path(show.legend = FALSE) +
  geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
plot5