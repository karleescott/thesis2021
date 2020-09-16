library(tidyverse)
library(spatstat)

usdata <- read.csv("/lfs/karlee_combined_data.csv")
usdata1 <- usdata %>%
  filter(time == 1594598410)
usdata1 <- na.omit(usdata1)

#http://finzi.psych.upenn.edu/R/library/spatstat/html/plot.ppp.html
x <- as.vector(usdata1$lon)
y <- as.vector(usdata1$lat)
P <- ppp(x, y, c(-125, -65), c(25, 50), unitname=c("degrees","degrees"))
P
summary(P)
plot(P)
#bw.nrd0 implements a rule-of-thumb for choosing the bandwidth of a Gaussian kernel density estimator. 
#It defaults to 0.9 times the minimum of the standard deviation and the interquartile range divided by 
#1.34 times the sample size to the negative one-fifth power (= Silverman's ‘rule of thumb’, Silverman 
#(1986, page 48, eqn (3.31))) unless the quartiles coincide when a positive result will be guaranteed.
plot(density(P, bw = "nrd0", kernel = "gaussian"), 
     main = "Intensity Plot", 
     axes = TRUE,
     ann = TRUE, 
     xlab = "Longitude (degrees)", 
     ylab = "Latitude (degrees)")

library(rgdal)
library(raster)
library(rgeos)
# unzip the file

unzip(zipfile = "thesis2021/states_21basic.zip", 
      exdir = 'states_21basic')

# load the data 
map <- readOGR("states_21basic/states.shp")

plot(map)
summary(map)

out <- crop(map, extent(-125, -65, 25, 50))
plot(out, add = TRUE)


