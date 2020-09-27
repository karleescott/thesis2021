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

# load the mapdata 
map <- readOGR("states_21basic/states.shp")


out <- crop(map, extent(-125, -65, 25, 50))

par(mar = c(5.1, 4.1, 4.1, 7))

#https://stackoverflow.com/questions/16201906/how-can-i-get-the-value-of-a-kernel-density-estimate-at-specific-points
library(MASS)
plotdata <- kde2d(usdata1$lon,usdata1$lat, n = 1813)
image(plotdata,
      main = "Intensity Plot", 
      xlab = "Longitude (degrees)", 
      ylab = "Latitude (degrees)")
plot(out, add = TRUE)

#http://search.r-project.org/library/plotfunctions/html/gradientLegend.html
library(plotfunctions)
gradientLegend(valRange = c(min(plotdata$z), max(plotdata$z)), color = hcl.colors(12, "YlOrRd", rev = TRUE), inside = FALSE, n.seg = 5)

View(plotdata[['z']])
kde2d
