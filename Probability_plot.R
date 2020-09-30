library(tidyverse)

usdata <- read.csv("/lfs/karlee_combined_data.csv")
firstdata <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-00.csv")
firstdata <- na.omit(firstdata)
firstdata <- firstdata %>%
  filter(lon >= -125 & lon <= -65 & lat >= 25 & lat <= 50)

usdata1 <- usdata %>%
  filter(time == 1594598410)


library(rgdal)
library(raster)
library(rgeos)

# unzip the zipfile
unzip(zipfile = "thesis2021/states_21basic.zip", 
      exdir = 'states_21basic')

# load the shapefile 
map <- readOGR("states_21basic/states.shp")

#crop the portion needed
out <- crop(map, extent(-125, -65, 25, 50))

#margins of plot
par(mar = c(5.1, 4.1, 4.1, 7))

#density plot
library(MASS)
plotdata <- kde2d(usdata1$lon,usdata1$lat, n = 500)
image(plotdata,
      main = "Intensity Plot", 
      xlab = "Longitude (degrees)", 
      ylab = "Latitude (degrees)")

#overlay map
plot(out, add = TRUE)

#add gradient legend
#http://search.r-project.org/library/plotfunctions/html/gradientLegend.html
library(plotfunctions)
gradientLegend(valRange = c(0, .004), color = hcl.colors(12, "YlOrRd", rev = TRUE), inside = FALSE, n.seg = 5)

#Normalize densities for probability plot
sum <- sum(plotdata$z)
plotdataZ <- plotdata$z
q <- 1
for(i in plotdataZ) {
  plotdataZ[q] <- i/sum
  q <- q + 1
}
sum(plotdataZ)
View(plotdataZ)

plotdata$z <- plotdataZ
image(plotdata,
      main = "Probability Plot", 
      xlab = "Longitude (degrees)", 
      ylab = "Latitude (degrees)")
plot(out, add = TRUE)
gradientLegend(valRange = c(min(plotdata$z), max(plotdata$z)), color = hcl.colors(12, "YlOrRd", rev = TRUE), inside = FALSE, n.seg = 5)
