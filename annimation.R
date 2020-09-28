library(tidyverse)

usdata <- read.csv("/lfs/karlee_combined_data.csv")

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
gradientLegend(valRange = c(min(plotdata$z), max(plotdata$z)), color = hcl.colors(12, "YlOrRd", rev = TRUE), inside = FALSE, n.seg = 5)

#make animation
library("animation")
lim_t <- range(usdata$time)
#https://stackoverflow.com/questions/46017812/r-get-all-categories-in-column
categories <- unique(usdata$time) 

gen_anim <- function() {
  for(temps in categories){ # for each time point
    par(mar = c(5.1, 4.1, 4.1, 7))
    tmax_sub <- usdata %>%
      filter(time == temps)
    plotdata <- kde2d(tmax_sub$lon,tmax_sub$lat, n = 500)
    image(plotdata,
          main = "Intensity Plot", 
          xlab = "Longitude (degrees)", 
          ylab = "Latitude (degrees)")
    plot(out, add = TRUE)
    gradientLegend(valRange = c(min(plotdata$z), max(plotdata$z)), color = hcl.colors(12, "YlOrRd", rev = TRUE), inside = FALSE, n.seg = 5) # plot data at this time point
  }
}

ani.options(interval = 0.2) # 0.2s interval between frames
saveHTML(gen_anim(), # run the main function
         autoplay = FALSE, # do not play on load
         loop = FALSE, # do not loop
         verbose = FALSE, # no verbose
         outdir = ".", # save to current dir
         single.opts = "'controls': ['first', 'previous',
'play', 'next', 'last',
'loop', 'speed'],
'delayMin': 0",
         htmlfile = "Intensity_anim.html") # save filename
