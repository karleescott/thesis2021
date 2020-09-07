library(tidyverse)
data1 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-00.csv")
data2 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-01.csv")
data3 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-02.csv")
data4 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-03.csv")
data5 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-04.csv")
data6 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-05.csv")
data7 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-06.csv")
data8 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-07.csv")
data9 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-08.csv")
data10 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-09.csv")
data11 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-10.csv")
data12 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-11.csv")
data13 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-12.csv")
data14 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-13.csv")
data15 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-14.csv")
data16 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-15.csv")
data17 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-16.csv")
data18 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-17.csv")
data19 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-18.csv")
data20 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-19.csv")
data21 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-20.csv")
data22 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-21.csv")
data23 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-22.csv")
data24 <- read.csv("/data/ADSB/OpenSky/states_2020-07-13-23.csv")
data <- rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12,data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24)
data <- na.omit(data)
usdata <- data %>%
  filter(lat >= 25 & lat <= 50) %>%
  filter(lon >= -125 & lon <= -65)

write.csv(usdata, "/lfs/karlee_combined_data.csv")
usdata <- read.csv("/lfs/karlee_combined_data.csv")

usdata1 <- usdata %>%
  filter(time == 1594598410)

col_scale <- function(palette = "Spectral", name = "", limits = NULL) {
  scale_colour_distiller(palette = palette,   # spectral colour scale
                         guide = "colourbar", # continuous colour bar
                         name = name,
                         limits = limits)
}

usplot1 <- ggplot(usdata1) + # plot points
  geom_point(aes(x = lon,y = lat, # lon and lat
                 colour = heading), # attribute color
             size = 2) + # make all points larger
  col_scale(name = "heading") + # attach color scale
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  geom_path(data = map_data("state"), # add US states map
            aes(x = long, y = lat, group = group)) +
  coord_fixed(xlim = c(-125, -65),
              ylim = c(25, 50)) + # zoom in
  theme_bw() # B&W theme
usplot1
