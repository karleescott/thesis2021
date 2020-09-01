library("dplyr")
library("tidyr")
station <- read.csv("C:/Users/Karlee Scott/OneDrive - West Point/AY 21-1/Thesis/Data/stations.csv", header = TRUE)
times <-  read.csv("C:/Users/Karlee Scott/OneDrive - West Point/AY 21-1/Thesis/Data/times.csv", header = TRUE)
tmax <- read.csv("C:/Users/Karlee Scott/OneDrive - West Point/AY 21-1/Thesis/Data/tmax.csv", header = TRUE, check.names=FALSE)
tmin <- read.csv("C:/Users/Karlee Scott/OneDrive - West Point/AY 21-1/Thesis/Data/tmin.csv", header = TRUE, check.names=FALSE)
DP <- read.csv("C:/Users/Karlee Scott/OneDrive - West Point/AY 21-1/Thesis/Data/DP.csv", header = TRUE, check.names=FALSE)
Prec <- read.csv("C:/Users/Karlee Scott/OneDrive - West Point/AY 21-1/Thesis/Data/prec.csv", header = TRUE, check.names=FALSE)
#combine tmax data with times
tmax <- cbind(times,tmax)
tmax_long <- gather(tmax, id, z, -Julian, -Year, -Month, -Day)
tmax_long$id <- as.integer(tmax_long$id)
# remove unknown data
tmax_long <- filter(tmax_long, !(z <= -9998))
tmax_long <- mutate(tmax_long, proc = "tmax")

tmin <- cbind(times,tmin)
tmin_long <- gather(tmin, id, z, -Julian, -Year, -Month, -Day)
tmin_long$id <- as.integer(tmin_long$id)
tmin_long <- filter(tmin_long, !(z <= -9998))
tmin_long <- mutate(tmin_long, proc = "tmin")

DP <- cbind(times,DP)
DP_long <- gather(DP, id, z, -Julian, -Year, -Month, -Day)
DP_long$id <- as.integer(DP_long$id)
DP_long <- filter(DP_long, !(z <= -999.8))
DP_long <- mutate(DP_long, proc = "DP")

Prec <- cbind(times,Prec)
Prec_long <- gather(Prec, id, z, -Julian, -Year, -Month, -Day)
Prec_long$id <- as.integer(Prec_long$id)
Prec_long <- filter(Prec_long, !(z <= -99.8))
Prec_long <- mutate(Prec_long, proc = "Prec")

NOAA_df_1990 <- rbind(tmax_long, tmin_long, DP_long, Prec_long)
NOAA_df_1990 <- left_join(NOAA_df_1990, station, by = "id")


install.packages("sp")
install.packages("spacetime")
library("sp")
library("spacetime")

#Constructing an STIDF Object
NOAA_df_1990$date <- with(NOAA_df_1990, paste(Year, Month, Day, sep = "-"))
NOAA_df_1990$date <- as.Date(NOAA_df_1990$date)
tmax_long2 <- filter(NOAA_df_1990, proc == "tmax")
STObj <- stConstruct(x = tmax_long2, space = c("long", "lat"), time = "date")
spat_part <- SpatialPoints(coords = tmax_long2[, c("long", "lat")])
temp_part <- tmax_long2$date
STObj2 <- STIDF(sp = spat_part, time = temp_part, data = select(tmax_long2, -date, -long, -lat))

#Constructing an STFDF Object
spat_part <- SpatialPoints(coords = station[, c("long", "lat")])
temp_part <- with(times, paste(Year, Month, Day, sep = "-"))
temp_part <- as.Date(temp_part)
tmax_long3 <- gather(tmax, id, z, -Julian, -Year, -Month, -Day)
tmax_long3$id <- as.integer(tmax_long3$id)
tmax_long3 <- arrange(tmax_long3,Julian,id)
STObj3 <- STFDF(sp = spat_part, time = temp_part, data = tmax_long3)
proj4string(STObj3) <- CRS("+proj=longlat +ellps=WGS84")
STObj3$z[STObj3$z == -9999] <- NA

install.packages("animation")
install.packages("gstat")
install.packages("maps")
library("animation")
library("dplyr")
library("ggplot2")
library("gstat")
library("maps")
set.seed(1)
tmax <- filter(NOAA_df_1990, proc == "tmax" & Month %in% 5:9 & Year == 1993)
tmax$t <- tmax$Julian - 728049
tmax_1 <- subset(tmax, t %in% c(1, 15, 30)) 
col_scale <- function(palette = "Spectral", name = "", limits = NULL) {
  scale_colour_distiller(palette = palette,   # spectral colour scale
                         guide = "colourbar", # continuous colour bar
                         name = name,
                         limits = limits)
}

NOAA_plot <- ggplot(tmax_1) + # plot points
  geom_point(aes(x = long,y = lat, # lon and lat
                 colour = z), # attribute color
             size = 2) + # make all points larger
  col_scale(name = "degF") + # attach color scale
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  geom_path(data = map_data("state"), # add US states map
            aes(x = long, y = lat, group = group)) +
  facet_grid(~date) + # facet by time
  coord_fixed(xlim = c(-105, -75),
              ylim = c(25, 50)) + # zoom in
  theme_bw() # B&W theme
NOAA_plot

UIDs <- unique(tmax$id) # extract IDs
UIDs_sub <- sample(UIDs, 10) # sample 10 IDs
tmax_sub <- filter(tmax, id %in% UIDs_sub) # subset data

tmaxTS <- ggplot(tmax_sub) +
  geom_line(aes(x = t, y = z)) + # line plot of z against t
  facet_wrap(~id, ncol = 5) + # facet by station
  xlab("Day number (days)") + # x label
  ylab("Tmax (degF)") + # y label
  theme_bw() + # BW theme
  theme(panel.spacing = unit(1, "lines")) # facet spacing
tmaxTS

#Hovmöller Plots
lim_lat <- range(tmax$lat) # latitude range
lim_t <- range(tmax$t) # time range
lat_axis <- seq(lim_lat[1], # latitude axis
                lim_lat[2],
                length=25)
t_axis <- seq(lim_t[1], # time axis
              lim_t[2],
              length=100)
lat_t_grid <- expand.grid(lat = lat_axis,
                          t = t_axis)
tmax_grid <- tmax
dists <- abs(outer(tmax$lat, lat_axis, "-"))
tmax_grid$lat <- lat_axis[apply(dists, 1, which.min)]
tmax_lat_Hov <- group_by(tmax_grid, lat, t) %>%
  summarise(z = mean(z))
fill_scale <- function(palette = "Spectral", name = "", limits = NULL) {
  scale_fill_distiller(palette = palette,   # spectral colour scale
                       guide = "colourbar", # continuous colour bar
                       name = name,
                       limits = limits)
}
Hovmoller_lat <- ggplot(tmax_lat_Hov) + # take data
  geom_tile(aes(x = lat, y = t, fill = z)) + # plot
  fill_scale(name = "degF") + # add color scale
  scale_y_reverse() + # rev y scale
  ylab("Day number (days)") + # add y label
  xlab("Latitude (degrees)") + # add x label
  theme_bw() # change theme
Hovmoller_lat

#annimations
tmax_t <- function(tau) {
  tmax_sub <- filter(tmax, t == tau) # subset data
  ggplot(tmax_sub) +
    geom_point(aes(x = long,y = lat, colour = z), # plot
               size = 4) + # pt. size
    col_scale(name = "z", limits = c(40, 110)) +
    theme_bw() # B&W theme
}
gen_anim <- function() {
  for(t in lim_t[1]:lim_t[2]){ # for each time point
    plot(tmax_t(t)) # plot data at this time point
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
         htmlfile = "NOAA_anim.html") # save filename

#Empirical Spatial Means
spat_av <- group_by(tmax, lat, long) %>% # group by lon-lat
  summarise(mu_emp = mean(z)) # mean for each lon-lat
lat_means <- ggplot(spat_av) +
  geom_point(aes(lat, mu_emp)) +
  xlab("Latitude (deg)") +
  ylab("Maximum temperature (degF)") + theme_bw()
lat_means
lon_means <- ggplot(spat_av) +
  geom_point(aes(long, mu_emp)) +
  xlab("Longitude (deg)") +
  ylab("Maximum temperature (degF)") + theme_bw()
lon_means

#Empirical Temoral Means
tmax_av <- group_by(tmax, date) %>%
  summarise(meantmax = mean(z))
gtmaxav <-
  ggplot() +
  geom_line(data = tmax,aes(x = date, y = z, group = id),
            colour = "blue", alpha = 0.04) +
  geom_line(data = tmax_av, aes(x = date, y = meantmax)) +
  xlab("Month") + ylab("Maximum temperature (degF)") +
  theme_bw()
gtmaxav

#Empirical Covariances
lm1 <- lm(z ~ lat + t + I(t^2), data = tmax) # fit a linear model
tmax$residuals <- residuals(lm1) # store the residuals
spat_df <- filter(tmax, t == 1) %>% # lon/lat coords of stations
  select(long, lat) %>% # select lon/lat only
  arrange(long, lat) # sort ascending by lon/lat
m <- nrow(spat_av) # number of stations
X <- select(tmax, long, lat, residuals, t) %>% # select columns
  spread(t, residuals) %>% # make time-wide
  select(-long, -lat) %>% # drop coord info
  t() # make space-wide
Lag0_cov <- cov(X, use = 'complete.obs')
Lag1_cov <- cov(X[-1, ], X[-nrow(X),], use = 'complete.obs')
spat_df$n <- 1:nrow(spat_df) # assign an index to each station
lim_lon <- range(spat_df$lon) # range of lon coordinates
lon_strips <- seq(lim_lon[1], # create 4 long. strip boundaries
                  lim_lon[2],
                  length = 5)
spat_df$lon_strip <- cut(spat_df$lon, # bin the lon into
                         lon_strips, # their respective bins
                         labels = FALSE, # don't assign labels
                         include.lowest = TRUE) # include edges
install.packages("fields")
plot_cov_strips <- function(C,spat_df) {  
  # for each longitudinal strip 
  require(fields)   # load fields for plotting
  for(i in seq_along(unique(spat_df$lon_strip))){                        
    spat_strip <- spat_df %>%         # take spat_df
      filter(lon_strip == i)  %>%   # extract the ith strip
      arrange(lat)                  # sort by latitude
    idx <- spat_strip$n               # extract indices of locations
    jitter <- seq(0,0.0001,           # add jitter for locations that
                  length=length(idx)) # have same latitude component
    image.plot(spat_strip$lat+jitter, # plot the matrix using fields
               spat_strip$lat+jitter,
               C[idx,idx],            # subset and permute C
               xlab="latitude",
               ylab="latitude",
               zlim=c(-15,85),
               col=tim.colors(10),
               cex=200)
  }
}
plot_cov_strips(Lag0_cov, spat_df) # plot the lag-0 matrices
plot_cov_strips(Lag1_cov, spat_df) # plot the lag-1 matrices

#Semivariogram Analysis
STObj4 <- STObj3[, "1993-07-01::1993-07-31"]
vv <- variogram(object = z~1 + lat, # fixed effect component
                data = STObj4, # July data
                width = 80, # spatial bin (80 km)
                cutoff = 1000, # consider pts < 1000 km apart
                tlags = 0.01:6.01) # 0 days to 6 days
plot(vv)
