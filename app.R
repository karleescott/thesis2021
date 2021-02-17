library(shiny)
library(leaflet)
library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)

if (interactive()) {
  {
    closest_path <- function(fun,lat,lon){
      dis <- 10000
      for(r in 1:nrow(fun)){
        dis1 <- sqrt((fun[r,3]-lon)^2+(fun[r,4]-lat)^2)
        if(dis1<dis){
          dis <- dis1
          loc <- r
        }
      }
      info <- c(dis,loc)
    }
    
    totalPath <- function(df1,df2,lat,lon){
      
      #which path from starting airport gets closest to end airport
      numclusters <- max(df1$group)
      fun <- df1 %>%
        filter(group == 1)
      info <- closest_path(fun,lat,lon)
      for(n in 2:numclusters){
        fun1 <- df1 %>%
          filter(group == n)
        info1 <- closest_path(fun1,lat,lon)
        if(info1[1] < info[1]){
          fun <- fun1
          info <- info1
        }
      }
      
      #which path from ending airport gets closest to the point above
      fun1 <- df2 %>%
        filter(group == min(df2$group))
      lon1 <- fun[info[2],3]
      lat1 <- fun[info[2],4]
      info1 <- closest_path(fun1,lat1,lon1)
      for(n in min(df2$group):max(df2$group)){
        fun2 <- df2 %>%
          filter(group == n)
        info2 <- closest_path(fun2,lat1,lon1)
        if(info2[1] < info1[1]){
          fun1 <- fun2
          info1 <- info2
        }
      }
      
      fun <- fun[1:info[2],]
      fun1 <- fun1[1:info1[2],]
      fun1 <- fun1 %>%
        arrange(desc(tz))
      
      for(r in 1:nrow(fun1)){
        fun1[r,2] <- info[2]+r
      }
      
      route <- rbind(fun,fun1)
      return(route)
    }
    
    #returns full data of all routes, returns data with "best route", returns plots of all routes and best route
    totalFunction <- function(starting_airport,ending_airport,startingTime,threshold){
      airport_data <- read.csv("thesis2021//airport_data_karlee.csv")
      airport_data <- airport_data[,-1]
      df1 <- airport_data %>%
        filter(airport == starting_airport & time_of_day == startingTime)
      df1 <- df1[,-5]
      df1 <- df1[,-5]
      
      df2 <- airport_data %>%
        filter(airport == ending_airport & time_of_day == startingTime)
      df2 <- df2[,-5]
      df2 <- df2[,-5]
      
      numclusters <- length(unique(df1$group))
      
      for(i in 1:nrow(df2)){
        df2[i,1] <- df2[i,1] + numclusters
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
        ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
        geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
      
      location <- read.csv("thesis2021//location_data_karlee.csv")
      location <- location[,-1]
      location <- location %>%
        filter(airport == ending_airport)
      lat <- as.numeric(location[,2])
      lon <- location[,3]
      
      CF <- data.frame(totalPath(df1,df2,lat,lon))
      
      plot2 <- ggplot(CF, aes(lon, lat, color= factor(group))) +  
        ggtitle("Flight Paths") + xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + xlim(-125, - 65) + ylim(25, 50) + geom_path() +
        geom_path(data = conversion, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .2)
      
      finalAnswer <- list(CF, totalData, plot1, plot2)
      
      return(finalAnswer)
    }
  }
  
  # Define UI
  ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
          selectInput("starting_airport", "Starting Airport:",
                c("MIA","RDU")),
          textOutput("SA"),
          br(),
          selectInput("ending_airport", "Ending Airport:",
                c("MIA","RDU")),
          textOutput("EA"),
          br(),
          selectInput("startingTime", "Departure Time:",
                c("morning","afternoon","evening","night")),
          textOutput("ST")),
        mainPanel(
          plotOutput("plot"),
          dataTableOutput("table"))
  )
)
  
  # Server logic
  server <- function(input, output) {
    output$SA <- renderText({
      st_air <- input$starting_airport
      return(paste('You chose', st_air, " as your starting airport"))
    })
    
    output$EA <- renderText({
      end_air <- input$ending_airport
      if(end_air != input$starting_airport){
        return(paste("You chose", end_air, " as your ending airport"))
      }
      else{
        stop(
          return(paste("You cannot have the same starting and ending airport"))
        )
      }
    })
    
    output$ST <- renderText({
      start_time <- input$startingTime
      return(paste("You chose to depart in the ", start_time))
      })
    
    output$plot <- renderPlot({
      time_of_day <- 0
      if(input$startingTime == "morning"){
        time_of_day <- 1
      }
      if(input$startingTime == "afternoon"){
        time_of_day <- 2
      }
      if(input$startingTime == "evening"){
        time_of_day <- 3
      }
      finalAnswer <- totalFunction(input$starting_airport,input$ending_airport,time_of_day,1)
      finalAnswer[4]
    })
    
    output$table <- renderDataTable({
      time_of_day <- 0
      if(input$startingTime == "morning"){
        time_of_day <- 1
      }
      if(input$startingTime == "afternoon"){
        time_of_day <- 2
      }
      if(input$startingTime == "evening"){
        time_of_day <- 3
      }
      finalAnswer <- totalFunction(input$starting_airport,input$ending_airport,time_of_day,1)
      route <- as.data.frame(finalAnswer[1])
      route <- route[,-1]
      colnames(route) <- c("Order of Travel","Longitude","Latitude")
      return(route)
    })
  }
    
  # Complete app with UI and server components
  shinyApp(ui, server)
}