library(shiny)
library(leaflet)

if (interactive()) {
  
  # Define UI
  ui <- fluidPage(
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
    textOutput("ST")
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
  }
    
  # Complete app with UI and server components
  shinyApp(ui, server)
}
