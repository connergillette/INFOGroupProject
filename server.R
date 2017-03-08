library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# For map visualization
library(sp)
library(leaflet)
library(htmltools)

url <- 'https://api.mapbox.com/styles/v1/connergillette/cizyy5a9m004m2smjhu438php/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiY29ubmVyZ2lsbGV0dGUiLCJhIjoiY2l6cWIwOWJuMDE3azMzcDdvdmx3eWMwMyJ9.Qa235PPv1uPZtvaWIUSpQA'

df <- read.csv('filtered_college_data.csv', stringsAsFactors=FALSE)
commonwealth.territories <- list("American Samoa" = "AS",
                                 "District of Columbia" = "DC",
                                 "Federated States of Micronesia" = "FM",
                                 "Guam" = "GU",
                                 "Marshall Islands" = "MH",
                                 "Northern Mariana Islands" = "MP",
                                 "Palau" = "PW",
                                 "Puerto Rico" = "PR",
                                 "Virgin Islands" = "VI")

shinyServer(function(input, output) {
  
  url <- 'https://api.mapbox.com/styles/v1/connergillette/cizyy5a9m004m2smjhu438php/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiY29ubmVyZ2lsbGV0dGUiLCJhIjoiY2l6cWIwOWJuMDE3azMzcDdvdmx3eWMwMyJ9.Qa235PPv1uPZtvaWIUSpQA'
  
  #df <- head(df, 100)
  #View(df)
  filtered.data <- reactive({
    full.data.set <- read.csv('filtered_college_data.csv',stringsAsFactors = FALSE)
    # Filters for States, if it is the united states it doesn't filter anything.
    if(input$state != "United States"){
      if(input$state %in% state.name){
        full.data.set <- filter(full.data.set, STABBR == state.abb[grep(input$state,state.name)])
      }else{
        full.data.set <- filter(full.data.set, STABBR == commonwealth.territories[input$state])
      }
    }
    min.range <- input$admission[1]/100
    max.range <- input$admission[2]/100
    # Filter for Admission Rate
    full.data.set <- filter(full.data.set, ADM_RATE >= min.range & ADM_RATE <= max.range)
    # Filter for Majors
    
    return(as.data.frame(full.data.set))
  })
  
  output$map <- renderLeaflet({
    df <- filtered.data()
    df$LATITUDE <- as.numeric(df$LATITUDE)
    df$LONGITUDE <- as.numeric(df$LONGITUDE)
    
    popup.content <- paste(sep = '<br />', paste0('<a target="_blank" href="http://', df$INSTURL, '">', 
                                                  df$INSTNM, '</a>'), paste0('<b>Location: </b>', df$CITY, ', ', df$STABBR), ifelse(df$ADM_RATE != 'NULL', 
                                                                                                                                    paste0('<b>Admission Rate: </b>', as.numeric(df$ADM_RATE) * 100, '%'), '<b>Admission Rate:</b> Not given'))
    
    leaflet(df) %>% 
      addCircleMarkers(popup = ~htmlEscape(paste0(INSTNM, ', ', CITY)), stroke = FALSE, fillOpacity = 1, radius = 4) %>% 
      addTiles(urlTemplate = url, attribution = 'Maps provided by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
      setView(lng = -104.82, lat = 47.55, zoom = 3)
  })
  
  output$list <- renderPrint({})
})


