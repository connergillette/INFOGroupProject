library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# For map visualization
library(sp)
library(leaflet)
library(htmltools)

shinyServer(function(input, output) {
    url <- 'https://api.mapbox.com/styles/v1/connergillette/cizyy5a9m004m2smjhu438php/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiY29ubmVyZ2lsbGV0dGUiLCJhIjoiY2l6cWIwOWJuMDE3azMzcDdvdmx3eWMwMyJ9.Qa235PPv1uPZtvaWIUSpQA'
    
    df <- as.data.frame(read.csv('MERGED2014_15_PP.csv', stringsAsFactors = FALSE)) %>% select(INSTNM, CITY, STABBR, ZIP, LATITUDE, LONGITUDE, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, starts_with('PCIP'), UGDS, TUITIONFEE_IN, TUITIONFEE_OUT
    )
    #df <- head(df, 100)
    #View(df)
    
    output$map <- renderLeaflet({
        df$LATITUDE <- as.numeric(df$LATITUDE)
        df$LONGITUDE <- as.numeric(df$LONGITUDE)
        
        leaflet(df) %>% 
            addCircleMarkers(popup = ~htmlEscape(paste0(INSTNM, ', ', CITY)), stroke = FALSE, fillOpacity = 1, radius = 4) %>% 
            addTiles(urlTemplate = url, attribution = 'Maps provided by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
            setView(lng = -93.85, lat = 37.45, zoom = 4)
        })
    })

    output$list <- renderPrint({})
