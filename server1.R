library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# For map visualization
library(sp)
library(leaflet)
library(htmltools)

url <- 'https://api.mapbox.com/styles/v1/connergillette/cizyy5a9m004m2smjhu438php/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiY29ubmVyZ2lsbGV0dGUiLCJhIjoiY2l6cWIwOWJuMDE3azMzcDdvdmx3eWMwMyJ9.Qa235PPv1uPZtvaWIUSpQA'

df <- as.data.frame(read.csv('MERGED2014_15_PP.csv', stringsAsFactors = FALSE)) %>% select(INSTNM, CITY, STABBR, ZIP, LATITUDE, LONGITUDE, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, starts_with('PCIP'), UGDS, TUITIONFEE_IN, TUITIONFEE_OUT
)

shinyServer(function(input, output) {
<<<<<<< HEAD:server1.R
   
=======
    url <- 'https://api.mapbox.com/styles/v1/connergillette/cizyy5a9m004m2smjhu438php/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiY29ubmVyZ2lsbGV0dGUiLCJhIjoiY2l6cWIwOWJuMDE3azMzcDdvdmx3eWMwMyJ9.Qa235PPv1uPZtvaWIUSpQA'
    
    df <- as.data.frame(read.csv('MERGED2014_15_PP.csv', stringsAsFactors = FALSE)) %>% select(INSTNM, CITY, STABBR, ZIP, LATITUDE, LONGITUDE, INSTURL, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, starts_with('PCIP'), UGDS, TUITIONFEE_IN, TUITIONFEE_OUT
    )
>>>>>>> e8b3132b15797803818aab6adcf278103ab7acff:server.R
    #df <- head(df, 100)
    #View(df)
    
    filtered.list <- reactive({
      data <- filter(df, )
    })
    
    output$map <- renderLeaflet({
        df$LATITUDE <- as.numeric(df$LATITUDE)
        df$LONGITUDE <- as.numeric(df$LONGITUDE)
        
        popup.content <- paste(sep = '<br />', paste0('<a target="_blank" href="http://', df$INSTURL, '">', 
                            df$INSTNM, '</a>'), paste0('<b>Location: </b>', df$CITY, ', ', df$STABBR), ifelse(df$ADM_RATE != 'NULL', 
                            paste0('<b>Admission Rate: </b>', as.numeric(df$ADM_RATE) * 100, '%'), '<b>Admission Rate:</b> Not given'))
        
        leaflet(df) %>% 
<<<<<<< HEAD:server1.R
            addCircleMarkers(popup = ~htmlEscape(paste0(INSTNM, ', ', CITY)), stroke = FALSE, fillOpacity = 1, radius = 4) %>% 
            addTiles(urlTemplate = url, attribution = 'Maps provided by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
            setView(lng = -104.82, lat = 47.55, zoom = 3)
        })
    })


    output$colleges <- render({
      
=======
            addCircleMarkers(popup = popup.content, stroke = FALSE, fillOpacity = 1, radius = 4) %>%
                        addTiles(urlTemplate = url, attribution = 'Maps provided by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
            setView(lng = -93.85, lat = 37.45, zoom = 4)
        })
    
    output$list <- renderPrint({})
>>>>>>> e8b3132b15797803818aab6adcf278103ab7acff:server.R
    })

