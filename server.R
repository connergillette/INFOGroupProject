library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(htmlTable)

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
  
  filtered.data <- reactive({
    full.data.set <- read.csv('filtered_college_data.csv',stringsAsFactors = FALSE)
    full.data.set$TUITIONFEE_IN <- as.numeric(full.data.set$TUITIONFEE_IN)
    full.data.set$TUITIONFEE_OUT <- as.numeric(full.data.set$TUITIONFEE_OUT)
    full.data.set$ADM_RATE <- as.numeric(full.data.set$ADM_RATE)
    full.data.set$UGDS <- as.numeric(full.data.set$UGDS)
    full.data.set$SATMTMID <- as.numeric(full.data.set$SATMTMID)
    full.data.set$SATVRMID <- as.numeric(full.data.set$SATVRMID)
    full.data.set$SATWRMID <- as.numeric(full.data.set$SATWRMID)
    
    # Filters for States, if it is the united states it doesn't filter anything.
    if(input$state != "United States"){
      if(input$state %in% state.name){
        full.data.set <- filter(full.data.set, STABBR == state.abb[grep(input$state,state.name)])
      }else{
        full.data.set <- filter(full.data.set, STABBR == commonwealth.territories[input$state])
      }
    }
    # Filter for Admission Rate
    min.range.adm <- input$admission[1]/100
    max.range.adm <- input$admission[2]/100
    full.data.set <- filter(full.data.set, ADM_RATE >= min.range.adm & ADM_RATE <= max.range.adm)
    # Filter for Majors
    if(input$major != "None"){
      full.data.set <- select_(full.data.set, "INSTNM", "CITY", "STABBR","ZIP","LATITUDE", 
                              "LONGITUDE","INSTURL","ADM_RATE", "SATVRMID", "SATMTMID","SATWRMID",
                              input$major, "UGDS", "TUITIONFEE_IN", "TUITIONFEE_OUT") %>% 
                       filter_(.dots=paste0(input$major,"!= ","0"))
    }

    # Filter for SAT Math Score
    min.sat.math.size <- input$sat.math[1]
    max.sat.math.size <- input$sat.math[2]
    full.data.set <- filter(full.data.set, SATMTMID >= min.sat.math.size & SATMTMID <= max.sat.math.size)
    
    # Filter for SAT Writing Score
    min.sat.writing.size <- input$sat.writing[1]
    max.sat.writing.size <- input$sat.writing[2]
    full.data.set <- filter(full.data.set, SATWRMID >= min.sat.writing.size & SATWRMID <= max.sat.writing.size)
    
    # Filter for SAT Math Score
    min.sat.reading.size <- input$sat.reading[1]
    max.sat.reading.size <- input$sat.reading[2]
    full.data.set <- filter(full.data.set, SATVRMID >= min.sat.reading.size & SATVRMID <= max.sat.reading.size)
    
    # Filter for Undergrad Pop Size
    min.range.size <- input$size[1]
    max.range.size <- input$size[2]
    full.data.set <- filter(full.data.set, UGDS >= min.range.size & UGDS <= max.range.size)
    
    # Filter for Tuition
    if(input$type_tuition == 2){
      full.data.set <- filter(full.data.set, TUITIONFEE_IN >= input$in_tuition[1] & TUITIONFEE_IN <= input$in_tuition[2])
    }else if(input$type_tuition == 3){
      full.data.set <- filter(full.data.set, TUITIONFEE_OUT >= input$out_tuition[1] & TUITIONFEE_OUT <= input$out_tuition[2])
    }
    
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
      addCircleMarkers(popup = popup.content, stroke = FALSE, fillOpacity = 1, radius = 4) %>% 
      addTiles(urlTemplate = url, attribution = 'Maps provided by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
      setView(lng = -104.82, lat = 47.55, zoom = 3)
  })
  
  list <- reactive({
    my.list <- filter(df, INSTNM == input$colleges1 | INSTNM == input$colleges2 |
                        INSTNM == input$colleges3 | INSTNM == input$colleges4 |
                        INSTNM == input$colleges5)
    return(my.list)
  })
  
  output$table <- renderDataTable({
    data <- list()
    data <- select(data, INSTNM, STABBR, CITY, ZIP, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, 
                   UGDS, TUITIONFEE_IN, TUITIONFEE_OUT, INSTURL)
    data <- rename(data, "Name" = INSTNM , "State" = STABBR , "City" = CITY , "Total Undergrads" = UGDS ,
                   "SAT Median Writing" = SATWRMID, "SAT Median Verbal" = SATVRMID, "SAT Median Math" = SATMTMID,
                   "Cost IN-STATE" = TUITIONFEE_IN , "Cost OUT-OF-STATE" = TUITIONFEE_OUT , "URL" = INSTURL )
    return(data)
  })
  
  output$full_df <- renderDataTable({
    data <- filtered.data()
    data <- select(data, INSTNM, STABBR, CITY, ZIP, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, 
                   UGDS, TUITIONFEE_IN, TUITIONFEE_OUT, INSTURL)
    data <- rename(data, "Name" = INSTNM , "State" = STABBR , "City" = CITY , "Total Undergrads" = UGDS ,
                   "SAT Median Writing" = SATWRMID, "SAT Median Verbal" = SATVRMID, "SAT Median Math" = SATMTMID,
                   "Cost IN-STATE" = TUITIONFEE_IN , "Cost OUT-OF-STATE" = TUITIONFEE_OUT , "URL" = INSTURL )
    return(data)
  })
  
  observe({
    click <- input$map_marker_click
    if(!is.null(click$lat) & !is.null(click$lng) ){
      df <- filtered.data()
      college.row <- filter(df,LATITUDE == click['lat'], LONGITUDE == click['lng'])
      output$clickinfo <- renderUI({
        HTML(paste(
               paste0("<strong>University & Location:</strong> ", college.row$INSTNM," (",college.row$CITY,", ",college.row$STABBR,")"),
               paste0("<strong>Undergrad Population:</strong> ",college.row$UGDS),
               paste0("<strong>SAT Writing Score (Median):</strong> ",college.row$SATWRMID),
               paste0("<strong>SAT Reading Score (Median):</strong> ",college.row$SATVRMID),
               paste0("<strong>SAT Math Score (Median):</strong> ",college.row$SATMTMID),
               paste0("<strong>In-State Tuition:</strong> ",college.row$TUITIONFEE_IN),
               paste0("<strong>Out-of-State Tuition:</strong> ",college.row$TUITIONFEE_OUT),
               paste0("<strong>University URL:</strong> ",college.row$INSTURL),
               sep='<br/>'))
      })
    }
  })
  
})

