library(shiny)
library(leaflet)

df <- as.data.frame(read.csv('MERGED2014_15_PP.csv', stringsAsFactors = FALSE)) %>% select(INSTNM, CITY, STABBR, ZIP, LATITUDE, LONGITUDE, INSTURL, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, starts_with('PCIP'), UGDS, TUITIONFEE_IN, TUITIONFEE_OUT)
df <- filter(df, !is.na(LATITUDE)) %>% filter(!is.na(LONGITUDE))


is.data.frame(df)

shinyUI(fluidPage(
    titlePanel('College Finder'),
    p('Descriptive sentence or something'),
    
    sidebarLayout(
        sidebarPanel(
            h2('Sidebar')
            ), 
        mainPanel(
            tabsetPanel(type = "tabs", 
                     tabPanel('map', h2('Map'), leafletOutput('map')),
                      tabPanel('list',  h2('List of Your Colleges'), 
                               fluidRow(
                                 selectInput('colleges1', label = "College 1:", 
                                             choices = c(df$INSTNM)), 
                                 selectInput('colleges2', label = "College 2:", 
                                             choices = c(df$INSTNM)), 
                                 selectInput('colleges3', label = "College 3:", 
                                             choices = c(df$INSTNM)),
                                 selectInput('colleges4', label = "College 4:", 
                                             choices = c(df$INSTNM)), 
                                 selectInput('colleges5', label = "College 5:", 
                                             choices = c(df$INSTNM))
                                 ),
                               fluidRow(dataTableOutput("table")),
                               fluidRow(h2('Summary statistics'), 
                                        verbatimTextOutput("summary"))
                      ),
                     tabPanel('all', h2('List of All Colleges'))
            )
        )
    )
  )
)
