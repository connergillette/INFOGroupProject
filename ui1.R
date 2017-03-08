library(shiny)
library(leaflet)

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
                                 selectInput('colleges', label = "Your Colleges:", 
                                             choices = c("No colleges"=""), 
                                             multiple=TRUE)
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
