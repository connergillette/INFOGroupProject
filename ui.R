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
            #@Thejas - None of this worked for me
            #tabPanel(type = "tabs", 
                    # tabPanel('map', h2('Map'), leafletOutput('map')),
                     # tabPanel('list', fluidRow(h2('List of Your Colleges'), 
                      #                          dataTableOutput("table")),
                      #                  fluidRow(h2('Summary statistics'), 
                      #                           verbatimTextOutput("summary"))
                      #),
                     #tabPanel('all', h2('List of All Colleges'))
            #)
            leafletOutput('map')
        )
    )
  )
)