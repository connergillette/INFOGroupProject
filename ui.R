library(shiny)

shinyUI(fluidPage(
    titlePanel('College Finder'),
    p('Descriptive sentence or something'),
    
    sidebarLayout(
        sidebarPanel(
            h2('Sidebar')
            ), 
        mainPanel(
            
            tabpanel(type = "tabs", 
                     tabpanel('map', h2('Map'), leafletOutput('map')),
                      tabpanel('list', fluidRow(h2('List of Your Colleges'), 
                                                dataTableOutput("table")),
                                        fluidRow(h2('Summary statistics'), 
                                                 VerbatimTextOutput("summary"))
                      ),
                     tabpanel('all', h2('List of All Colleges'))
            )
        )
    )
  )
)