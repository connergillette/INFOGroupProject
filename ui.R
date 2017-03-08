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
            leafletOutput('map')
            )
        )
    )
)