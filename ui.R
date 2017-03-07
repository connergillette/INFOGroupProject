library(shiny)

shinyUI(fluidPage(
    titlePanel('College Finder'),
    p('Descriptive sentence or something'),
    
    sidebarLayout(
        sidebarPanel(
            h2('Sidebar')
            ), 
        mainPanel(
            h2('Map'),
            leafletOutput('map')
            )
        )
    )
)