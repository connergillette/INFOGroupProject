library(shiny)
library(leaflet)
library(dplyr)

# Reads data from a csv file
data.set <- read.csv('MERGED2014_15_PP.csv', stringsAsFactors=FALSE)

# Selects the usable coloumns for data manipulation
#data.set <- select(data.set.all, LATITUDE, LONGITUDE, STABBR, CITY, INSTNM, ADM_RATE,
#                   SAT_AVG_ALL, UGDS, TUITIONFEE_IN, TUITIONFEE_OUT, PCIP04, PCIP09, PCIP11,
#                   PCIP13, PCIP14, PCIP15, PCIP16, PCIP22, PCIP23, PCIP24, PCIP26, PCIP27,
#                   PCIP38, PCIP40, PCIP41, PCIP42, PCIP44, PCIP45, PCIP46, PCIP51, PCIP52, PCIP54)

state.abbr.list <- as.vector(unique(select(data.set,STABBR))[,1])
state.name.list <- c()
commonwealth.territories <- list("AS" = "American Samoa",
                                 "DC" = "District of Columbia",
                                 "FM" = "Federated States of Micronesia",
                                 "GU" = "Guam",
                                 "MH" = "Marshall Islands",
                                 "MP" = "Northern Mariana Islands",
                                 "PW" = "Palau",
                                 "PR" = "Puerto Rico",
                                "VI" = "Virgin Islands")
max.pop <- select(df, UGDS) %>% filter(UGDS != "NULL") %>% summarise("max" = max(UGDS))
for(abbr in state.abbr.list){
  if(abbr %in% names(commonwealth.territories)){
    names <- commonwealth.territories[abbr]
  }else{
    names <- ( state.name[grep(abbr,state.abb)] )
  }
  state.name.list <- append(state.name.list,names)
}
state.name.list <- unlist(state.name.list, use.names=FALSE)
major.list <- c("Computer Science", "Informatics" )


shinyUI(fluidPage(
  titlePanel('College Finder'),
  p('Descriptive sentence or something'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", label=h3("State"),
                  choices=list('States'= state.name.list)),
      sliderInput("admission",label=h3("Admission Rate"),
                  min=0,max=100,value=c(40,60)),
      selectInput("major", label=h3("Major"),
                  choices=list('Major'=major.list)),
      sliderInput("sat.math",label=h3("SAT Math Score"),
                  min=0,max=800,value=c(400,700)),
      sliderInput("sat.reading",label=h3("SAT Reading Scores"),
                  min=0,max=800,value=c(400,700)),
      sliderInput("sat.writing",label=h3("SAT Writing Scores"),
                  min=0,max=800,value=c(400,700)),
      sliderInput("size",label=h3("Undergraduate Population Size"),
                  min=0,max= as.numeric(max.pop$max),value=c(30000,50000)),
      sliderInput("tuition",label=h3("Tuition"),
                  min=0,max=30000,value=10000)
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
