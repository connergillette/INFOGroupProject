library(shiny)
library(leaflet)
library(dplyr)


data.set <- read.csv('MERGED2014_15_PP.csv', stringsAsFactors=FALSE)
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
max.pop <- select(data.set,UGDS) %>% summarise(max = max(UGDS))
for(abbr in state.abbr.list){
  if(abbr %in% names(commonwealth.territories)){
    names <- commonwealth.territories[abbr]
  }else{
    names <- ( state.name[grep(abbr,state.abb)] )
  }
  state.name.list <- append(state.name.list,names)
}
state.name.list <- unlist(state.name.list, use.names=FALSE)
state.name.list <- c("United States", state.name.list)

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
                  c("Agriculture" = "PCIP01",
                    "Resources" = "PCIP03",
                    "Architecture" = "PCIP04",
                    "Ethnic Studies" = "PCIP05",
                    "Communication" = "PCIP09",
                    "Communications Technologies" = "PCIP10",
                    "Computer" = "PCIP11",
                    "PersonalCulinary" = "PCIP12",
                    "Education" = "PCIP13",
                    "Engineering" = "PCIP14",
                    "Engineering Technology" = "PCIP15",
                    "Linguistics" = "PCIP16",
                    "Human Sciences" = "PCIP19",
                    "Legal Studies" = "PCIP22",
                    "English" = "PCIP23",
                    "Humanities" = "PCIP24",
                    "Library Science" = "PCIP25",
                    "Biomedical Science" = "PCIP26",
                    "Mathematics" = "PCIP27",
                    "Military Science" = "PCIP29",
                    "Interdisciplinary Studies" = "PCIP30",
                    "Fitness Studies" = "PCIP31",
                    "Philosophy" = "PCIP38",
                    "Theology" = "PCIP39",
                    "Physical Sciences" = "PCIP40",
                    "Science Technologies" = "PCIP41",
                    "Psychology" = "PCIP42",
                    "Protective Services" = "PCIP43",
                    "Public Administration" = "PCIP44",
                    "Social Sciences" = "PCIP45",
                    "Construction Trades" = "PCIP46",
                    "Repair Technologies" = "PCIP47",
                    "Precision Production" = "PCIP48",
                    "Transportation" = "PCIP49",
                    "Visual And Performing Arts" = "PCIP50",
                    "Health Professions" = "PCIP51",
                    "Business" = "PCIP52",
                    "History" = "PCIP54")),
      sliderInput("sat.math",label=h3("SAT Math Scores"),
                  min=0,max=800,value=c(400,700)),
      sliderInput("sat.writing",label=h3("SAT Writing Scores"),
                  min=0,max=800,value=c(400,700)),
      sliderInput("sat.reading",label=h3("SAT Reading Scores"),
                  min=0,max=800,value=c(400,700)),
      sliderInput("size",label=h3("Undergraduate Population Size"),
                  min=0,max=100000,value=c(30000,50000)),
      radioButtons("type_tuition",label=h3("Type of Tuition"),
                   choices=list("No Filter" = 1, "In-State"=2, "Out-of-State"=3),
                   selected=1),
      sliderInput("in_tuition",label=h3("Tuition In-State"),
                  min=0,max=40000,value=c(5000,20000)),
      sliderInput("out_tuition",label=h3("Tuition Out-of-State"),
                  min=0,max=40000,value=c(5000,20000))
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
                  tabPanel('Data Table', h2('Data Table for Currently Filtered Data'), tableOutput('full_df'))
      )
    )
  )
)
)
