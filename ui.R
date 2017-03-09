library(shiny)
library(leaflet)
library(dplyr)
library(shinythemes)

data.set <- read.csv('filtered_college_data.csv', stringsAsFactors=FALSE)
data.set$TUITIONFEE_IN <- as.numeric(data.set$TUITIONFEE_IN)
data.set$TUITIONFEE_OUT <- as.numeric(data.set$TUITIONFEE_OUT)
data.set$ADM_RATE <- as.numeric(data.set$ADM_RATE)
data.set$UGDS <- as.numeric(data.set$UGDS)
data.set$SATMTMID <- as.numeric(data.set$SATMTMID)
data.set$SATVRMID <- as.numeric(data.set$SATVRMID)
data.set$SATWRMID <- as.numeric(data.set$SATWRMID)
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

shinyUI(
    fluidPage(theme = shinytheme('united'),
        
navbarPage("College Data Information",
 tabPanel("Home Page",
    titlePanel('College Finder Tool for Undergraduates'),
    p('This tool is designed to help potential and current undergraduate students find out helpful
      information about various colleges across the United States. Checkout the',
      strong('Information'),' tab at the top for more details!'),
    p('View the Instructions tab to get started on searching for colleges.'),
    sidebarLayout(
      sidebarPanel(
        selectInput("state", label=h3("State"),
                    choices=list('States'= state.name.list)),
        sliderInput("admission",label=h3("Admission Rate"),
                    min=0,max=100,value=c(0,100)),
        selectInput("major", label=h3("Major"),
                    c("No Filter" = "None",
                      "Agriculture" = "PCIP01",
                      "Resources" = "PCIP03",
                      "Architecture" = "PCIP04",
                      "Ethnic Studies" = "PCIP05",
                      "Communication" = "PCIP09",
                      "Communications Technologies" = "PCIP10",
                      "Computer" = "PCIP11",
                      "Personal & Culinary" = "PCIP12",
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
                    min=0,max=800,value=c(0,800)),
        sliderInput("sat.writing",label=h3("SAT Writing Scores"),
                    min=0,max=800,value=c(0,800)),
        sliderInput("sat.reading",label=h3("SAT Reading Scores"),
                    min=0,max=800,value=c(0,800)),
        sliderInput("size",label=h3("Undergraduate Population Size"),
                    min=0,max=max(data.set$UGDS, na.rm=TRUE),value=c(0,30000)),
        radioButtons("type_tuition",label=h3("Type of Tuition"),
                     choices=list("No Filter" = 1, "In-State"=2, "Out-of-State"=3),
                     selected=1),
        sliderInput("in_tuition",label=h3("Tuition In-State"),
                    min=0,max=max(data.set$TUITIONFEE_IN, na.rm=TRUE),value=c(0,10000)),
        sliderInput("out_tuition",label=h3("Tuition Out-of-State"),
                    min=0,max=max(data.set$TUITIONFEE_OUT,na.rm=TRUE),value=c(0,10000))
      ), 
      mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel('Map', h2('Map'), 
                             p("The following is information based on a set of colleges in the USA. To explore
                               more on specific colleges, access the Data Table tab."),
                             leafletOutput('map'),
                             htmlOutput('clickinfo')),
                    tabPanel('Compare',  h2('Compare Schools'), 
                             p("Enter schools you are interested in to compare them on various criteria."),
                            selectInput('colleges', 'Schools of Interest', choices = data.set$INSTNM, multiple = TRUE, width = "100%"),
                             fluidRow(dataTableOutput("table"))
                    ),
                    tabPanel('Results', 
                             h2('Showing schools matching your filters'),
                             p("The following is a fully comprehesive table to represent the
                               set of colleges being considered in the map."),
                              dataTableOutput('full_df')),
                    tabPanel('Instructions', h2('Instructions'),
                             p("Instructions on how to use the filter sidebar:",
                               h3("State"),
                               "This filter changes what state's colleges are displayed.",
                               h3("Admission Rate"),
                               "This filter selects the colleges that have an admission rate inside the selected range.",
                               h3("Major"),
                               "This filter selects only the colleges that have this major.",
                               h3("SAT Math/Writing/Reading Scores"),
                               "This filter selects the colleges that have an average SAT Math/Writing/Reading
                               score within the selected range.",
                               h3("Undergraduate Population Size"),
                               "This filter selects for colleges with an undergraduate population within the selected range.",
                               h3("Type of Tuition"),
                               "Identifies whether or not you want to filter for in state tuition, out of state tuition, or not 
                               at all.",
                               h3("Tuition In-State/Out-of-State"),
                               "Selects colleges with a tuition within the selected range."
                             ))
        )
      )
    )
  ),
 tabPanel("Information",
    titlePanel("Information about College Finding Application"),
        h3('Thesis'),
      p("Education is a cornerstone to the modern society. One of
      the steps involved in the process of education is college. Nowadays,
      a degree in a college is almost a necessity for many jobs. To succeed 
      in future career, students need to pick the right college that matches 
      their preferences to spend about 4 years of their life in. Hence, it is
      important to look at the nationally accumulated data on Colleges to 
      ultimately help students pick their preferred colleges.",br(),br(),"We will be 
      observing the data on such colleges and present it in a helpful way 
      for the users to understand. The source that we will be using comes 
      from the source Data.gov, the link to which has been provided below.
      The data is collected, managed and hosted by the U.S. General Services
      Administration, Technology Transformation Service.",br(),br(),"We aim to make use
      of specific subsets of the 2014-2015 data set. This data set will help 
      us relate the different requirements of different schools throughout 
      the nation and also provide us with information like top 75 percentile
      admit SAT score, acceptance rate, city of the respective university, 
      tuition fees, etc.
      "),
         h3('Target Audience'),
      p("The major audience that we hope to aim are the graduating high
      school seniors who will be looking at different colleges to apply to. 
      There dire need to compare the 75/25 percentile of each college to
      their own scores, tuition fee of each university and suitable city will
      be fulfilled by this data representation.",br(),br(),"For example, John Wick, an 
      average american high school senior who has bagged a decent score in 
      SAT, limited financial standing and now wants to apply in different 
      colleges. Since he cannot apply to all of the colleges (due to high 
      application fee), John has to finalize few colleges where he thinks 
      he can get in and where his other requirements are met. He turns to 
      the information provided by us and he can easily list down universities
      according to his needs.",br(),br(),"Another audience that we can target will be 
      the high school counselors who need to keep a track of each university,
      their requirements, tuition fee etc., so that they can be helpful 
      towards their students seeking guidance.",br(),br(),"For example, Josh Spencer, 
      a counselor at a public school, would need to consider the academic,
      financial and vocational situations of each of the student to suggest
      appropriate schools. Our presentation can assist people like Josh to
      consider preferable options for their students based on our easily 
      understandable data and their experience.
      "),
          h3('Reference'),
      p("US Department of Education: https://collegescorecard.ed.gov/data/documentation/")
    )
)))