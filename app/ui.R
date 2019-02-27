## Data Cleaning:
library(tidyverse)
library(data.table)
library(plotly)
library(formattable)
library(dplyr)
library(rgdal)
library(plyr)


## Visualization:
library(leaflet)
library(shiny)
library(shinythemes)
library(maps)
library(shinyWidgets)
library(DT)
library(ggmap)
library("base64enc")
library("ggplot2")
library("reshape2")
library("geosphere")
library(GGally)
#library("parcoords")
library("stringr")
library(htmltools)
library(vistime)
#devtools::install_github("timelyportfolio/parcoords")


#####################################################################
# Read Data:
#####################################################################
############################################################ 
# Read Data
############################################################ 
## Map:
# Map:
Air_map <- readRDS("../output/Airline_map.RDS")
Air_map <- na.omit(Air_map)
Air_map$city1 <- as.factor(Air_map$city1)
Air_map$city2 <- as.factor(Air_map$city2)

## Fares:
carrierLg <- data.frame(read.csv('../data/carrier_lg.csv', header = TRUE))
carrierLow <- data.frame(read.csv('../data/carrier_low.csv', header = TRUE))
Airfare <- readRDS("../output/Airfare_2008.RDS")

## Delay
aot.delay <- readRDS("../output/airline_on_time_2018.RDS")
map.delay.plot <-readRDS("../output/Airport_delay_status.RDS")

## Accident Data
airport.data <- read.csv("../output/accident_state.csv")
month.data <- read.csv("../output/accident_month.csv")
aircraft.make <- read.csv("../output/accident_aircraft.csv")
operator <- read.csv("../output/accident_operator.csv")
accident.reason <- read.csv("../output/accident_reason.csv")
state.names <- unique(airport.data$Event.State)

#Customer Data
data_customer <- read.csv("../output/combind_data.csv")

#Summary
arln_summ <-readRDS('../output/Airline_summary.RDS')

#####################################################################
# Define Levels:
#####################################################################

# Route Map & Fare:
level_Depart <- levels(Airfare$city1)
level_Arrive <- levels(Airfare$city2)

# Route Map:
level_Depart.map <- levels(unique(Air_map$city1))
level_Arrive.map <- levels(unique(Air_map$city2))

# Delay:
level_Depart.delay <- sort(unique(aot.delay$ORIGIN_CITY_NAME))
level_Arrive.delay <- sort(unique(aot.delay$DEST_CITY_NAME))

# Summary:
orig <- arln_summ %>% select(ORIGIN_CITY_NAME) %>% unique() 
dest <- arln_summ %>% select(DEST_CITY_NAME) %>% unique() 

#####################################################################
# Define Tabs:
#####################################################################

# Map Tab
tab.map <- tabPanel("Route Map", icon=icon("map-o"),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("Depart.map", label = h5("Departure From"), 
                                        choices = c("New York, NY"=level_Depart.map[222],
                                                    level_Depart.map[which(level_Depart.map!="New York, NY")]),
                                        selected = 0)
                            ,
                            uiOutput("arrive.map")
                          ),
                          # Show two panels
                          mainPanel(
                            leafletOutput("air_map", width="100%", height = "500px"),
                            br(),
                            includeMarkdown('intro.md')
                          )))


# Fares Tab
tab1<- tabPanel("Fares",
                icon=icon("money-bill"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("Depart", label = h5("City 1"),
                                choices = c("All"="All",level_Depart),
                                selected = 0),
                    width = 3,
                    uiOutput("arrive")
                  ),
                  
                  # Show the panel
                  mainPanel(
                    
                    plotlyOutput("distPlot.fare"),
                    br(),
                    br(),
                    plotlyOutput("LgCarrier"),
                    br(),
                    plotlyOutput("LowCarrier"))),
                tags$a(href = "https://www.bts.gov/air-fares","2018 Source:https://www.bts.gov/air-fares"))

## On-time Performance Tab

tab2<- navbarMenu("On-Time Performance",
                  icon=icon("clock"),
                  tabPanel("On-Time Summary on Airlines",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("Depart.delay", label = h5("Departure From"),
                                           choices = c("All" = "All",level_Depart.delay),
                                           selected = 0),
                               width = 3,
                               uiOutput("arrive_delay")
                             ),
                             # Show One Panel
                             mainPanel(
                               plotlyOutput("bubblePlot"))
                           ),
                           tags$a(href = "https://transtats.bts.gov/ONTIME/","2018 Source:https://transtats.bts.gov/ONTIME/"),
                           style = "overflow-y:scroll; height: 600px; opacity: 0.9; background-color: #ffffff;")
                  ,
                  
                  tabPanel("Cause of Delay Based on Airports",
                           mainPanel(
                             plotlyOutput("map.delay",width = "150%",height = "600px"),
                             tags$a(href = "https://transtats.bts.gov/ONTIME/","2018 Source:https://transtats.bts.gov/ONTIME/")
                           )
                  ))

# Accident tab
accident.tab <-   navbarMenu("Accident",
                             icon = icon("exclamation-triangle"),
                             tabPanel("State & Year",
                                      sidebarLayout(
                                        sidebarPanel(
                                          sliderInput("year",
                                                      "Different year:",
                                                      min = 1978,
                                                      max = 2018,
                                                      value = 2008),
                                          width = 12
                                        ),
                                        
                                        mainPanel(
                                          plotlyOutput("accident.year",height="300px"
                                          ),
                                          width = 12,
                                          verbatimTextOutput("click")
                                        )
                                      ),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      plotlyOutput("accident.month")
                                      
                             ),
                             
                             tabPanel("Operator & Aircraft",
                                      plotlyOutput("accident.operator"),
                                      br(),
                                      plotlyOutput("aircraft")),
                             
                             tabPanel("Flight Type & Damage",
                                      plotlyOutput("reason"))
)

## Customer Satisfication Tab

tab3 <- navbarMenu("Customer Satisfaction",
                   icon = icon("grin"),
                   #tabpanel1
                   tabPanel(title = "Combined Statistics",
                            fluidRow( wellPanel(style = "overflow-y:scroll;  height: 500px; opacity: 0.9; background-color: #ffffff;",
                                                column(width = 9, plotOutput("combineplot"),
                                                       tags$a(href = "https://www.transtats.bts.gov/","2015-2017 Data,Source:https://www.transtats.bts.gov/")
                                                       
                                                ),
                                                column(width = 3, checkboxGroupInput("carrier", "Choose an Airline:",
                                                                                     choices = c('ALASKA AIRLINES','AMERICAN AIRLINES','DELTA AIR LINES','ENVOY AIR','EXPRESSJET AIRLINES','FRONTIER AIRLINES','HAWAIIAN AIRLINES','JETBLUE AIRWAYS','SKYWEST AIRLINES','SOUTHWEST AIRLINES','SPIRIT AIRLINES','UNITED AIRLINES'),
                                                                                     selected = "AMERICAN AIRLINES"))
                                                
                                                
                            ))),
                   
                   #tabpanel 2
                   tabPanel(title = "Seperate Statistics",
                            fluidRow( wellPanel(style = "overflow-y:scroll;  height: 500px; opacity: 0.9; background-color: #ffffff;",
                                                column(width = 12, 
                                                       plotOutput("luggageplot"),
                                                       plotOutput("complaintplot"),
                                                       plotOutput("voluntaryplot"),
                                                       plotOutput("involuntaryplot")
                                                       
                                                       
                                                ))))
                   
)

tab4 <- tabPanel(
  title = 'Summary',
  icon = icon('thumbs-up'),
  sidebarLayout(
    sidebarPanel(
      selectInput('Orig', label = h5('Origin City'),
                  choices = c('All'='All', orig$ORIGIN_CITY_NAME %>% sort()), 
                  selected = 0),
      uiOutput('dest'),
      selectInput('Con', label = h5('Most Concern'),
                  choices = c('Arrival Delay'='ARR_DELAY',
                              'Departure Delay'='DEP_DELAY',
                              'Missing Luggage',
                              'Customer Complaint',
                              'Involuntary Denied Boarding',
                              'All Denied Boarding'), 
                  selected = NULL,
                  multiple = TRUE),
      sliderInput('Wgt', label = h5('Concern Range'), min = 1., max = 5., 
                  value = 1., step = .01)
    ),
    mainPanel(
      plotlyOutput('summary')
    )
  )  
)

#####################################################################
# Finalization:
#####################################################################
ui <- fluidPage(theme= "bootstrap.min-copy.css",
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                # setBackgroundImage(src = "https://cdn.dribbble.com/users/150724/screenshots/1860497/126_plane.png"),
                navbarPage(title = strong("AirPlan2.0"),
                           tab.map,
                           tab1,
                           tab2,
                           accident.tab,
                           tab3,
                           tab4))


