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
library(rsconnect)
library(ggmap)
library("base64enc")
library("ggplot2")
library("reshape2")
library("geosphere")


#####################################################################
# Read Data:
#####################################################################
# Fares:
carrierLg <- data.frame(read.csv('../data/carrier_lg.csv', header = TRUE))
carrierLow <- data.frame(read.csv('../data/carrier_low.csv', header = TRUE))
Airfare <- readRDS("../output/Airfare_2008.RDS")
# 
# Delay
aot.delay <- readRDS("../output/airline_on_time_2018.RDS")
map.delay.plot <-readRDS("../output/Airport_delay_status.RDS")


#####################################################################
# Define Levels:
#####################################################################

# Route Map & Fare:
level_Depart <- levels(Airfare$city1)
level_Arrive <- levels(Airfare$city2)

# Route Map:
level_Depart.map <- levels(Airfare$city1)
level_Arrive.map <- levels(Airfare$city2)

# Delay:
level_Depart.delay <- sort(unique(aot.delay$ORIGIN_CITY_NAME))
level_Arrive.delay <- sort(unique(aot.delay$DEST_CITY_NAME))


#####################################################################
# Define Tabs:
#####################################################################

# Map Tab
tab.map <- tabPanel("Route Map",  
  sidebarLayout(
    sidebarPanel(
      selectInput("Depart.map", label = h5("Choose a departure city"), 
                  choices = level_Depart, 
                  selected = 0)
      ,
      uiOutput("arrive.map")
    ),
    mainPanel(
      leafletOutput("air_map", width="100%", height = "500px")
    )))

# Fares Tab
tab1<- tabPanel("Historical Average Fares",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("Depart", label = h5("Departure From"),
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
                    plotlyOutput("LowCarrier")
                  )
                ))


tab2<- navbarMenu("On-Time Performance",
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
                            style = "overflow-y:scroll; height: 600px; opacity: 0.9; background-color: #ffffff;")
                   ,

                   tabPanel("Cause of Delay Based on Airports",
                            mainPanel(
                              plotlyOutput("map.delay",width = "150%",height = "600px")),
                            tags$a(href = "https://www.transtats.bts.gov/","2000-2017 Data, Source:https://www.transtats.bts.gov/"))
 )



#####################################################################
# Finalization:
#####################################################################
ui <- fluidPage(theme=shinytheme("flatly"),
                # setBackgroundImage(src = "https://cdn.dribbble.com/users/150724/screenshots/1860497/126_plane.png"),
                navbarPage(title = strong("AirPlan2.0"),
                           tab1,
                           tab2,
                           tab.map))


