r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
options(shiny.sanitize.errors = TRUE)
options(warn = FALSE)


# Library:
packages.used=c("tidyverse", "data.table","formattable","dplyr","rgdal","plyr","vistime",
                "leaflet","plotly","shiny","shinythemes","maps","shinyWidgets","rsconnect","ggmap")
# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

## Data Cleaning:
library(tidyverse)
library(data.table)
library(plotly)
library(formattable)
library(plyr)
library(dplyr)
library(rgdal)
library(vistime)

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

############################################################ 
# Read Data
############################################################ 
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

############################################################
# Define levels
############################################################
# Route Map & Fare:
level_Depart <- levels(Airfare$city1)
level_Arrive <- levels(Airfare$city2)

# Route Map:
level_Depart.map <- levels(Airfare$city1)
level_Arrive.map <- levels(Airfare$city2)

# Delay:
level_Depart.delay <- sort(unique(aot.delay$ORIGIN_CITY_NAME))
level_Arrive.delay <- sort(unique(aot.delay$DEST_CITY_NAME))

############################################################
# Shiny Server
############################################################

shinyServer(function(input, output,session) {
  ######################## 
  # Map:
  ########################
  output$arrive.map <- renderUI({
    if (input$Depart.map == "All"){
      level_Arrive_new.map <- level_Arrive.map
    }else{
      aot_Original_city <- subset(Airfare, city1 == input$Depart.map)
      level_Arrive_new.map <- sort(level_Arrive.map[unique(aot_Original_city$city2)])
    }
    selectInput("Arrive.map", label = h5("Arriving At"),
                choices = c("All" = "All", level_Arrive_new.map)
    )
  })
  
  # Map:
  output$air_map <- renderLeaflet({
    ###
    sub_data <- subset(Airfare, select = c(city1,city2,long_city1,lati_city1,long_city2,lati_city2))
    if (input$Depart.map != "All"){
      sub_data <- subset(sub_data, city1 == input$Depart.map)
    }
    if (input$Arrive.map != "All"){
      sub_data <- subset(sub_data, city2 == input$Arrive.map)
    }
    
    ###
    map <- leaflet(sub_data) %>% setView(-98.35 , 39.48, zoom = 4) %>% 
      addProviderTiles(providers$Esri.WorldStreetMap)
    
    ###
    icons <- awesomeIcons(
      icon = 'ion-plane',
      iconColor = 'white',
      library = 'ion',
      markerColor = "blue")
    
    #add marker for dest in the map
    map <- map %>% addAwesomeMarkers(lng = ~lati_city2, lat = ~long_city2, icon=icons, layerId = ~city2) 
    #load lon and lat 
    flows <- gcIntermediate(select(sub_data, lati_city1, long_city1), 
                            select(sub_data, lati_city2, long_city2), 
                            n=200 , sp = TRUE, addStartEnd = TRUE) 
    #add polylines connecting origin and destination 
    map <- map %>% 
      addPolylines(data = flows,color = "grey",weight = 2,
                   dashArray = "5, 5") #color= colors,fillOpacity = 1 weight = data$frequency/5) 
  })

  ##########################
  # #  map click
  ##########################
  observeEvent(input$air_map_marker_click,{
    temp_data <- subset(Airfare, city1 == input$Depart.map & city2 == input$air_map_marker_click$id, 
                        select = c(city1,city2,long_city1,lati_city1,long_city2,lati_city2))
    Depart_city = temp_data$city1[1]
    Depart_city_ll = c(temp_data$lati_city1[1], temp_data$long_city1[1])
    Arrive_city = temp_data$city2[2]
    Arrive_city_ll= c(temp_data$lati_city2[1], temp_data$long_city2[1])
    
    flows = gcIntermediate(Depart_city_ll, Arrive_city_ll , n=200 , sp = TRUE, addStartEnd = TRUE) 
    
    icons <- awesomeIcons(
      icon = 'ion-plane',
      iconColor = 'white',
      library = 'ion',
      markerColor = "blue")
    
    ArrContent <- paste(
      sep = "<br />",
      "From: ",
      Depart_city,
      "To: ",
      Arrive_city
    )
    
    leafletProxy("air_map", session) %>%
      clearShapes() %>% clearMarkers() %>% clearPopups() %>%
      addPolylines(data = flows, color = "black", weight = 5) %>%
      addAwesomeMarkers(lng = Depart_city_ll[1], lat = Depart_city_ll[2], icon = icons, 
                        layerId = Depart_city) %>%
      addAwesomeMarkers(lng = Arrive_city_ll[1], lat = Arrive_city_ll[2], icon = icons, 
                        layerId = Arrive_city, label = htmltools::HTML(ArrContent), labelOptions = labelOptions(noHide = TRUE))
    
  })
  
  observeEvent(input$air_map_click, {
    sub_data <- subset(Airfare, select = c(city1,city2,long_city1,lati_city1,long_city2,lati_city2))
    if (input$Depart.map != "All"){
      sub_data <- subset(sub_data, city1 == input$Depart.map)
    }
    if (input$Arrive.map != "All"){
      sub_data <- subset(sub_data, city2 == input$Arrive.map)
    }
    
    
    ###
    icons <- awesomeIcons(
      icon = 'ion-plane',
      iconColor = 'white',
      library = 'ion',
      markerColor = "blue")
    
    #load lon and lat 
    flows <- gcIntermediate(select(sub_data, lati_city1, long_city1), 
                            select(sub_data, lati_city2, long_city2), n=200 , sp = TRUE, addStartEnd = TRUE) 
    
    ###
    leafletProxy("air_map", session) %>%
      clearShapes() %>% clearMarkers() %>% clearPopups() %>%
      addPolylines(data = flows,color = "grey",weight = 2,
                   dashArray = "5, 5") %>%
      addAwesomeMarkers(lng = sub_data$lati_city2, lat = sub_data$long_city2, icon=icons, layerId = sub_data$city2)
  })
  
  # ######################## 
  # # mouse_hover:
  # ########################
  observeEvent(input$air_map_marker_mouseover$id,{
    markerID <- input$air_map_marker_mouseover$id
    marker_data <- subset(Airfare, city1 == input$Depart.map & city2 == input$air_map_marker_mouseover$id, 
                          select = c(long_city2,lati_city2))
    lati <- marker_data$long_city2[1]
    long <- marker_data$lati_city2[1]
    leafletProxy("air_map") %>%
      clearPopups() %>%
      addPopups(lat = lati+1, lng = long, as.character(markerID))
  })
  
  #observeEvent(input$air_map_marker_mouseout, {
  #leafletProxy("air_map") %>%
  #clearPopups()
  #})
  
  
  # ######################## 
  # # Fare:
  # ########################
  ## Panel 1: summary plots of time trends, 
  ##          unit price and full price of sales. 
  output$arrive <- renderUI({
    if (input$Depart == "All"){
      level_Arrive_new <- level_Arrive
    }else{
      aot_Original_city <- subset(Airfare, city1 == input$Depart)
      level_Arrive_new <- level_Arrive[unique(aot_Original_city$city2)]
    }
    selectInput("Arrive", label = h5("Arriving At"),
                choices = c("All" = "All", level_Arrive_new)
    )
  })
  
  # Line Chart:
  ## All to All level:
  output$distPlot.fare <- renderPlotly({
    #subset by given the Origin City and Destination City
    aot_Basic <- Airfare
    if(input$Depart != "All"){
      aot_Basic <- subset(aot_Basic, city1 == input$Depart)
    }
    if(input$Arrive != "All"){
      aot_Basic <- subset(aot_Basic, city2 == input$Arrive)
    }

    ## City to CITY level:
    air.avg.fare <- aot_Basic %>% group_by(yq) %>%
      dplyr::summarize(mean_fare = mean(fare),mean_lg = mean(fare_lg),mean_low = mean(fare_low),na.rm=TRUE)
    base <- air.avg.fare %>%
      plot_ly(x=~yq,y=~mean_fare, type = 'scatter', mode = 'lines+markers',
              line=list(width=3),name="Airlines Average",
              marker=list(size=4)) %>%
      add_trace(y=~mean_lg,name = 'Dominant Carrier',
                line=list(width=1,dash='dot'),
                marker=list(size=4)) %>%
      add_trace(y=~mean_low,name = 'Cheapest Carrier',
                line=list(width=1,dash='dot'),
                marker=list(size=4)) %>%
      layout(xaxis=list(title="Time"),yaxis=list(title="Fares"))
  })

  # Largest Carrier:
  output$LgCarrier <- renderPlotly({
    aot_Basic <- carrierLg
    if(input$Depart == "All" || input$Arrive == "All"){
      return()
    }
    aot_Basic <- subset(aot_Basic, city1 == input$Depart)
    aot_Basic <- subset(aot_Basic, city2 == input$Arrive)
    data <- data.frame(
      Name = aot_Basic$carrier_lg,
      start = aot_Basic$start,
      end = aot_Basic$end
    )
    vistime(data, groups="Name", events="Name", title="Dominant Carrier")
  })

  # Lowest cost carrier:
  output$LowCarrier <- renderPlotly({
    aot_Basic <- carrierLow
    if(input$Depart == "All" || input$Arrive == "All"){
      return()
    }
    aot_Basic <- subset(aot_Basic, city1 == input$Depart)
    aot_Basic <- subset(aot_Basic, city2 == input$Arrive)
    data <- data.frame(
      Name = aot_Basic$carrier_low,
      start = aot_Basic$start,
      end = aot_Basic$end
    )
    vistime(data, groups="Name", events="Name", title="Cheapest Carrier")
  })

  ########################
  # On-time Performance:
  ########################

  # Delay Control panel:
  output$arrive_delay <- renderUI({
    if (input$Depart.delay == "All"){
      level_Arrive_new.delay <- level_Arrive.delay
    }else{
      aot_Original_city <- subset(aot.delay, ORIGIN_CITY_NAME == input$Depart.delay)
      level_Arrive_new.delay <- sort(unique(aot_Original_city$DEST_CITY_NAME))
    }
    selectInput("Arrive.delay", label = h5("Arriving At"),
                choices = c("All" = "All", level_Arrive_new.delay)
    )
  })

  # Bubble Plot: Delay on Carriers
  output$bubblePlot <- renderPlotly({
    #subset by given the Origin City and Destination City
    aot_Basic.delay <- aot.delay
    if(input$Depart.delay != "All"){
      aot_Basic.delay  <- subset(aot_Basic.delay , ORIGIN_CITY_NAME == input$Depart.delay)
    }
    if(input$Arrive.delay != "All"){
      aot_Basic.delay  <- subset(aot_Basic.delay, DEST_CITY_NAME == input$Arrive.delay)
    }


    aot_carr <- aot_Basic.delay %>% group_by(OP_UNIQUE_CARRIER, MONTH) %>%
      dplyr::summarise(canc_perc=mean(CANCELLED),
                       delay_perc=mean(ARR_DELAY>15, na.rm=TRUE),
                       delay_time=mean(ARR_DELAY[ARR_DELAY>15], na.rm=TRUE)) %>%
      left_join(
        aot_Basic.delay %>% filter(ARR_DELAY>15) %>%
          group_by(OP_UNIQUE_CARRIER, MONTH) %>%
          dplyr::summarise(carrier_delay=mean(CARRIER_DELAY, na.rm=TRUE)),
        by=c('OP_UNIQUE_CARRIER','MONTH')) %>%
     na.omit()

    carr_info <- aot_carr %>%
      plot_ly(x=~1-canc_perc, y=~1-delay_perc, frame = ~MONTH,
              type = 'scatter', mode = 'markers',
              size=~delay_time, sizes=c(10,35),
              color = ~OP_UNIQUE_CARRIER, colors = 'Paired',
              marker=list(
                sizemode='diameter',
                opacity = 0.5
              ),
              hoverinfo = 'text',
              text = ~paste('Carrier:', OP_UNIQUE_CARRIER,
                            '<br>Non-Cancelled Rate:',1- percent(canc_perc),
                            '<br>Non-Delayed Rate:', 1-percent(delay_perc),
                            '<br>Average Delay Time:', signif(delay_time,3))) %>%
      layout(title='Airlines On-Time Performance',
             xaxis= list(title='Non-Cancelled Rate'),
             yaxis= list(title='Non-Delayed Rate')) %>%
      hide_legend()

    df_delay <- aot_carr %>% gather('delay_time', 'carrier_delay', key='delay_type', value='delay_time')
    df_delay$delay_type <- revalue(df_delay$delay_type,
                                   c('delay_time'='Total Delay', 'carrier_delay'='Carrier Delay'))
    carr_dely <- df_delay %>%
      plot_ly(x=~delay_time, y=~ OP_UNIQUE_CARRIER, frame=~MONTH, type='scatter',
              mode="markers", color=~ delay_type,
              symbol=~delay_type,
              marker = list(size = 9),
              hoverinfo='text',
              text=~paste('Type:', delay_type,
                          '<br>Carrier:', OP_UNIQUE_CARRIER,
                          '<br>Average Time:', signif(delay_time,3)))%>%
      layout(title = 'Air Carrier Delay',
             xaxis = list(title = 'Mean Delay Time'),
             yaxis = list(title = 'Carrier',
                          tickfont = list(
                            size = 10
                          )),
             margin = list(l = 100)) %>%
      hide_legend()

      plot <- subplot(carr_dely, carr_info, nrows = 1, widths = c(0.3, 0.7),
                      titleX = TRUE, titleY = TRUE, margin = 0.06) %>%
      animation_opts(1000, redraw = TRUE) %>%
      layout(margin = list(l = 100))


  })

  # Map: Depay on Airports & Reasons
  output$map.delay <- renderPlotly(map.delay.plot)
  
  # Accident Statistics
  output$accident.year <- renderPlotly({
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    
    plotdata <- airport.data %>%
      mutate(full.name = state.name[match(airport.data$Event.State,state.abb)]) %>%
      filter(year == input$year) 
    
    plotdata$hover <- with(plotdata, paste(full.name, '<br>', 
                                           "Number of Accident", count, "<br>","Year",year))
    
    plot_ly(z = plotdata$count, locations = plotdata$Event.State,text = plotdata$hover,
            type = 'choropleth', locationmode = 'USA-states') %>%
      colorbar(title = "Number of Accident") %>%
      layout(
        title = 'Accident Count In Each State<br>(Hover for details)',
        geo = g
      )
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a state to view accident number happened in it" else d
  })
  
  output$accident.month <- renderPlotly(
    month.data %>%
      plot_ly(
        x = ~month, 
        y = ~count, 
        frame = ~year, 
        text = ~count, 
        hoverinfo = "text",
        type="scatter",
        mode = "marker",
        name = "numebr of accident"
      ) %>% 
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
      )
  )
  
  output$accident.operator <- renderPlotly(
    plot_ly(operator, x = ~year, y = ~count) %>%
      add_lines(color = ~Operator)
  )
  
  output$aircraft <- renderPlotly(
    plot_ly(aircraft.make, x = ~year, y = ~count) %>%
      add_lines(color = ~Aircraft.Make)
  )
  
  output$reason <- renderPlotly(accident.reason %>%
                                  ggplot(aes(Aircraft.Damage)) +
                                  geom_bar(aes(fill = Primary.Flight.Type)) +
                                  labs(x = "",  y="") +
                                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                                  coord_flip())
  
  ###################################################
  # Customer Satisfication:
  ###################################################
  #plot for combined statistics
  output$combineplot <- renderPlot({
    if(length(input$carrier == 0)){
      plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    }
    if(length(input$carrier > 0)){
      # generate an rnorm distribution and plot it
      carr_name <- input$carrier
      carr_row <- data_customer$airline %in% carr_name
      carr_data <- data_customer[carr_row, c(2,3,4,5,6)]
      ggparcoord(carr_data, columns = 2:5, groupColumn = 1)+geom_line(size=2)+theme_bw()+theme(panel.border = element_blank())
    }
  })
  
  #plots for seperate statistics
  output$luggageplot=renderPlot({
    ggplot(data_customer)+
      layer(mapping = aes(x = fct_reorder(airline,`missing.luggage.per.1000.passangers`), y =`missing.luggage.per.1000.passangers`),
            params = list(fill = "pink"),
            geom = "bar",stat = "identity", position = "identity")+
      theme(axis.text.x = element_text(size = 8,hjust=1,vjust = 1, angle = 20),panel.border = element_blank())+
      labs(x='airline')
  })
  
  output$complaintplot=renderPlot({
    ggplot(data_customer)+
      layer(mapping = aes(x = fct_reorder(airline,`customer.complaint`), y =`customer.complaint`),
            params = list(fill = "lightblue"),
            geom = "bar",stat = "identity", position = "identity")+
      theme(axis.text.x = element_text(size = 8,hjust=1,vjust = 1, angle = 20),panel.border = element_blank())+
      labs(x='airline')
    
  })
  
  output$voluntaryplot=renderPlot({
    ggplot(data_customer,aes(x=airline,y=All.permillage))+
      geom_point(color="lightgreen",size=5)+
      theme(axis.text.x = element_text(size = 8,hjust=1,vjust = 1, angle = 45),panel.border = element_blank())+
      labs(x='airline')
  })
  
  output$involuntaryplot=renderPlot({
    ggplot(data_customer,aes(x=airline,y=Involuntary.permillage))+
      geom_point(color='grey',size=5)+
      theme(axis.text.x = element_text(size = 8,hjust=1,vjust = 1, angle = 45),panel.border = element_blank())+
      labs(x='airline')
  })
  
})

