library(tidyverse)
library(lubridate) # used to extract year from date
library(shiny)
library(plotly)

# Accidents
airport.data <- read.csv("../output/accident_state.csv")
month.data <- read.csv("../output/accident_month.csv")
aircraft.make <- read.csv("../output/accident_aircraft.csv")
operator <- read.csv("../output/accident_operator.csv")
accident.reason <- read.csv("../output/accident_reason.csv")
state.names <- unique(airport.data$Event.State)

accident.tab <-   navbarMenu("Accident",
                     icon = icon("exclamation-triangle"),
                     tabPanel("Year & Month",
                              sidebarLayout(
                                sidebarPanel(
                                  sliderInput("year",
                                              "Different year:",
                                              min = 1978,
                                              max = 2018,
                                              value = 2008)
                                ),
                                
                                mainPanel(
                                  plotlyOutput("accident.year"),
                                  verbatimTextOutput("click"))
                              ),
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


ui <- fluidPage(navbarPage(title = strong("AirPlan2.0"),
                           accident.tab)
                )



server <- function(input, output, session) {
  
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
    coord_flip())
  
}

shinyApp(ui, server)