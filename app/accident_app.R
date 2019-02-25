library(tidyverse)
library(lubridate) # used to extract year from date
library(shiny)
library(plotly)

# Accidents
airport.data <- read.csv("../output/accident_state.csv")
# month.data <- read.csv("../output/accident_month.csv")
aircraft.make <- read.csv("../output/accident_aircraft.csv")
operator <- read.csv("../output/accident_operator.csv")
state.names <- unique(airport.data$Event.State)


accident.tab1 <- tabPanel("Accident Map",
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
                          ))



# accident.tab2 <- tabPanel("By Year",
#                           plotOutput("accident.month")
#                           )

accident.tab3 <- tabPanel("By Other Elements",
                         plotlyOutput("accident.operator"),
                         br(),
                         plotlyOutput("aircraft"))

accident.tab <-   navbarMenu("aaa",
                     accident.tab1,
                     accident.tab3
                     # accident.tab2,
                     # accident.tab3,
                     # accident.tab4,
                     # accident.tab5
                )

# tab2 <- tabPanel()

ui <- fluidPage(navbarPage(title = strong("AirPlan2.0"),
                           accident.tab
                           # ,tab2
)
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
      filter(year == input$year)
    
    plot_ly(z = plotdata$count, locations = plotdata$Event.State,
            type = 'choropleth', locationmode = 'USA-states') %>%
      layout(geo = g)
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    # if (is.null(d)) "Click on a state to view event data" else d
  })
  
  output$accident.month <- renderPlotly(
    month.data %>%
      plot_ly(
        x = ~month, 
        y = ~count, 
        frame = ~year, 
        text = ~count, 
        hoverinfo = "text",
        linetype = I("dash"),
        name = 'Accident Number'
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
  
}

shinyApp(ui, server)