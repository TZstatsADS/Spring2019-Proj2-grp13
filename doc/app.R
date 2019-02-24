library(tidyverse)
library(lubridate)

# Load Data 



# Number of accidents in each state

airport.data <- read.csv("")

state.names <- unique(airport.data$Event.State)

#################
library(shiny)
library(plotly)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Different year:",
                  min = 1978,
                  max = 2018,
                  value = 2008)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot"),
      verbatimTextOutput("click"))
      )

)



server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
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
  
}

shinyApp(ui, server)