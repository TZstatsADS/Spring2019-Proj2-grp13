#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Load Data 
# month.data <- read.csv("../data/accident_month.csv")
airport.data <- read.csv("../data/accident_state.csv")
state.names <- unique(airport.data$Event.State)

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
