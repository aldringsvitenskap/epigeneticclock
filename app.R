#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Epigenetic clock"),
  mainPanel(
    tableOutput("table"),
    
    #plotOutput("distPlot"),
    textOutput("dump")))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$dump <- renderText("Calculating")
  
  source("horvath2013.R")
  
  output$table <- renderTable(datout)
  
  output$dump <- renderText("Hello")
}

# Run the application
shinyApp(ui = ui, server = server)
