#
# Calculates age from methylation data
#

library(shiny)

# Define UI
ui <- fluidPage(titlePanel("Epigentic clock"),
                
                sidebarLayout(
                  sidebarPanel(
                    fileInput(
                      inputId = "methylationFile",
                      label = "Choose methylation csv file",
                      accept = c('text/csv',
                                 'text/comma-separated-values,text/plain',
                                 '.csv'),
                      multiple = FALSE
                    )
                  ),
                  
                  mainPanel(tableOutput("table"),
                            textOutput("dump"))
                ))

# Define server
server <- function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  source("horvath2013.R")
  
  observe({
    if (!is.null(input$methylationFile)) {
      inputFileName <- input$methylationFile[[1, 'name']]
      inputFileSize <- input$methylationFile[[1, 'size']]
      inputFilePath <- input$methylationFile[[1, 'datapath']]
      
      output$dump <-
        renderText(sprintf("Processing %s %d bytes", inputFileName, inputFileSize))
      

      output$table <- renderTable(calculateAge(inputFilePath))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
