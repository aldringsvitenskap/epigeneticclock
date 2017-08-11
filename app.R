#
# Calculates age from methylation data
#

library(shiny)
library(readr)

# Define UI
ui <- fluidPage(titlePanel("Epigenetic clock"),
                
                sidebarLayout(sidebarPanel(
                  fileInput(
                    inputId = "methylationFile",
                    label = "Choose methylation csv file",
                    accept = c('text/csv',
                               'text/comma-separated-values,text/plain',
                               '.csv'),
                    multiple = FALSE
                  )
                ),
                
                mainPanel(tableOutput("table"), textOutput("logfile"))))

# Define server
server <- function(input, output) {
  options(shiny.maxRequestSize = 200 * 1024 ^ 2)
  source("horvath2013.R")
  
  observe({
    if (!is.null(input$methylationFile)) {
      inputFileName <- input$methylationFile[[1, 'name']]
      inputFileSize <- input$methylationFile[[1, 'size']]
      inputFilePath <- input$methylationFile[[1, 'datapath']]
      
      withProgress(
        message = sprintf("Processing %s", inputFileName),
        detail = sprintf("%d bytes", inputFileSize),
        value = 0,
        {
          setProgress(1/20)
          ageResults = tryCatch({
            csvFile = read.csv.sql(inputFilePath)
            calculateAge(csvFile)
          }, error = function(e) {
            paste("Error: ", e$message)
          })
          setProgress(1)
        }
      )
      
      output$table <- renderTable(ageResults)
      output$logfile <- renderText(read_file("LogFile.txt"))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
