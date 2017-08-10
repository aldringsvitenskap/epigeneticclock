#
# Calculates age from methylation data
#

library(shiny)

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
                
                mainPanel(tableOutput("table"))))

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
          ageResults = tryCatch({
            calculateAge(inputFilePath)
          }, error = function(e) {
            paste("Error: ", e$message)
          })
        }
      )
      
      output$table <- renderTable(ageResults)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
