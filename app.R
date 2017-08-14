#
# Calculates age from methylation data
#

library(shiny)
library(readr)
library(sqldf)
library(GEOquery)

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
                  ),
                  textInput(inputId = "gsmID", label = "GSM sample ID", placeholder = "GSMxxx")
                ),
                
                mainPanel(tableOutput("table"), tableOutput("description"), textOutput("logfile"))))

# Define server
server <- function(input, output) {
  options(shiny.maxRequestSize = 200 * 1024 ^ 2)
  source("horvath2013.R")
  
  observe({
    if ((!is.null(input$methylationFile)) || (!is.null(input$gsmID) && nchar(input$gsmID) > 3)) {
      methylationTable = NULL;
      methylationMessage = NULL;
      methylationDetail = NULL;
      methylationDescription = NULL;
      
      if (!is.null(input$methylationFile)) {
        inputFileName <- input$methylationFile[[1, 'name']]
        inputFileSize <- input$methylationFile[[1, 'size']]
        inputFilePath <- input$methylationFile[[1, 'datapath']]
        
        methylationMessage <- sprintf("Processing %s", inputFileName)
        methylationDetail <- sprintf("%d bytes", inputFileSize)
        methylationDescription <- "Empty..."
        methylationTable <- read.csv.sql(inputFilePath)
      } else {
        gsmTable <-  tryCatch({
          getGEO(input$gsmID)
        }, error = function(e) {
          NULL
        })
        methylationMessage <- sprintf("Processing %s", input$gsmID)
        methylationDetail <- "Unknown size"
        if (!is.null(gsmTable)) {
          methylationTable <- Table(gsmTable)[,1:2]
          methylationDescription <- head(Meta(gsmTable))
        }
      }
      
      withProgress(
        message = methylationMessage,
        detail = methylationDetail,
        value = 0,
        {
          setProgress(1/20)
          ageResults = tryCatch({
            calculateAge(methylationTable)
          }, error = function(e) {
            paste("Error: ", e$message)
          })
          setProgress(1)
        }
      )
      
      output$table <- renderTable({ageResults},  
                                  striped = TRUE,  
                                  rownames = TRUE,
                                  caption = "Estimaged DNAm age",
                                  caption.placement = getOption("xtable.caption.placement", "top"))
      output$logfile <- renderText(read_file("LogFile.txt"))
      output$description <- renderTable({methylationDescription},
                                        striped = TRUE,  
                                        rownames = TRUE,
                                        caption = "GSM metadata",
                                        caption.placement = getOption("xtable.caption.placement", "top"))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
