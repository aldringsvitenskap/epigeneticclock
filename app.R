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
                  helpText("Supports .csv files where the first column lists the cpg cites, and the following columns list their methylation values"),
                  textInput(inputId = "gsmID", label = "GSM sample ID", placeholder = "GSMxxx"),
                  helpText("Enter a GSM id from the Geo Expression Omnibus and wait for processing to start"),
                  tags$a(href="https://doi.org/10.1186/gb-2013-14-10-r115", "Based on \"DNA methylation age of human tissues and cell types\" by Steve Horvath")
                ),
                
                mainPanel(tableOutput("table"), tableOutput("description"), textOutput("logfile"))))

# Define server
server <- function(input, output) {
  options(shiny.maxRequestSize = 200 * 1024 ^ 2)
  source("horvath2013.R")
  
  methylation <- reactiveValues(Table  = NULL, Message = NULL, Detail = NULL, Description = NULL)
  
  observeEvent(input$gsmID, {
    if (!is.null(input$gsmID) && nchar(input$gsmID) > 3) {
      gsmTable <-  tryCatch({
        getGEO(input$gsmID)
      }, error = function(e) {
        NULL
      })
      methylation$Message <- sprintf("Processing %s", input$gsmID)
      methylation$Detail <- "Unknown size"
      if (!is.null(gsmTable)) {
        methylation$Description <- head(Meta(gsmTable))
        methylation$Table <- Table(gsmTable)[,1:2]
      } else {
        methylation$Description <- NULL
        methylation$Table <- NULL
      }
    }
  })

  observeEvent(input$methylationFile, {
    inputFileName <- input$methylationFile[[1, 'name']]
    inputFileSize <- input$methylationFile[[1, 'size']]
    inputFilePath <- input$methylationFile[[1, 'datapath']]
    
    methylation$Message <- sprintf("Processing %s", inputFileName)
    methylation$Detail <- sprintf("%d bytes", inputFileSize)
    methylation$Description <- "Empty..."
    methylation$Table <- tryCatch({
      read.csv.sql(inputFilePath)
    }, error = function(e) {
      NULL
    })
  })

  observeEvent(methylation$Table, {
    withProgress(
      message = methylation$Message,
      detail = methylation$Detail,
      value = 0,
      {
        setProgress(1/20)
        ageResults = tryCatch({
          calculateAge(methylation$Table)
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
    output$description <- renderTable({methylation$Description},
                                      striped = TRUE,  
                                      rownames = TRUE,
                                      caption = "GSM metadata",
                                      caption.placement = getOption("xtable.caption.placement", "top"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
