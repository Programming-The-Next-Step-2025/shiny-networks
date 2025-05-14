library(shiny)

ui <- fluidPage(
  titlePanel("netsum - Network Analysis App"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      actionButton("check", "Check Data")
    ),

    mainPanel(
      tableOutput("preview"), # this is like a placeholder in the ui that i can
                              # call on later in the server to fill
      verbatimTextOutput("checkResult")
    )
  )
)


# ============ Server ========================================================
server <- function(input, output, session) {

  # This will store messages to show to the user
  fileStatus <- reactiveVal(NULL)

  data <- reactive({ # reactive means the servers waits for something to happen
    req(input$file)  # Don’t try this unless a file has actually been uploaded
      filename <- input$file$name

      # Check file extension
      if (!grepl("\\.csv$", filename, ignore.case = TRUE)) {
        fileStatus("❌ Error: The uploaded file is not a CSV.")
        return(NULL)
      }
      # Try reading file
      df <- tryCatch(
        read.csv(input$file$datapath),  # gives the temporary location of the uploaded
                                        #file: data() is now a shortcut to the uploaded file
        error = function(e) {
          fileStatus("❌ Error: Failed to read the CSV file.")
          return(NULL)
        }
      )


      fileStatus(NULL)  # Clear message if all good
      return(df)
  })


  # Only show result message if CSV is valid
  output$checkResult <- renderText({# output is a list of outputs the app will show
                                      # with  $preview im telling the server where to
                                      # display it in the ui at the location where
                                      # Table output "preview".
                                      # render_something tells ui what form the output
                                      # should have
    req(input$check)
    isolate({
      df <- data()

      # If data is NULL (bad format or load failed), show stored message
      if (is.null(df)) {
        return(fileStatus())
      }

      # Else check for missing values
      if (any(is.na(df))) {
        return("⚠️ Data contains missing values (NA). Please clean it before proceeding.")
      } else {
        return("✅ Data looks good. No missing values detected.")
      }
    })
  })
}


shinyApp(ui, server)
