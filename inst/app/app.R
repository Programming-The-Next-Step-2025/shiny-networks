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


# ============ Server ==========================================================
server <- function(input, output, session) {


#--------- check csv and return output------------------------------------------

  # This will store messages to show to the user
  fileStatus <- reactiveVal(NULL)

  # check_csv() function to validate the file
  data <- reactive({ # reactive means the server waits for something to happen
    req(input$file)  # Don’t try this unless a file has actually been uploaded
    result <- check_csv(input$file$datapath)
    fileStatus(result$message)
    return(result$data)
  })

  # Show the result message from check_csv()
  output$checkResult <- renderText({
    req(input$check)
    isolate({
      df <- data()
      return(fileStatus())
    })
  })


#--------- Compute Networks ------------------------------------------











}

shinyApp(ui, server)
