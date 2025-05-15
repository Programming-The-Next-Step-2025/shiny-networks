library(shiny)

ui <- fluidPage(
  titlePanel("netsum - Network Analysis App"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      checkboxGroupInput("vars", "Select variables for network:", choices = NULL),
      selectInput("group", "Select grouping variable (optional):", choices = NULL, selected = NULL),
      selectInput("method", "Network estimation method:",
                  choices = c("EBICglasso", "glasso", "pcor", "cor_auto"),
                  selected = "EBICglasso"),

      numericInput("tuning", "Tuning parameter (gamma):", value = 0.5, min = 0, max = 1, step = 0.05),
      actionButton("run_network", "Estimate Network"),

      actionButton("check", "Check Data")
    ),

    mainPanel(
      tableOutput("preview"),
      verbatimTextOutput("checkResult"),
      verbatimTextOutput("networkStatus")
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


#--------- update UI based on uploaded data ------------------------------------

  observeEvent(data(), {
    vars <- names(data())
    updateCheckboxGroupInput(session, "vars", choices = vars, selected = vars)
    updateSelectInput(session, "group", choices = c("", vars), selected = "")
  })

#--------- Compute Networks ----------------------------------------------------

  networkResult <- eventReactive(input$run_network, {
    df <- data()
    vars <- input$vars
    group <- if (input$group == "") NULL else input$group
    method <- input$method
    tuning <- input$tuning

    compute_network(df, vars = vars, group_var = group, method = method, tuning = tuning)
  })

  output$networkStatus <- renderText({
    req(input$run_network)
    result <- networkResult()

    msgs <- lapply(result$networks, function(net) attr(net, "empty_message"))
    msgs <- unlist(msgs)

    if (length(msgs) == 0) return("✅ Network(s) estimated successfully.")
    return(paste(msgs, collapse = "\n"))
  })

}

shinyApp(ui, server)
