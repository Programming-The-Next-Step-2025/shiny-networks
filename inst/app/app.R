
library(shiny)
library(colourpicker)

ui <- fluidPage(
  titlePanel("netsum - Network Analysis App"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      actionButton("check", "Check Data"),
      checkboxGroupInput("vars", "Select variables for network:", choices = NULL),
      selectizeInput("group", "Select grouping variable (optional):",
                     choices = c("None" = ""),selected = "",
                     options = list(placeholder = 'None selected')),
      selectInput("method", "Network estimation method:",
                  choices = c("EBICglasso", "glasso", "pcor", "cor"),
                  selected = "EBICglasso"),

      numericInput("tuning", "Tuning parameter (gamma):", value = 0.25, min = 0, max = 1, step = 0.05),
      actionButton("run_network", "Estimate Network"),
      textInput("group_names", "Group names for coloring nodes (comma-separated):", placeholder = "e.g. Brain, Behavior"),
      textInput("group_ranges", "Group indices for coloring nodes (semicolon-separated):", placeholder = "e.g. 1:3;4:6"),
      colourInput("color1", "Group 1 Color", "#3E9EAD"),
      colourInput("color2", "Group 2 Color", "#3E6795")

    ),

    mainPanel(
      tableOutput("preview"),
      verbatimTextOutput("checkResult"),
      verbatimTextOutput("networkStatus"),

      # customizing the plot
      textInput("plot_title", "Plot title:", value = "Network Graph"),
      selectInput("layout", "Layout:", choices = c("spring", "circle")),
      colourInput("label_color", "Label color:", value = "black"),
      htmlOutput("groupLegendLabels"),

      plotOutput("networkPlot")
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
    updateSelectizeInput(session, "group", choices = c("None" = "", vars), selected = "", server = TRUE)
  })

#--------- Compute Networks ----------------------------------------------------

  networkResult <- eventReactive(input$run_network, {
    df <- data()
    vars <- input$vars
    group <- if (input$group == "") NULL else input$group
    method <- input$method
    tuning <- if (method %in% c("pcor", "cor")) NULL else input$tuning

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
#--------- Plot Networks ----------------------------------------------------
  output$groupLegendLabels <- renderUI({
    group_names <- strsplit(input$group_names, ",\\s*")[[1]]
    group_colors <- c(input$color1, input$color2)[seq_along(group_names)]

    if (length(group_names) == 0) return(NULL)

    HTML(paste0(
      "<strong>Group Colors:</strong><br>",
      paste0(
        "<span style='font-size: 20px; color:", group_colors, "'>&#11044;</span> ",
        group_names, " = ", group_colors,
        collapse = "<br>"
      )
    ))
  })
  output$networkPlot <- renderPlot({
    req(input$run_network)

    result <- networkResult()
    net_list <- result$networks

    group_names <- strsplit(input$group_names, ",\\s*")[[1]]
    group_ranges <- strsplit(input$group_ranges, ";\\s*")[[1]]

    valid_grouping <- length(group_names) == length(group_ranges)

    if (valid_grouping) {
      groups_list <- setNames(
        lapply(group_ranges, function(r) {
          tryCatch(eval(parse(text = r)), error = function(e) NULL)
        }),
        group_names
      )

      group_colors <- setNames(
                               c(input$color1, input$color2)[seq_along(group_names)],
                               group_names
      )
    } else {
      groups_list <- NULL
      group_colors <- NULL
    }

    if (length(net_list) == 1) {
      plot_network(
        net_list[[1]],
        title = input$plot_title,
        layout = input$layout,
        label.color = input$label_color,
        groups = groups_list,
        group.colors = group_colors
      )
    } else {
      par(mfrow = c(1, length(net_list)))  # Side-by-side plots
      for (i in seq_along(net_list)) {
        plot_network(
          net_list[[i]],
          title = paste0(input$plot_title, ": Group ", result$groups[i]),
          layout = input$layout,
          label.color = input$label_color,
          groups = groups_list,
          group.colors = group_colors
        )
      }
    }
  })




}

shinyApp(ui, server)
