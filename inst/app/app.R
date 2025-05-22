
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
      uiOutput("colorPickers")

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


server <- function(input, output, session) {

  #--------- check csv and return output ------------------------------------------
  fileStatus <- reactiveVal(NULL)

  data <- reactive({
    req(input$file)
    result <- check_csv(input$file$datapath)
    fileStatus(result$message)
    return(result$data)
  })

  output$checkResult <- renderText({
    req(input$check)
    isolate({
      df <- data()
      return(fileStatus())
    })
  })

  #--------- update UI based on uploaded data -------------------------------------
  observeEvent(data(), {
    vars <- names(data())
    updateCheckboxGroupInput(session, "vars", choices = vars, selected = vars)
    updateSelectizeInput(session, "group", choices = c("None" = "", vars), selected = "", server = TRUE)
  })

  #--------- Compute Networks -----------------------------------------------------
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

  #--------- Dynamic Color Pickers ------------------------------------------------
  output$colorPickers <- renderUI({
    group_names <- strsplit(trimws(input$group_names), ",\\s*")[[1]]
    if (length(group_names) == 1 && group_names == "") return(NULL)

    lapply(seq_along(group_names), function(i) {
      colourInput(
        inputId = paste0("group_color_", i),
        label = paste("Color for group:", group_names[i]),
        value = "#999999"
      )
    }) |> tagList()
  })

  #--------- Group Legend ---------------------------------------------------------
  output$groupLegendLabels <- renderUI({
    group_names <- strsplit(trimws(input$group_names), ",\\s*")[[1]]
    if (length(group_names) == 1 && group_names == "") return(NULL)

    group_colors <- sapply(seq_along(group_names), function(i) {
      input[[paste0("group_color_", i)]] %||% "#999999"
    })

    HTML(paste0(
      "<strong>Group Colors:</strong><br>",
      paste0(
        "<span style='font-size: 20px; color:", group_colors, "'>&#11044;</span> ",
        group_names, " = ", group_colors,
        collapse = "<br>"
      )
    ))
  })

  #--------- Plot Networks --------------------------------------------------------
   output$networkPlot <- renderPlot({
     cat("🔍 renderPlot starting\n")
    req(input$run_network)
    result <- tryCatch(networkResult(), error = function(e) NULL)
    cat("🔍 networkResult safely evaluated\n")
    if (is.null(result) || is.null(result$networks)) return(NULL)

    net_list <- result$networks

    # Robust group name handling
    group_names <- tryCatch({
      if (!is.null(input$group_names) && nzchar(input$group_names)) {
        strsplit(trimws(input$group_names), ",\\s*")[[1]]
      } else {
        character(0)
      }
    }, error = function(e) character(0))

    # Robust group range handling
    group_ranges <- tryCatch({
      if (!is.null(input$group_ranges) && nzchar(input$group_ranges)) {
        strsplit(trimws(input$group_ranges), ";\\s*")[[1]]
      } else {
        character(0)
      }
    }, error = function(e) character(0))

    valid_grouping <- length(group_names) == length(group_ranges) &&
      length(group_names) > 0

    groups_list <- NULL
    group_colors <- NULL

    if (valid_grouping) {
      parsed_ranges <- lapply(group_ranges, function(r) {
        tryCatch(eval(parse(text = r)), error = function(e) NULL)
      })

      if (all(sapply(parsed_ranges, Negate(is.null)))) {
        groups_list <- setNames(parsed_ranges, group_names)

        color_inputs_exist <- all(sapply(seq_along(group_names), function(i) {
          !is.null(input[[paste0("group_color_", i)]])
        }))

        if (color_inputs_exist) {
          group_colors <- list()
          for (i in seq_along(group_names)) {
            color_input <- paste0("group_color_", i)
            group_colors[[group_names[i]]] <- input[[color_input]]
          }
        }
      }
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
      par(mfrow = c(1, length(net_list)))
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
