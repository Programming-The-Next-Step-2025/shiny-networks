
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

      plotOutput("networkPlot"),

      # section for top nodes
      h4("Top 5 Central Nodes (by Strength)"),
      tableOutput("topNodes"),

      #Export Buttons
      h4("Download Results"),
      downloadButton("downloadPlotPNG", "Download Plot (PNG)"),
      downloadButton("downloadPlotPDF", "Download Plot (PDF)"),
      downloadButton("downloadCentrality", "Download Centrality Table (CSV)")

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
    print(vars) # for debugging
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

  #--------- Centrality Computation ------------------------------------------------
  centralityTables <- reactive({
    result <- networkResult()
    if (is.null(result) || is.null(result$networks)) return(NULL)

    net_list <- result$networks
    group_labels <- result$groups

    tables <- lapply(seq_along(net_list), function(i) {
      net <- net_list[[i]]
      label <- if (!is.null(group_labels)) group_labels[i] else NULL
      get_centrality_table(net, group = label)
    })

    names(tables) <- if (!is.null(group_labels)) group_labels else "Network"
    return(tables)
  })

  #--------- Render Top 5 Central Nodes -------------------------------------------
  output$topNodes <- renderTable({
    tables <- centralityTables()
    if (length(tables) == 0) return(NULL)

    top_list <- lapply(names(tables), function(group) {
      tab <- tables[[group]]
      top_nodes <- tab[order(-tab$Strength), ][1:min(5, nrow(tab)), ]
      top_nodes$Group <- group
      top_nodes
    })

    do.call(rbind, top_list)
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
    result <- networkResult()
    if (is.null(result) || is.null(result$networks)) return(NULL)

    original_labels <- input$vars

    # Smarter abbreviation: 3 uppercase letters if present, otherwise fallback
    abbreviate_node <- function(name) {
      caps <- unlist(regmatches(name, gregexpr("[A-Z]", name)))
      if (length(caps) >= 3) {
        paste(caps[1:3], collapse = "")
      } else {
        clean <- gsub("[^a-zA-Z]", "", name)
        substr(clean, 1, 3)
      }
    }

    label_map <- sapply(original_labels, abbreviate_node)
    names(label_map) <- original_labels

    HTML(paste0(
      "<strong>Node Legend:</strong><br>",
      paste0(
        "<span style='font-family: sans-serif;'>", label_map, ": ", names(label_map), "</span>",
        collapse = "<br>"
      )
    ))
  })


  #--------- Plot Networks --------------------------------------------------------
  output$networkPlot <- renderPlot({
    req(input$run_network)
    result <- tryCatch(networkResult(), error = function(e) NULL)
    if (is.null(result) || is.null(result$networks)) return(NULL)

    net_list <- result$networks

    plot_titles <- strsplit(input$plot_title, ",\\s*")[[1]]
    while (length(plot_titles) < length(net_list)) {
      plot_titles <- c(plot_titles, "")
    }

    # COLOR AND GROUP HANDLING
    group_names <- strsplit(trimws(input$group_names), ",\\s*")[[1]]
    group_ranges <- strsplit(trimws(input$group_ranges), ";\\s*")[[1]]
    parsed <- parse_group_colors(group_names, group_ranges, input)
    groups_list <- parsed$groups_list
    group_colors <- parsed$group_colors

    if (length(net_list) == 1) {
      plot_network(
        net_list[[1]],
        title = plot_titles[1],
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
          title = plot_titles[i],
          layout = input$layout,
          label.color = input$label_color,
          groups = groups_list,
          group.colors = group_colors
        )
      }
    }
  })


  #--------- Updated Plot Download Handlers (with group/colors/layout) -------------

  output$downloadPlotPNG <- downloadHandler(
    filename = function() {
      paste0("network_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      result <- networkResult()

      # COLOR & GROUP HANDLING
      group_names <- strsplit(trimws(input$group_names), ",\\s*")[[1]]
      group_ranges <- strsplit(trimws(input$group_ranges), ";\\s*")[[1]]
      parsed <- parse_group_colors(group_names, group_ranges, input)
      groups_list <- parsed$groups_list
      group_colors <- parsed$group_colors

      titles <- strsplit(input$plot_title, ",\\s*")[[1]]
      while (length(titles) < length(result$networks)) {
        titles <- c(titles, "")
      }

      png(file, width = 1000, height = 800)
      if (length(result$networks) == 1) {
        plot_network(
          result$networks[[1]],
          title = titles[1],
          layout = input$layout,
          label.color = input$label_color,
          groups = groups_list,
          group.colors = group_colors,
          filename = NULL
        )
      } else {
        par(mfrow = c(1, length(result$networks)))
        for (i in seq_along(result$networks)) {
          plot_network(
            result$networks[[i]],
            title = titles[i],
            layout = input$layout,
            label.color = input$label_color,
            groups = groups_list,
            group.colors = group_colors,
            filename = NULL
          )
        }
      }
      dev.off()
    }
  )



  output$downloadPlotPDF <- downloadHandler(
    filename = function() {
      paste0("network_plot_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      result <- networkResult()

      # COLOR & GROUP HANDLING
      group_names <- strsplit(trimws(input$group_names), ",\\s*")[[1]]
      group_ranges <- strsplit(trimws(input$group_ranges), ";\\s*")[[1]]
      parsed <- parse_group_colors(group_names, group_ranges, input)
      groups_list <- parsed$groups_list
      group_colors <- parsed$group_colors

      titles <- strsplit(input$plot_title, ",\\s*")[[1]]
      while (length(titles) < length(result$networks)) {
        titles <- c(titles, "")
      }

      pdf(file, width = 11, height = 8.5)
      if (length(result$networks) == 1) {
        plot_network(
          result$networks[[1]],
          title = titles[1],
          layout = input$layout,
          label.color = input$label_color,
          groups = groups_list,
          group.colors = group_colors,
          filename = NULL
        )
      } else {
        par(mfrow = c(1, length(result$networks)))
        for (i in seq_along(result$networks)) {
          plot_network(
            result$networks[[i]],
            title = titles[i],
            layout = input$layout,
            label.color = input$label_color,
            groups = groups_list,
            group.colors = group_colors,
            filename = NULL
          )
        }
      }
      dev.off()
    }
  )



output$downloadCentrality <- downloadHandler(
  filename = function() {
    paste0("centrality_table_", Sys.Date(), ".csv")
  },
  content = function(file) {
    tables <- centralityTables()
    if (length(tables) == 1) {
      write.csv(tables[[1]], file, row.names = FALSE)
    } else {
      all <- do.call(rbind, tables)
      write.csv(all, file, row.names = FALSE)
    }
  }
)

}

shinyApp(ui, server)


