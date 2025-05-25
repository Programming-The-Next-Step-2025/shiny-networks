#' Plot a network graph using qgraph (with optional export)
#'
#' @param net A bootnetResult object (from compute_network()).
#' @param title Plot title.
#' @param layout Graph layout ("spring" or "circle").
#' @param label.color Color for node labels.
#' @param node.color Vector of node colors. Overridden by group.colors if provided.
#' @param groups Named list of node index groups.
#' @param group.colors Named vector mapping group names to hex colors.
#' @param legend Logical: show legend?
#' @param legend.cex Size of legend text.
#' @param filename Optional: if provided, saves the plot to this file (supports PNG or PDF).
#'
#'
#' @return A qgraph plot.
#' @export
#' @importFrom qgraph qgraph
#' @examples
#' # Simulate data
#' set.seed(1)
#' df <- MASS::mvrnorm(100, mu = rep(0, 4), Sigma = diag(4))
#' df <- as.data.frame(df)
#' colnames(df) <- c("A", "B", "C", "D")
#'
#' # Estimate network
#' result <- compute_network(df, vars = c("A", "B", "C", "D"))
#'
#' # Define groups and colors
#' groups <- list("Group1" = 1:2, "Group2" = 3:4)
#' group.colors <- c("Group1" = "#3E9EAD", "Group2" = "#3E6795")
#'
#' # Plot to screen
#' plot_network(result$networks[[1]], title = " Network", groups = groups, group.colors = group.colors)
#'
#' # Save to file
#' plot_network(result$networks[[1]], filename = "network_plot.png", groups = groups, group.colors = group.colors)
#'
plot_network <- function(net,
                         title = "Network Graph",
                         layout = "spring",
                         label.color = "black",
                         node.color = NULL,
                         groups = NULL,
                         group.colors = NULL,
                         legend = FALSE,
                         legend.cex = 0.5,
                         filename = NULL) {

  nodes <- colnames(net$graph)

  # Abbreviate node labels
  abbreviate_node <- function(name) {
    caps <- unlist(regmatches(name, gregexpr("[A-Z]", name)))
    if (length(caps) >= 3) paste(caps[1:3], collapse = "") else substr(name, 1, 3)
  }
  labels <- sapply(nodes, abbreviate_node)
  names(labels) <- nodes

  # Group coloring
  if (!is.null(groups) && !is.null(group.colors)) {
    node.color <- rep(NA, length(nodes))
    for (group in names(groups)) {
      node.color[groups[[group]]] <- group.colors[[group]]
    }
  }

  # Export to file if filename provided
  if (!is.null(filename)) {
    ext <- tools::file_ext(filename)
    if (ext == "pdf") {
      pdf(filename, width = 7, height = 7)
    } else if (ext == "png") {
      png(filename, width = 800, height = 800, res = 150)
    } else {
      stop("Unsupported file format. Use .png or .pdf")
    }
    on.exit(dev.off())
  }

  qgraph::qgraph(
    net$graph,
    layout = layout,
    labels = labels,
    label.color = label.color,
    color = node.color,
    groups = groups,
    title = title,
    legend = legend,
    legend.mode = "names",
    legend.cex = legend.cex,
    nodeNames = nodes
  )
}











