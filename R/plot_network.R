#' Plot a network graph using qgraph
#'
#' @param net A bootnetResult object (as returned by compute_network).
#' @param title Plot title.
#' @param layout Graph layout ("spring" or "circle").
#' @param labels Vector of node labels. Default = first 3 letters of variable names.
#' @param label.color Color for node labels.
#' @param node.color Vector of colors (one per node). Overridden by group.colors if provided.
#' @param groups Named list of node index groups (e.g., list("A" = 1:3, "B" = 4:6)).
#' @param group.colors Named vector mapping group names to hex colors.
#' @param legend Logical: show legend?
#' @param legend.cex Size of legend text.
#'
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
#' # Plot network
#' plot_network(result$networks[[1]],
#'              title = "Grouped Network",
#'              groups = groups,
#'              group.colors = group.colors,
#'              label.color = "white")
#'
#' @return A qgraph plot.
#' @export
#' @importFrom qgraph qgraph
plot_network <- function(net,
                         title = "Network Graph",
                         layout = "spring",
                         labels = NULL,
                         label.color = "black",
                         node.color = NULL,
                         groups = NULL,
                         group.colors = NULL,
                         legend = TRUE,
                         legend.cex = 0.5) {

  nodes <- colnames(net$graph)

  if (is.null(labels)) {
    labels <- substr(nodes, 1, 3)
  }

  # Handle group-based coloring if specified
  if (!is.null(groups) && !is.null(group.colors)) {
    node.color <- rep(NA, length(nodes))
    for (group in names(groups)) {
      node.color[groups[[group]]] <- group.colors[[group]]
    }
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
    legend.cex = legend.cex
  )
}















