#' Compute centrality table from a network
#'
#' Computes a table of node centrality measures (Betweenness, Closeness, Strength, Expected Influence)
#' from a `bootnet` network object, using `qgraph::centrality_auto()`.
#'
#' Ensures that node labels are correctly preserved from the original data.
#'
#' @param net A `bootnetResult` object from `compute_network()`.
#' @param group Optional: a string to label the group (if comparing multiple networks).
#'
#' @return A data frame with centrality measures per node (and group if specified).
#'
#' @importFrom qgraph qgraph centrality_auto
#' @export
#'
#' @examples
#' set.seed(123)
#' df <- MASS::mvrnorm(100, mu = rep(0, 5), Sigma = diag(5)) |> as.data.frame()
#' colnames(df) <- paste0("V", 1:5)
#' net_result <- compute_network(df, vars = colnames(df), method = "cor")
#' centrality_table <- get_centrality_table(net_result$networks[[1]])
#' print(centrality_table)
get_centrality_table <- function(net, group = NULL) {
  if (is.null(net) || is.null(net$graph) || all(net$graph == 0)) {
    warning("Network is empty or invalid.")
    return(NULL)
  }

  # Set proper node labels if missing
  if (!is.null(net$data)) {
    colnames(net$graph) <- rownames(net$graph) <- colnames(net$data)
  }

  # Create qgraph object (suppress plot)
  qg <- qgraph::qgraph(net$graph, DoNotPlot = TRUE)

  # Compute centrality
  centrality <- qgraph::centrality_auto(qg)
  df <- as.data.frame(centrality$node.centrality)

  # Add node names
  df$Node <- rownames(df)
  df <- df[, c("Node", setdiff(names(df), "Node"))]

  # Add group label (optional)
  if (!is.null(group)) {
    df$Group <- group
    df <- df[, c("Group", names(df)[names(df) != "Group"])]
  }

  return(df)
}
