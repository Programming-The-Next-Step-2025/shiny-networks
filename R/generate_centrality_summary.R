#' Generate a plain-text summary of centrality measures
#'
#' Produces a compact text summary highlighting the top 3 nodes per centrality metric
#' (Strength, Betweenness, Closeness, Expected Influence) for one or more networks.
#' Designed to complement the output of `get_centrality_table()` and support group-wise summaries.
#'
#' The summary explains what each centrality metric reflects and which nodes are most central
#' within each group (or within the full network, if ungrouped).
#'
#' @param centrality_data A data frame of centrality results from `get_centrality_table()`,
#'   optionally including a `Group` column if multiple networks were estimated.
#'
#' @return A single character string with HTML formatting (e.g., bold metric names),
#'   suitable for use with `renderUI()` and `HTML()` in Shiny apps.
#'
#' @examples
#' # Simulate and estimate a simple network
#' df <- MASS::mvrnorm(100, mu = rep(0, 5), Sigma = diag(5)) |> as.data.frame()
#' colnames(df) <- paste0("V", 1:5)
#' net_result <- compute_network(df, vars = colnames(df), method = "cor")
#' centr_table <- get_centrality_table(net_result$networks[[1]])
#' generate_centrality_summary(centr_table)
#'
#' @export
generate_centrality_summary <- function(centrality_data) {
  if (is.null(centrality_data) || nrow(centrality_data) == 0) {
    return("No centrality data available.")
  }

  centrality_labels <- list(
    "Strength" = paste0(
      "indicating that these nodes have many or strong direct connections to other nodes, ",
      "making them central hubs in the network"
    ),
    "Betweenness" = paste0(
      "indicating that these nodes serve as bridges or bottlenecks ",
      "for information flow between other nodes"
    ),
    "Closeness" = paste0(
      "indicating that these nodes can quickly interact with all other nodes, ",
      "making them influential in spreading information or effects"
    ),
    "ExpectedInfluence" = paste0(
      "capturing not just the number of connections but also their direction and strength, ",
      "reflecting the node’s overall influence on the network"
    )
  )


  metrics <- names(centrality_labels)
  has_group <- "Group" %in% names(centrality_data)
  groups <- if (has_group) unique(centrality_data$Group) else "Network"

  text_out <- c()

  for (grp in groups) {
    data <- if (has_group) subset(centrality_data, Group == grp) else centrality_data
    header <- if (has_group) paste0("In Network ", grp, ",") else "In the network,"

    for (metric in metrics) {
      if (!metric %in% names(data)) next

      top_nodes <- head(data[order(-data[[metric]]), c("Node", metric)], 3)
      node_summary <- paste0(
        paste0(top_nodes$Node, " (", round(top_nodes[[metric]], 2), ")",
               collapse = ", ")
      )
      explanation <- centrality_labels[[metric]]
      text_out <- c(text_out, paste0(
        header, " the top nodes by <strong>", metric, "</strong> are: ", node_summary, ", ", explanation, "."
      ))
    }
  }

  paste(text_out, collapse = "\n\n")
}


