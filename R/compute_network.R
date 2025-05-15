#' Estimate network(s) from data
#'
#' Estimates one or more networks using `bootnet::estimateNetwork()`.
#'
#' @param data A data frame containing the input data.
#' @param vars Character vector of column names to include in the network.
#' @param group_var (Optional) A column name used to split the data into groups (e.g., "group"). If NULL, estimates a single network.
#' @param method The network estimation method (e.g., "EBICglasso", "glasso", "pcor").
#' @param tuning Tuning parameter passed to `bootnet`. Default is 0.25.
#'
#' @return A list containing:
#' \describe{
#'   \item{networks}{A list of one or more `bootnetResult` objects.}
#'   \item{groups}{Character vector of group names (if grouping used), otherwise NULL.}
#' }
#'
#' @examples
#' set.seed(42)
#' df <- MASS::mvrnorm(100, mu = rep(0, 4), Sigma = diag(4))
#' df <- as.data.frame(df)
#' colnames(df) <- c("A", "B", "C", "D")
#' compute_network(df, vars = c("A", "B", "C", "D"))
#'
#' @export
#' @importFrom bootnet estimateNetwork

compute_network <- function(data, vars, group_var = NULL, method = "EBICglasso", tuning = 0.25) {

  # Check group_var exists (if provided)
  if (!is.null(group_var)) {
    if (!group_var %in% colnames(data)) {
      stop("Grouping variable not found in data.")
    }
  }

  # Check all selected vars are numeric
  if (!all(sapply(data[, vars, drop = FALSE], is.numeric))) {
    stop("All selected variables must be numeric.")
  }

  # Helper to estimate a single network
  estimate <- function(df) {
    net <- bootnet::estimateNetwork(
      df,
      default = method,
      tuning = tuning
    )
    if (all(net$graph == 0)) {
      attr(net, "empty_message") <- "⚠️ Empty network detected (no edges). Try adjusting tuning."
    }
    return(net)
  }

  # If no group_var: filter and return one network
  if (is.null(group_var)) {
    data <- data[, vars, drop = FALSE]
    net <- estimate(data)
    return(list(networks = list(net), groups = NULL))
  }

  # Handle grouped data (any number of groups)
  groups <- unique(data[[group_var]])
  if (length(groups) < 2) {
    stop("Grouping variable must have at least two levels.")
  }

  network_list <- lapply(groups, function(g) {
    df_group <- data[data[[group_var]] == g, vars, drop = FALSE]
    net <- estimate(df_group)
    if (all(net$graph == 0)) {
      attr(net, "empty_message") <- paste0("⚠️ Empty network for group '", g, "'. Try adjusting tuning.")
    }
    return(net)
  })

  return(list(networks = network_list, groups = groups))
}

