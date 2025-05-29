#' Parse group names, ranges, and color inputs
#'
#' Helper function to parse group names, ranges, and user-selected colors from Shiny inputs.
#'
#' @param group_names character vector of group names
#' @param group_ranges character vector of ranges (e.g., c("1:3", "4:6"))
#' @param input shiny input object (to pull color values)
#'
#' @return A list with two elements:
#' \describe{
#'   \item{groups_list}{Named list of index vectors}
#'   \item{group_colors}{Named vector of hex colors}
#' }
#' @export
parse_group_colors <- function(group_names, group_ranges, input) {
  if (length(group_names) != length(group_ranges) || length(group_names) == 0) {
    return(list(groups_list = NULL, group_colors = NULL))
  }

  parsed_ranges <- lapply(group_ranges, function(r) {
    tryCatch(eval(parse(text = r)), error = function(e) NULL)
  })

  if (any(sapply(parsed_ranges, is.null))) {
    return(list(groups_list = NULL, group_colors = NULL))
  }

  groups_list <- setNames(parsed_ranges, group_names)

  group_colors <- sapply(seq_along(group_names), function(i) {
    input[[paste0("group_color_", i)]] %||% "#999999"
  })
  names(group_colors) <- group_names

  list(groups_list = groups_list, group_colors = group_colors)
}
