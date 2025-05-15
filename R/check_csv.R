
#' Check if file is a valid CSV and has no missing values
#'
#' @param file_path Full path to the uploaded file
#'
#' @return A list with a `message` and `data` (or NULL if invalid)
#'
#' @examples
#' result <- check_csv("example.csv")
#' result$message
#' head(result$data)
#'
#' @export
check_csv <- function(file_path) {
  if (!grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    return(list(message = "❌ The uploaded file is not a CSV.", data = NULL))
  }

  df <- tryCatch(
    read.csv(file_path),
    error = function(e) return(NULL)
  )

  if (is.null(df)) {
    return(list(message = "❌ Failed to read the CSV file.", data = NULL))
  }

  if (any(is.na(df))) {
    return(list(message = "⚠️ Data contains missing values (NA). Please clean it before proceeding.", data = NULL))
  }

  return(list(message = "✅ Data looks good. No missing values detected.", data = df))
}
