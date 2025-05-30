#' Launch the netsum Shiny app
#'
#' This function launches the Shiny app built into the netsum package.
#'
#' @export
launch_netsum_app <- function() {
  app_dir <- system.file("app", package = "netsum")
  if (app_dir == "") stop("\u274C Could not find app directory. Did you run devtools::load_all()?")
  shiny::runApp(app_dir)
}
