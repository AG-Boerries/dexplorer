#' @title Run DExploreR Shiny App
#'
#' @description
#' ...
#' @export
runDExploreR <- function() {
  app_dir <- system.file("dexplorer_app", package = "dexplorer")
  runApp(app_dir, display.mode = "normal")
}

# Give it a directory to get the data from
# add parameter to change the layout for the need
