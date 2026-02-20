#' @title Run DExploreR Shiny App
#'
#' @description
#' ...
#' @export
runDExploreR <- function() {
  app_dir <- system.file("dexplorer_app", package = "dexplorer")
  shiny::runApp(app_dir, display.mode = "normal")
}
