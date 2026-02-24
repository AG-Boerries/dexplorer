#' @title Run DExploreR Shiny App
#'
#' @description
#' ...
#' @export
runDExploreR <- function(
  asset_dir = "/Users/tobiashundertmark/Downloads",
  data_dir = "/Users/tobiashundertmark/Downloads/data"
) {
  asset_dir <- normalizePath(asset_dir, mustWork = TRUE)
  data_dir <- normalizePath(data_dir, mustWork = TRUE)

  addResourcePath(
    "www",
    system.file("dexplorer_assets", package = "dexplorer")
  )

  addResourcePath(
    "color_palettes",
    system.file("dexplorer_assets/color_palettes", package = "dexplorer")
  )

  addResourcePath("assets", asset_dir)

  config <- list(
    data_dir = data_dir
  )

  app <- shinyApp(
    ui = app_ui(),
    server = function(input, output, session) {
      app_server(input, output, session, config)
    },
    onStart = app_startup
  )
  runApp(app)
}
