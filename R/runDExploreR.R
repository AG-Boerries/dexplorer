#' @title Run DExploreR Shiny App
#'
#' @description
#' ...
#'
#' @param asset_dir A directory containing the assets for DExploreR, such as logos.
#'
#' @param data_dir A directory containing `.rds` files and corresponding `.csv`s as prepared by \code{\link{createDataSet}()}.
#'
#' @export
runDExploreR <- function(
  asset_dir = "/Users/tobiashundertmark/Downloads",
  data_dir = "/Users/tobiashundertmark/Downloads/data"
) {
  asset_dir <- normalizePath(asset_dir, mustWork = TRUE)
  data_dir <- normalizePath(data_dir, mustWork = TRUE)

  # TODO: make sure the python environment is sourced and loaded correctly
  # use_virtualenv(
  #   virtualenv = "r-reticulate-dexplorer",
  #   required = TRUE
  # )

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
