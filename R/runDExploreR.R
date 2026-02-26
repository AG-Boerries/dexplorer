#' @title Run DExploreR Shiny App
#'
#' @description
#' This functions runs the DExploreR Shiny app. It sets up a python environment, if not existing, using \code{\link{configurePyEnv}()}, mounts user specified assets and data directories and finally starts DExploreR using \code{\link[shiny]{runApp}()} on "0.0.0.0:1234". If running without any parameters, it uses the data and assets provided with this package form GSE273186.
#'
#' @param asset_dir A directory containing the assets for DExploreR, such as logos.
#'
#' @param data_dir A directory containing `.rds` files and corresponding `.csv`s as prepared by \code{\link{createDataSet}()}.
#'
#' @param env_path Path to the Python virtual environment to use for DExploreR. If `NULL`, the default environment created by \code{\link{configurePyEnv}()} will be used. If no environment exists, this will also create a new environment.
#'
#' @param host The host address to run the Shiny app on. Default is "0.0.0.0".
#'
#' @param port The port to run the Shiny app on. Default is 1234.
#'
#' @export
runDExploreR <- function(
  asset_dir = NULL,
  data_dir = NULL,
  env_path = NULL,
  host = "0.0.0.0",
  port = 1234
) {
  # If no directories provided, use the default data and assets provided with this package
  if (is.null(asset_dir)) {
    asset_dir <- system.file("extdata", "assets", package = "dexplorer")
  }
  if (is.null(data_dir)) {
    data_dir <- system.file("extdata", "data", package = "dexplorer")
  }

  asset_dir <- normalizePath(asset_dir, mustWork = TRUE)
  data_dir <- normalizePath(data_dir, mustWork = TRUE)

  # Configure python environment if not provided
  if (is.null(env_path)) {
    env_path <- configurePyEnv()
  }

  use_virtualenv(
    env_path,
    required = TRUE
  )

  # Add further assets contained in the package
  addResourcePath(
    "www",
    system.file("dexplorer_assets", package = "dexplorer")
  )

  # Add the color palettes as resources
  addResourcePath(
    "color_palettes",
    system.file("dexplorer_assets/color_palettes", package = "dexplorer")
  )

  # Add the user-specified assets
  addResourcePath("assets", asset_dir)

  # Create a config list to pass to the server function
  config <- list(
    data_dir = data_dir
  )

  # Create the Shiny app
  app <- shinyApp(
    ui = app_ui(),
    server = function(input, output, session) {
      app_server(input, output, session, config)
    },
    onStart = app_startup
  )

  # Run the app on default host and port
  runApp(app, host = host, port = port)
}
