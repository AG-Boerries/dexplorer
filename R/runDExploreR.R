#' @title Run DExploreR Shiny App
#'
#' @description
#' This functions runs the DExploreR Shiny app. It sets up a python environment, if not existing, using \code{\link{configurePyEnv}()}, mounts user specified assets and data directories and finally starts DExploreR using \code{\link[shiny]{runApp}()} on "0.0.0.0:1234". If running without any parameters, it uses the data and assets provided with this package form GSE273186.
#'
#' @param data A directory containing `.rds` files and corresponding `.csv`s as prepared by \code{\link{createDataSet}()}.
#'
#' @param asset_dir A directory containing the assets for DExploreR, such as logos. If you want your logo to appear on the home and as a loading icon, name it `logo.png` in `assed_dir`. Other assets are not yet specified.
#'
#' @param with_upload Boolean. Only needed when `data` is a path to a directory with `.rds` files. This uses mode "standard".
#'
#' @param env_path Path to the Python virtual environment to use for DExploreR. If `NULL`, the default environment created by \code{\link{configurePyEnv}()} will be used. If no environment exists, this will also create a new environment.
#'
#' @param host The host address to run the Shiny app on. Default is "0.0.0.0".
#'
#' @param port The port to run the Shiny app on. Default is 1234.
#'
#' @export
runDExploreR <- function(
  data = NULL,
  asset_dir = NULL,
  with_upload = FALSE,
  env_path = NULL,
  host = "0.0.0.0",
  port = 1234
) {
  # If no directories provided, use the default data and assets provided with this package
  if (is.null(asset_dir)) {
    asset_dir <- system.file("extdata", "assets", package = "dexplorer")
  }
  asset_dir <- normalizePath(asset_dir, mustWork = TRUE)

  # Check which type of data is provided
  if (is.null(data)) {
    # If nothing provided, DExploreR shall use package internal data
    # Mode "internal": read data from `.rds` and `.csv` but don't show `Home` and `Data sets` tabs
    mode <- "internal"
    # Get the path to the internal data of the package
    data <- system.file("extdata", "data", package = "dexplorer")
    data <- normalizePath(data, mustWork = TRUE)
  } else if (is.list(data)) {
    # If a list is provided, we assume the user prepared the data directly in R and passes all necessary data frames as a list
    # This does not requires any file reading
    # The `.rds` files actually contain nothing more
    # Ideally, this list of data frames was created using `dexplorer::createDataSet()`
    mode <- "interactive"
  } else if (is.character(data)) {
    # When a string is provided, we assume it is a directory containing `.rds` and correspinding `.csv` files
    mode <- "standard"
    data <- normalizePath(data, mustWork = TRUE)
  }

  # Data is a path, when either a path is provided or `data == NULL`
  # Then, normalize path because we read from it
  # if (is.character(data)) {
  #   data <- normalizePath(data, mustWork = TRUE)
  # }
  # if (is.null(data_dir)) {
  #   # If using the data provided with the package, set mode to internal
  #   mode <- "internal"
  #   data_dir <- system.file("extdata", "data", package = "dexplorer")
  # }

  # Create a config list to pass to the server function
  config <- list(
    # Data can be a path or a list
    data = data,
    mode = mode,
    with_upload = with_upload
  )

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

  # Create the Shiny app
  app <- shinyApp(
    ui = app_ui(config = config),
    server = function(input, output, session) {
      app_server(input, output, session, config)
    },
    # Currently, sets only some ggplot defaults
    onStart = app_startup
  )

  # Run the app on default host and port
  runApp(app, host = host, port = port)
}
