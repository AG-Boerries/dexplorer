#' @title Configure a Python Virtual Environment for DExploreR
#'
#' @description
#' This function creates a python environment from R via `reticulate`. First, it checks if the environment (`"r-reticulate-dexplorer"`) for DExploreR already exists and if not creates a new one using Python 3.12 (or any other Python 3 version if 3.12 is not found). Then, it installs the required Python packages for DExploreR in that environment (`"plotly==6.5.2"`, `"numpy==2.4.2"`, `"kaleido==0.2.1"`, `"ipykernel==7.2.0"`). Finally, it returns the path to the created (or existing) virtual environment.
#'
#' @param env_name Name of the virtual environment. Default is `"r-reticulate-dexplorer"`.
#'
#' @param env_path Path to the directory where the virtual environment will be created. Default is `"~/.virtualenvs"`.
#'
#' @return The full path to the Python virtual environment as a character string.
#'
#' @export
configurePyEnv <- function(
  env_name = "r-reticulate-dexplorer",
  env_path = "~/.virtualenvs"
) {
  # Create the full path to the environment
  env_path_full <- file.path(path.expand(env_path), env_name)
  env_exists <- dir.exists(file.path(env_path_full, "bin"))

  # This assumes the default environment contains the correct packages
  if (env_exists) {
    message(
      "Using existing virtual python environment '",
      env_name,
      "' at ",
      env_path_full,
      " ...\n"
    )
    return(env_path_full)
  }

  # Find python 3.12 or 3
  python_path <- Sys.which("python3.12")
  if (python_path == "") {
    warning(
      "Python 3.12 not found. This package was developed and tested with Python 3.12; using another version may lead to unexpected behavior."
    )
    python_path <- Sys.which("python3")
    if (python_path == "") {
      stop(
        "No Python installation found. Please install Python 3.12 to use DExploreR."
      )
    }
  }
  # Create the virtual environment
  virtualenv_create(env_path, python = python_path)

  # Name and version of required python packages
  pkgs_to_install <- c(
    "plotly==6.5.2",
    "numpy==2.4.2",
    "kaleido==0.2.1",
    "ipykernel==7.2.0"
  )

  # Install required python packages
  lapply(
    pkgs_to_install,
    function(pkg) {
      py_install(
        pkg,
        envname = env_name,
        pip = TRUE
      )
    }
  )

  message(
    "\n\nVirtual environment '",
    env_name,
    "' created at ",
    env_path_full,
    " with packages:",
    paste0("\n- ", pkgs_to_install, collapse = "")
  )

  return(env_path_full)
}
