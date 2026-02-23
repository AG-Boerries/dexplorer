.onLoad <- function(libname, pkgname) {
  # Python setup is required for DExploreR
  # `plotly::save_image()` requires the kaleido python package to save plots in different formats

  # Find path to python and use it, if it exists
  python_path <- Sys.which("python3.12")
  if (python_path == "") {
    warning(
      "Python 3.12 not found. This package was developed and tested with Python 3.12, so using another version may lead to unexpected behavior."
    )
    python_path <- Sys.which("python3")
    if (python_path == "") {
      stop(
        "No python installation found, please install python 3.12 to use DExploreR."
      )
    }
  }
  packageStartupMessage(
    "Using this python installation: ",
    python_path,
    " ...\n"
  )

  # Create virtual python environment
  env_path <- "~/.virtualenvs/r-reticulate-dexplorer"
  # This will no recreate the environment if it already exists
  virtualenv_create(env_path, python = python_path)

  # Activate the virtual environment
  use_virtualenv(
    virtualenv = "r-reticulate-dexplorer",
    required = TRUE
  )

  # Install required packages if missing
  pkgs <- c("plotly==6.5.2", "numpy==2.4.2", "kaleido==0.2.1")
  for (pkg in pkgs) {
    suppressMessages(
      py_install(
        pkg,
        envname = "r-reticulate-dexplorer",
        pip = TRUE,
        pip_options = list("--quiet")
      )
    )
    packageStartupMessage(paste0("... to install ", pkg, "\n"))
  }

  packageStartupMessage(
    "\nPython environment setup complete!\n\n"
  )
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "########################################################################################\n",
    "#### Welcome to 'DExploreR'! Visualize and interactively explore bulk RNA-seq data. ####\n",
    "########################################################################################\n"
  )
}
