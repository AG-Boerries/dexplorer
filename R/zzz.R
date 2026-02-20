.onLoad <- function(libname, pkgname) {
  # Python setup is required for DExploreR
  # `plotly::save_image()` requires the kaleido python package to save plots in different formats
  library(reticulate)

  # Find path to python3 and use it, if it exists
  python_path <- Sys.which("python3")
  if (python_path == "") {
    stop(
      "No python installation found, please install python to use DExploreR."
    )
  }
  packageStartupMessage(
    "Using this python installation: ",
    python_path,
    " ...\n"
  )
  use_python(python_path, required = TRUE)

  # Install required python packages with defined versions in the `r-reticulate-dexplorer` virtual environment
  suppressMessages(
    py_install(
      "plotly==6.5.2",
      envname = "r-reticulate-dexplorer",
      pip = TRUE,
      pip_options = list("--quiet")
    )
  )
  packageStartupMessage("... to install plotly v6.5.2\n")

  suppressMessages(
    py_install(
      "numpy==2.4.2",
      envname = "r-reticulate-dexplorer",
      pip = TRUE,
      quite = TRUE,
      pip_options = list("--quiet")
    )
  )
  packageStartupMessage("... to install numpy v2.4.2\n")

  suppressMessages(
    # kaleido 0.2.1 proved to work for this purpose
    py_install(
      "kaleido==0.2.1",
      envname = "r-reticulate-dexplorer",
      pip = TRUE,
      pip_options = list("--quiet")
    )
  )
  packageStartupMessage("... to install kaleido v0.2.1\n")

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
