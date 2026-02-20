# Set up of python for `save_image()` of plotly
library(reticulate)

# Use system python installation
use_python("/usr/bin/python3", required = TRUE)

# Install required python packages in the r-reticulate virtual environment
py_install(
  packages = c("plotly", "numpy"),
  envname = "r-reticulate",
  pip = TRUE
)

# Latest version of kaleido does not work, because scopes are missing?!
py_install(
  "kaleido==0.2.1",
  envname = "r-reticulate",
  pip = TRUE
)
