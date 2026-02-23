# Load python environment
# library(reticulate)
# use_virtualenv("r-reticulate-dexplorer", required = TRUE)
# This ensures that the python is used
# py_config()

# Data handling
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
library(reshape2)
suppressPackageStartupMessages(library(tidyr))
library(tibble)
library(stringr)
library(tidytext)

# Plotting
library(ggplot2)
library(ggridges)
library(ComplexUpset)
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(circlize))
suppressPackageStartupMessages(library(heatmaply))
suppressPackageStartupMessages(library(scales))
# suppressPackageStartupMessages(library(viridis))
# library(wesanderson)
# library(RColorBrewer)

# Shiny
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(DT))
library(waiter)
library(htmlwidgets)
library(htmltools)

# Code structure
library(conflicted)

# Resolve conflicts
suppressMessages(conflict_prefer("dataTableOutput", "DT"))
suppressMessages(conflict_prefer("renderTableOutput", "DT"))
suppressMessages(conflict_prefer("layout", "plotly"))
suppressMessages(conflict_prefer("filter", "dplyr"))
suppressMessages(conflict_prefer("rename", "dplyr"))

# General ggplot settings
theme_set(
  theme_minimal() +
    theme(
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.margin = margin(l = 20, b = 10),
      strip.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    )
)

# Suppress warnings from `geom_point()` because using the aestetic `text`,
# which is required for the `ggplotly()` tooltip, is unknown to `geom_point()`
geom_point_quiet <- function(...) {
  suppressWarnings(ggplot2::geom_point(...))
}
