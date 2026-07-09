# Load package functions
# library(devtools)
# load_all()

# ---- Test DExploreR after modifications ----
# Using two "bigger" datasets
# AG_Frew was created using the built-in functions
# ucare mouse organoids was created using the accompanying CoPilot SKILL
dir <- "~/Documents/Projects/07_DExploreR/test_data/"
runDExploreR(data = dir)

# ---- Manually load datasets for development ----
df1 <- readRDS("inst/extdata/data/GSE273186.rds")
df2 <- readRDS(
  "~/Documents/Projects/07_DExploreR/test_data/AG_Frew_RCC_mouse_model_2022.rds"
)
df3 <- readRDS(
  "~/Documents/Projects/07_DExploreR/test_data/ucare_mouse_organoid_dexplorer_dataset.rds"
)

# ---- Scree plot ----
createScreePlot(
  df3$VarianceExplained,
  pc_x = c("PC1", "PC2"),
  pc_y = "",
  # selected_palette = "App colors",
  standalone = TRUE
)
