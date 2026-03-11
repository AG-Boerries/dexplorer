# This runs DExploreR with the internal data set provided with the package
runDExploreR()

# This runs DExploreR with a custom data set, meeting the apps's data formatting requirements
d <- readRDS(paste0(
  system.file("extdata", "data", package = "dexplorer"),
  "/GSE273186.rds"
))
runDExploreR(data = d)


# This runs DExploreR with a directory containing `.rds` and `.csv` files to select from different data sets
data_dir <- system.file("extdata", "data", package = "dexplorer")
data_dir <- system.file("extdata", "data", package = "dexplorer")
runDExploreR(data = data_dir, asset_dir = asset_dir)

# Contains more complex data set
# asset_dir <- "~/Downloads"
# data_dir <- "~/Downloads/data"

# This runs DExploreR with a directory containing `.rds` and `.csv` files to select from different data sets, and with the upload functionality enabled, which allows users to upload their own data sets in the app
runDExploreR(
  data = data_dir,
  asset_dir = asset_dir,
  with_upload = TRUE
)
