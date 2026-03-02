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
asset_dir <- system.file("extdata", "assets", package = "dexplorer")
runDExploreR(data = data_dir, asset_dir = asset_dir)

# TODO: this logic needs to be impemented
runDExploreR(
  data = data_dir,
  asset_dir = asset_dir,
  with_upload = TRUE
)
