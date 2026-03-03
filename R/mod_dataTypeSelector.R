#' @title Data Type Selector for DExploreR
#'
#' @description
#' This function checks the provided data and reads files accordingly.
#'
#' @param config A list containing the configuration for the data loading, including the mode of operation and the data provided. This parameter is created during initialization of the Shiny app in \code{\link{runDExploreR}()}
#'
#' @return A list containing:
#'
#' * `dataSetsTable`: A data frame containing the meta data of the available data sets, which is rendered in the `Data sets` tab for selection, when `mode == "standard"`. When `mode == "internal"`, contained information is only shown in respective tabs of the app, while in `mode == "interactive"`, this is `NULL` as no selection is necessary.
#'
#' * `dataSet`: A list containing the data frames of the selected data set, which is used for all downstream analyses. This is `NULL`, when `dataSetsTable` is not `NULL`, thus, when `mode == "standard"` and no data set is selected yet. When `mode == "internal"` or `mode == "interactive"`, this contains the data frames of the internal or user-provided data set, respectively.
#'
dataTypeSelector <- function(config) {
  # If the user provided a directory (or if `data` in `runDExploreR()` is `NUll`, which sets a path to the internal data), then read the available meta data
  if (is.character(config$data)) {
    metaFiles <- list.files(config$data, pattern = "\\.csv$", full.names = TRUE)
    dataSetsTable <- lapply(metaFiles, read.csv, check.names = FALSE) %>%
      bind_rows()

    # When mode is "internal", there is only one data set, thus, we can directly load the data set and skip the data set selection step
    if (config$mode == "internal") {
      dataLoaded <- readRDS(list.files(
        config$data,
        pattern = "\\.rds$",
        full.names = TRUE
      )[1])
    } else {
      dataLoaded <- NULL
    }
    # When the user provided a list, we assume that this list already contains the data frames of the data set, thus, we can directly load the data set and skip the data set selection step
  } else if (is.list(config$data)) {
    dataSetsTable <- NULL
    dataLoaded <- config$data
  }

  list(
    dataSetsTable = dataSetsTable,
    dataSet = dataLoaded
  )
}
