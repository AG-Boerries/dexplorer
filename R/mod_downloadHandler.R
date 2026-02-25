#' @title Data Download Handler
#'
#' @description
#' Creates a Shiny download handler for exporting data frames as CSV files with a customizable filename. The filename includes the provided name, additional identifiers, author information, and the current date.
#'
#' @param name Character. The base name for the downloaded file.
#'
#' @param data A data frame to be written to CSV.
#'
#' @param authors Character. Author information to include in the filename.
#'
#' @param ... Additional character values to include in the filename (e.g., contrast, gene set).
#'
#' @return A Shiny downloadHandler object for use in UI download buttons.
#'
dataDownload <- function(name, data, authors, ...) {
  downloadHandler(
    filename = function() {
      paste0(
        name,
        "_",
        ...,
        authors,
        "_",
        Sys.Date(),
        ".csv"
      )
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
}
