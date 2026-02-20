data_download <- function(name, data, authors, ...) {
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
