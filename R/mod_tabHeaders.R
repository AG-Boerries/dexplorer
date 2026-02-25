#' @title Generate Tab Headers
#'
#' @description
#' Creates a header layout for a tab in the DExploreR app, including a title panel and a text output area. Used to provide a consistent header with a title and descriptive text for each tab.
#'
#' @param title Character. The title to display in the tab header and browser window.
#'
#' @param text_id Character. The output ID for the descriptive text to be rendered below the title.
#'
#' @return A Shiny UI element (HTML tag list) representing the tab header.
#'
tabHeaders <- function(title, text_id) {
  fluidPage(
    div(
      titlePanel(title = title, windowTitle = title),
      textOutput(text_id),
      class = "tab-header"
    )
  )
}
