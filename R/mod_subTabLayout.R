#' @title Create Consistent Sub Tab Layout
#'
#' @description
#' Generates a consistent UI layout for sub-tabs in the DExploreR app, including plot controls, information and download buttons, and a main content area for plots or tables. Allows insertion of additional controls and buttons as needed for specific tabs.
#'
#' @param id Character. The namespace ID, used for namespacing UI elements. This is passed on to \code{\link{plotControls}()} to ensure that the controls are properly namespaced, too.
#'
#' @param further_controls UI element(s) to be included as additional controls in \code{\link{plotControls}()}. Default is `div()`.
#'
#' @param top_left_wide UI element to be displayed in the top left wide area. Default is `div()`.
#'
#' @param main_content UI element(s) for the main content area (e.g., plots or tables). Default is `div()`.
#'
#' @param remove_sample_selection Logical. If TRUE, removes the sample selection control from \code{\link{plotControls}()}. Default is FALSE.
#'
#' @param remove_color_selection Logical. If TRUE, removes the color selection control from \code{\link{plotControls}()}. Default is FALSE.
#'
#' @param additional_button_right UI element for an additional button to be placed on the right. Default is `div()`.
#'
#' @return A Shiny UI element (HTML tag list) representing the sub-tab layout.
#'
makeSubTabContent <- function(
  id,
  further_controls = div(),
  top_left_wide = div(),
  main_content = div(),
  remove_sample_selection = FALSE,
  remove_color_selection = FALSE,
  additional_button_right = div()
) {
  div(
    class = "container",
    fluidRow(
      column(
        width = 9,
        top_left_wide
      ),
      column(
        width = 3,
        div(
          plotControls(
            id,
            remove_sample_selection,
            remove_color_selection,
            further_controls
          ),
          actionButton(
            paste0("info_", id),
            label = "Further information",
            class = "custom-button"
          ),
          div(
            downloadButton(
              paste0("download_data_", id),
              label = "Download data",
              class = "custom-button"
            ),
            style = "width: 155px; margin-left: auto;"
          ),
          actionButton(
            paste0("download_plot_", id),
            label = "Download plot",
            class = "custom-button",
            icon = icon("download"),
            width = "155px"
          ),
          div(additional_button_right),
        )
      ),
      class = "tab-head-row mb-2"
    ),
    fluidRow(
      column(
        width = 12,
        div(
          main_content,
          class = "plot-loader-wrap panel_plot_box"
        )
      )
    )
  )
}

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
