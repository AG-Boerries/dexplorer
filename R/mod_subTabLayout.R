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
  additional_button_right = div()
) {
  div(
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
          div(additional_button_right, style = "margin-bottom: 10px;"),
        )
      ),
      style = "display: flex; align-items: center; margin-top: 30px; margin-bottom: 10px; margin-right: 10px; height: 220px"
    ),
    fluidRow(
      column(
        width = 12,
        div(
          main_content,
          class = "panel_plot_box"
        )
      )
    )
  )
}
