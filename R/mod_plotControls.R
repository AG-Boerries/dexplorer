#' @title Generate Plot Controls UI
#'
#' @description
#' Creates a set of user interface controls for customizing plots in the DExploreR app. Includes inputs for selecting samples, color palettes, plot settings, and additional controls as needed for specific plots or tabs.
#'
#' @param id Character. The namespace ID, used for namespacing UI elements.
#'
#' @param ... UI element(s) to be included as additional controls in the plot controls section.
#'
#' @param remove_sample_selection Logical. If TRUE, removes the sample selection control from the plot controls. Default is FALSE.
#'
#' @return A Shiny UI element (HTML tag list) representing the plot controls.
#'
plotControls <- function(id, remove_sample_selection = FALSE, ...) {
  div(
    dropdownButton(
      inputId = paste0("plot_settings_", id),
      right = FALSE,
      circle = FALSE,
      size = "lg",
      icon = icon("sliders"),
      div(
        # Color selection
        virtualSelectInput(
          inputId = paste0("color_select_", id),
          label = "Select color palette:",
          choices = color_choices,
          selected = "App colors",
          search = TRUE,
          showSelectedOptionsFirst = TRUE,
          # Add custom renderers for the colors, which include images of the color scales
          labelRenderer = "colorsWithIconChoice",
          selectedLabelRenderer = "colorsWithIconSelected"
        ),
        # In some cases we might not want to have sample selection
        if (remove_sample_selection) {
          div()
        } else {
          # Sample selection
          virtualSelectInput(
            inputId = paste0("sample_select_", id),
            label = "Select samples:",
            choices = c(),
            multiple = TRUE,
            search = TRUE,
            showSelectedOptionsFirst = TRUE
          )
        },
        # This allows to pass additional UI input elements to the `actionButton()`
        ...,
      )
    ),
    style = "display: flex; justify-content: flex-end; width: 100%;"
  )
}
