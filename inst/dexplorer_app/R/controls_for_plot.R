# Function to create control element for plots
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
