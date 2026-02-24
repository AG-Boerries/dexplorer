# Create the content for each sub tab
# This ensures a consistent layout
makeSubTabContent <- function(
  # `id` is used for the `actionButton()`, which shows further information
  # and for the `plotControls()`
  id,
  # This is passed to `plotControls(...)`, so that additional controls can be added
  further_controls = div(),
  # This is the wide element on the top left, which is at the moment not used
  top_left_wide = div(),
  # This is the main content, i.e. the plots or tables
  main_content = div(),
  # Remove sample selection from `plotControls()`, might be useful for tabs looking a the groups
  remove_sample_selection = FALSE,
  # In case we need an additional button, this is the place
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
