download_settings_modal <- function(id) {
  tagList(
    virtualSelectInput(
      inputId = paste0("plot_format_", id),
      label = "Select file format:",
      choices = c("png", "jpeg", "svg", "webp", "pdf"),
      selected = "png"
    ),
    numericInput(
      inputId = paste0("plot_height_", id),
      label = "Height (in px):",
      value = 720,
      min = 1,
      max = 100000
    ),
    numericInput(
      inputId = paste0("plot_width_", id),
      label = "Width (in px):",
      value = 1280,
      min = 1,
      max = 100000
    ),
    downloadButton(
      outputId = paste0(
        "download_plot_",
        ifelse(str_detect(id, "_modal"), sub("_modal", "", id), id),
        "_modal"
      ),
      label = "Download plot",
      class = "custom-button"
    )
  )
}
