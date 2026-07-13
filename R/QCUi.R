#' @title Quality Control Tab UI
#'
#' @description
#' Generates the user interface for the Quality Control tab in the DExploreR app. Provides controls for selecting plot type, color palette, and samples, as well as buttons for further information, data download, and plot download. Displays the selected plot using `plotly`. This has the same layout as \code{\link{makeSubTabContent}()}.
#'
#' @param id Character. The module namespace ID.
#'
#' @return A Shiny UI element (HTML tag list) for inclusion in the app UI.
#
tabContentUI <- function(id) {
  ns <- NS(id)
  div(
    class = "container",
    fluidRow(
      # ---- Select the plot ----
      column(
        width = 9,
        prettyRadioButtons(
          inputId = ns("select_plot_raw_counts"),
          label = "Select plot",
          status = "success",
          choices = c(
            "Number of reads",
            "Number of genes",
            "Read count distribution"
          ),
          shape = "round",
          bigger = TRUE,
          animation = "smooth",
          fill = TRUE,
          thick = TRUE
        )
      ),
      # ---- Plot controls ----
      column(
        width = 3,
        div(
          dropdownButton(
            inputId = ns("plot_settings"),
            right = FALSE,
            circle = FALSE,
            size = "lg",
            icon = icon("sliders"),
            div(
              # ---- Select color palette ----
              virtualSelectInput(
                inputId = ns("color_select"),
                label = "Select color palette:",
                # The color choices are defined in `controls_colors.R`
                choices = color_choices,
                selected = "App colors",
                search = TRUE,
                showSelectedOptionsFirst = TRUE,
                # Add custom renderers for the colors, which include images of the color scales
                labelRenderer = "colorsWithIconChoice",
                selectedLabelRenderer = "colorsWithIconSelected"
              ),
              # ---- Select samples ----
              virtualSelectInput(
                inputId = ns("sample_select"),
                label = "Select samples:",
                choices = c(),
                multiple = TRUE,
                search = TRUE,
                showSelectedOptionsFirst = TRUE
              )
            )
          ),
          class = "plot-controls-qc-server"
        ),
        # ---- Display further information ----
        actionButton(
          ns("further_info"),
          label = "Further information",
          class = ns("custom-button")
        ),
        div(
          # ---- Download data ----
          downloadButton(
            ns("download_data"),
            label = "Download data",
            class = ns("custom-button")
          ),
          class = "data-download-btn"
        ),
        # ---- Open modal to select plot format and download plot ----
        actionButton(
          ns("download_plot"),
          label = "Download plot",
          icon = icon("download"),
          width = "155px",
          class = ns("custom-button")
        )
      ),
      class = "head-row-qc-server mb-2"
    ),
    # ---- Plot panel ----
    fluidRow(
      column(
        width = 12,
        div(
          # Using auto height allows to
          plotlyOutput(ns("plot"), height = "auto", width = "98%"),
          class = "plot-loader-wrap plot-loader-wrap-dynamic panel_plot_box"
        )
      )
    )
  )
}
