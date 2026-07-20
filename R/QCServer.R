#' @title Quality Control Tab Server Logic
#'
#' @description
#' Implements the server-side logic for the Quality Control tab in the DExploreR app. Handles plot selection, color palette and sample selection, data filtering, plot generation, download handlers, and modal dialogs for further information and plot downloads. Integrates with `plotly` for interactive visualization and dynamically adjusts plot layout and tooltips.
#'
#' @param id Character. The module namespace ID.
#'
#' @param data A reactive expression returning a list of data frames required for plotting. This is `data_set_loaded()` in \code{\link{app_server}()}.
#'
#' @param plot_status A reactiveValues object for tracking plot readiness.
#'
#' @param authors Character. Author information for use in download file names.
#'
tabContentServer <- function(
  id,
  data,
  plot_status,
  authors
) {
  # Define variables locally for R CMD check
  SampleNameUser <- Group <- domain <- NULL

  moduleServer(id, function(input, output, session) {
    # Get the namespace
    ns <- session$ns

    # ---- Get the data for the requested plot ----
    needed_data <- reactive({
      req(data())

      # Based on the selected plot different data is required
      if (input$select_plot_raw_counts == "Read count distribution") {
        # For the read count distributions the raw counts are required
        data()[["RawCounts"]] |>
          filter(SampleNameUser %in% input$sample_select)
      } else {
        # Filter for selected samples but keep the "All samples" group
        # This contains the total number of detected genes
        data()[["QualityControl"]] |>
          filter(SampleNameUser %in% c(input$sample_select, "All samples"))
      }
    })

    # ---- Create the plot ----
    qc_plot <- reactive({
      req(needed_data())

      # Extract function for plotting from lookup table
      plotter <- assign_format_plot_info()[[input$select_plot_raw_counts]]$plot

      if (input$select_plot_raw_counts == "Read count distribution") {
        p <- plotter(needed_data())

        # This plot uses a fixed color scale with 4 colors for the quartiles
        # The aesthetic is `factor(after_stat(quantile))`, which is not a column in the data
        # Thus, this needs to be specified manually
        plot_components <- data.frame(
          aes = "fill",
          aes_name = NA_character_,
          aes_cont = FALSE,
          aes_n = 4
        )

        p <- add_selected_colors(
          p = p,
          selected_palette = input$color_select,
          color_by = plot_components
        )
      } else {
        p <- plotter(needed_data())

        # Add the selected color scale to the plot
        p <- add_selected_colors(p = p, selected_palette = input$color_select)
      }

      # ---- Calculate plot height ----
      p_height <- calculatePlotHeight(
        n_samples = length(unique(needed_data()$SampleNameUser))
      )

      # ---- Convert ggplot to plotly ----
      p <- ggplotly(
        p,
        tooltip = "text",
        height = p_height
      ) |>
        layout(
          # Plotly overwrites legend setting of ggplot
          # Place legend at the top of the plot
          # This is only relative positioning and will be screwed up when the plot is large
          # Below the position is dynamically adjusted
          legend = list(
            orientation = "h",
            yref = "paper",
            yanchor = "bottom",
            y = 1.0,
            xanchor = "center",
            xref = "paper",
            x = 0.5
          )
        ) |>
        # Reduce the modebar to only essential tools
        config(
          displaylogo = FALSE,
          modeBarButtons = list(
            list("toImage"),
            list("zoom2d"),
            list("pan2d"),
            list("resetScale2d")
          )
        ) |>
        # Attach custom tooltip
        onRender(
          "
          function(el, x, tooltipType) {
            enableCustomTooltip(el, tooltipType);
          }
          ",
          data = list(tooltipType = "raw_data")
        )

      # ---- Manual plot stylings ----
      # ---- Remove default tooltip ----
      for (i in seq_along(p$x$data)) {
        p$x$data[[i]]$hoverinfo <- "none"
      }

      # ---- Adjust the y-axis domain for each facet ----
      # This ensures that the bars all have the same height rather than the facets having the same height

      # Identify the number of y-axis in the plot
      # Usually it is one per facet
      y_axis <- na.omit(str_extract(names(p$x$layout), "^yaxis.*"))

      if (length(y_axis) > 1) {
        df <- lapply(y_axis, function(yax) {
          # Identify mapped samples
          samples <- p$x$layout[[yax]]$ticktext
          # Use samples to identify group
          group <- needed_data() |>
            filter(SampleNameUser %in% samples) |>
            pull(Group) |>
            unique()

          # Save as dataframe
          data.frame(yaxis = yax, n_samples = length(samples), group = group)
        }) |>
          bind_rows() |>
          # Calculate the start and end points of each domain by considering the total height
          calculateDomains(total_height = p_height) |>
          # For the facet labels (annotations), use the upper y-value of the domain as position
          mutate(annotation = sapply(domain, function(x) x[2]))

        # Assign domains to y-axis
        for (i in seq_along(p$x$layout)) {
          if (str_detect(names(p$x$layout[i]), "yaxis")) {
            y <- names(p$x$layout[i])
            p$x$layout[[y]]$domain <- base::unlist(df[df$yaxis == y, ]$domain)
            # Set the automargin to TRUE to ensure that the y-axis tick labels are not cut off
            p$x$layout[[y]]$automargin <- TRUE
          }
        }

        for (i in seq_along(p$x$layout$annotations)) {
          ann <- p$x$layout$annotations[[i]]
          # Adjust the y-position of the facet labels (i.e. annotations)
          # This assumes that the facet labels do not have an `annotationType`
          if (is.null(ann$annotationType)) {
            # Extract the group name, which is equal to the facet label
            annotation <- ann$text
            # Use the group name to identify the y-position
            p$x$layout$annotations[[i]]$y <- df[
              df$group == annotation,
            ]$annotation
          }

          # Adjust the x-position of the y-axis title based on the longest sample name
          if (!is.null(ann$text) && ann$text == "Sample name") {
            p$x$layout$annotations[[i]]$xshift <- -max(
              nchar(unique(needed_data()$SampleNameUser)),
              na.rm = TRUE
            ) *
              7 -
              10
          }
        }
      }

      # ---- Adjust the y-position of the legend ----
      # The factor 25 px was determined empirically
      y_legend_pos <- 25 / p_height + 1
      p$x$layoutAttrs[[1]]$legend$y <- y_legend_pos

      return(p)
    })

    # ---- Observe when the plot is ready and update the plot status ----
    observe({
      req(qc_plot())
      plot_status$qc_plot <- TRUE
      message(
        "[Raw data][Quality Control] Plot ",
        input$select_plot_raw_counts,
        " is ready."
      )
    })

    # ---- Render the plot ----
    output$plot <- renderPlotly({
      qc_plot()
    })

    # ---- Download data ----
    observeEvent(
      input$select_plot_raw_counts,
      {
        output$download_data <- dataDownload(
          name = gsub(" ", "_", input$select_plot_raw_counts),
          data = needed_data(),
          authors = authors
        )
      }
    )

    # ---- Display the further information as modal if requested ----
    observeEvent(input$further_info, {
      req(input$select_plot_raw_counts)

      showModal(modalDialog(
        title = "Further information",
        easyClose = TRUE,
        footer = NULL,
        assign_format_plot_info()[[input$select_plot_raw_counts]]$info
      ))
    })

    # ---- Download plot ----
    # Show modal with download option upon clicking the corresponding `actionButton()`
    # This is the part that requires the python environment
    observeEvent(input$download_plot, {
      req(input$select_plot_raw_counts)

      showModal(modalDialog(
        title = "Download plot",
        easyClose = TRUE,
        footer = NULL,

        # ---- Selectplot format ----
        virtualSelectInput(
          inputId = ns("plot_format"),
          label = "Select file format:",
          choices = c("png", "jpeg", "svg", "webp", "pdf"),
          selected = "png"
        ),

        # ---- Select plot height and width ----
        numericInput(
          inputId = ns("plot_height"),
          label = "Height (in px):",
          value = 720,
          min = 1,
          max = 100000
        ),
        numericInput(
          inputId = ns("plot_width"),
          label = "Width (in px):",
          value = 1280,
          min = 1,
          max = 100000
        ),

        # ---- Download button ----
        downloadButton(
          outputId = ns("download_plot_modal"),
          label = "Download plot",
          class = ns("custom-button")
        )
      ))
    })

    # ---- Download handler for plot downloads from the modal ----
    output$download_plot_modal <- downloadHandler(
      filename = function() {
        # Create the name of the downloaded file similar to the data downloads
        paste0(
          gsub(" ", "_", input$select_plot_raw_counts),
          "_plot_",
          authors,
          "_",
          Sys.Date(),
          ".",
          input$plot_format
        )
      },
      content = function(file) {
        # Use `save_image()` from plotly to save the plot in the desired format
        # This depends on python, thus, usage of `reticulate` and `kaleido`
        save_image(
          p = qc_plot(),
          file = file,
          width = input$plot_width,
          height = input$plot_height
        )
      }
    )
  })
}
