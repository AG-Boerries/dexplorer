# This is the lookup table for the plotting functions and the info texts
assign_format_plot_info <- list(
  "Number of reads" = list(
    plot = read_count_plot,
    info = tagList(
      tags$b("Assigned reads:"),
      "Number of mapped reads that could be assigned unambiguously to an annotated genomic region.",
      br(),
      br(),
      tags$b("Unassigned / mapped reads:"),
      "Number of reads that could be mapped to the reference but without unique assignement, for instance, because of overlapping annotated genomic regions or no available annotation.",
      br(),
      br(),
      tags$b("Unassigned / unmapped reads:"),
      "Number of reads that did not map to the reference or that mapped to many locations.",
      br(),
      br(),
      "In the case of paired-end sequencing, read pairs are counted instead of single reads."
    )
  ),
  "Number of genes" = list(
    plot = gene_count_plot,
    info = HTML(
      "To be considered as detected, a gene must have at least one read assigned, i.e. at least one count. The bar <i>All samples</i> depicts the overall number of distinct detected genes across all samples."
    )
  ),
  "Read count distribution" = list(
    plot = read_count_distribution_plot,
    info = "The read count distribution shows how the reads are distributed across all recorded genes in a sample. The vertical lines indicate the quartiles separating the distribution into equal proportions with 25 % of the data. Then central line indicates the median read counts per gene."
  )
)

# This creates the UI with the same layout as the other tabs
# The other tabs use `makeSubTabContent()`, but here I could not find a way to do so
tabContentUI <- function(id) {
  ns <- NS(id)
  div(
    fluidRow(
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
      # This is the same as plotControls
      # I could not find a solution with the namespacing issues
      # Thus this is repeated here
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
              # Color selection
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
          style = "display: flex; justify-content: flex-end; width: 100%;"
        ),
        actionButton(
          ns("further_info"),
          label = "Further information",
          class = ns("custom-button")
        ),
        div(
          downloadButton(
            ns("download_data"),
            label = "Download data",
            class = ns("custom-button")
          ),
          style = "width: 155px; margin-left: auto;"
        ),
        actionButton(
          ns("download_plot"),
          label = "Download plot",
          icon = icon("download"),
          width = "155px",
          class = ns("custom-button")
        ),
        div(style = "margin-bottom: 10px;"),
      ),
      style = "display: flex; align-items: center; margin-top: 30px; margin-bottom: 10px; margin-right: 10px; height: 220px"
    ),
    fluidRow(
      column(
        width = 12,
        div(
          # Using auto height allows to
          plotlyOutput(ns("plot"), height = "auto", width = "98%"),
          class = "panel_plot_box"
        )
      )
    )
  )
}


# Create the module server, which requires `data` from the parent server
tabContentServer <- function(
  id,
  data,
  plot_status,
  authors
) {
  moduleServer(id, function(input, output, session) {
    # Get the namespace for inputs generated on the server site, like the ones in the download dialog
    ns <- session$ns

    # Identify the data needed based on the selected plot
    needed_data <- reactive({
      req(data())

      # Based on the selected plot different data is required
      if (input$select_plot_raw_counts == "Read count distribution") {
        # For the read count distributions the raw counts are required
        d <- data()[["RawCounts"]] %>%
          filter(SampleNameUser %in% input$sample_select)
      } else {
        # Filter for selected samples but keep the "All samples" group
        # This contains the total number of detected genes
        d <- data()[["QualityControl"]] %>%
          filter(SampleNameUser %in% c(input$sample_select, "All samples"))
      }
      return(d)
    })

    # Create the plot
    bar_plot <- reactive({
      req(needed_data())

      # Extract function for plotting form lookup table
      plotter <- assign_format_plot_info[[input$select_plot_raw_counts]]$plot
      # Based on the selected plot different data is required
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

      # Turn plot in to ggplotly and adjust tooltip color and legend position
      p <- ggplotly(
        p,
        tooltip = "text",
        # Adjust the plot heigth based on the number of samples
        height = plot_height(
          n_samples = length(unique(needed_data()$SampleNameUser))
        )
      ) %>%
        layout(
          # Plotly overwrites legend setting of ggplot
          # Bottom position of legend requries extra space to avoid overplotting with x-axis title
          legend = list(
            orientation = "h",
            y = 1.1,
            xanchor = "center",
            xref = "paper",
            x = 0.5
          ),
          yaxis = list(
            title = list(standoff = 20)
          )
        ) %>%
        # Reduce the modebar to only essential tools
        config(
          displaylogo = FALSE,
          modeBarButtons = list(
            list("toImage"),
            list("zoom2d"),
            list("pan2d"),
            list("resetScale2d")
          )
        ) %>%
        # Attach custom tooltip
        onRender(
          "
        function(el, x, tooltipType) {
          enableCustomTooltip(el, tooltipType);
        }
      ",
          data = list(tooltipType = "raw_data")
        )

      for (i in seq_along(p$x$data)) {
        # Remove default tooltip
        p$x$data[[i]]$hoverinfo <- "none"
      }

      # Get the total height of the plot, which is defined by the number of samples
      # In this case, not `input$sample_select`
      # The gene counts plot always contains the "All samples" group
      total_height <- plot_height(
        n_samples = length(unique(needed_data()$SampleNameUser))
      )

      # Identify the number of y-axis in the plot, usually it is one per facet
      y_axis <- na.omit(str_extract(names(p$x$layout), "^yaxis.*"))

      if (length(y_axis) > 1) {
        df <- lapply(y_axis, function(yax) {
          # Identify mapped samples
          samples <- p$x$layout[[yax]]$ticktext
          # Use samples to identify group
          # There must be a better way to extract the group name from `p`, too
          group <- needed_data() %>%
            filter(SampleNameUser %in% samples) %>%
            pull(Group) %>%
            unique()

          # Save as dataframe
          data.frame(yaxis = yax, n_samples = length(samples), group = group)
        }) %>%
          bind_rows() %>%
          # Calculate the start and end points of each domain by considering the total height
          calculate_domains(total_height = total_height) %>%
          # For the facet labels (annotations), use the upper y-value of the domain as position
          mutate(annotation = sapply(domain, function(x) x[2]))

        # Assign domains to y-axis
        for (i in seq_along(p$x$layout)) {
          if (str_detect(names(p$x$layout[i]), "yaxis")) {
            y <- names(p$x$layout[i])
            p$x$layout[[y]]$domain <- base::unlist(df[df$yaxis == y, ]$domain)
          }
        }

        # Then adjust the y-position of the facet labels (e.g. annotations)
        for (i in seq_along(p$x$layout$annotations)) {
          # Let's how stable this is ...
          # It assumes that the facet labels do not have an `annotationType`
          if (is.null(p$x$layout$annotations[[i]]$annotationType)) {
            # Extract the group name, which is equal to the facet label
            annotation <- p$x$layout$annotations[[i]]$text
            # Use the group name to identify the y-position
            p$x$layout$annotations[[i]]$y <- df[
              df$group == annotation,
            ]$annotation
          }
        }
      }

      return(p)
    })

    # Set plot status to TRUE when done
    observe({
      req(bar_plot())
      plot_status$bar_plot <- TRUE
      message(
        "[Raw data][Quality Control] Plot ",
        input$select_plot_raw_counts,
        " is ready."
      )
    })

    output$plot <- renderPlotly({
      bar_plot()
    })

    observeEvent(
      input$select_plot_raw_counts,
      {
        output$download_data <- data_download(
          name = gsub(" ", "_", input$select_plot_raw_counts),
          data = needed_data(),
          authors = authors
        )
      }
    )

    # Also the info text come from the lookup and are displayed as modals
    observeEvent(input$further_info, {
      req(input$select_plot_raw_counts)
      showModal(modalDialog(
        title = "Further information",
        easyClose = TRUE,
        footer = NULL,
        assign_format_plot_info[[input$select_plot_raw_counts]]$info
      ))
    })
    # Show modal with download option upon clicking the corresponding `actionButton()`
    observeEvent(input$download_plot, {
      req(input$select_plot_raw_counts)
      showModal(modalDialog(
        title = "Download plot",
        easyClose = TRUE,
        footer = NULL,
        virtualSelectInput(
          inputId = ns("plot_format"),
          label = "Select file format:",
          choices = c("png", "jpeg", "svg", "webp", "pdf"),
          selected = "png"
        ),
        # Adjust downloaded plot size and format
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
        downloadButton(
          outputId = ns("download_plot_modal"),
          label = "Download plot",
          class = ns("custom-button")
        )
      ))
    })

    # Download handler for plot downloads from the modal
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
          p = bar_plot(),
          file = file,
          width = input$plot_width,
          height = input$plot_height
        )
      }
    )
  })
}
