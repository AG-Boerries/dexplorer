#' @title Create Top Gene Sets Dotplot
#'
#' @description
#' Generates a dotplot of top gene sets for selected contrasts in gene set enrichment analysis (GSEA) using `ggplot2` or, if `standalone = FALSE`, using `plotly` for interactivity. The plot displays enrichment scores, set sizes, and direction of regulation, with custom tooltips, facetting by contrast, and links to gene set descriptions. This plot is registered for click events, which open a modal showing a volcano plot (\code{\link{createVolcanoPlot}()}) and a heatmap (\code{\link{createGeneExpressionHeatmap}()}) of the selected gene set.
#'
#' @param df A data frame containing gene set enrichment results, including enrichment scores, set sizes, direction,contrast labels and a column with the genes of the gene set as a nested tibble.
#'
#' @param selected_palette Character. The name of the color palette to use for coloring gene sets.
#'
#' @param standalone Logical. If `TRUE`, the PCA plot is generated as a standalone plot, which is not interactive. If `FALSE` (required inside DExploreR), the plot is interactive via `plotly`. Defaults to `FALSE`.
#'
#' @param color_scale_order Logical. Whether to use the standard order of colors (TRUE) or reverse order (FALSE). Defaults to TRUE.
#'
#' @return The dot plot, either as a `ggplot2` object (if `standalone = TRUE`) or a `plotly` object (if `standalone = FALSE`).
#'
#' @export
GeneSetsPlot <- function(
  df,
  selected_palette = "App colors",
  standalone = FALSE,
  color_scale_order = TRUE
) {
  # Define variables locally for R CMD check
  Contrast <- Pathway <- GSDescription <- GSURL <- EnrichmentScore <- SetSize <- TooltipText <- PVal <- data <- NULL

  if (nrow(df) == 0) {
    return(empty_plot("No gene sets available for the selected pathway class."))
  }

  # ---- Prepare data ----
  df <- df |>
    mutate(
      PVal = -log10(PVal),
      # Format the pathway names for display and tooltips
      Pathway = (\(x) {
        x <- str_split_fixed(x, "_", 2)
        pathway_name <- x[, 2] |>
          gsub("_", " ", x = _) |>
          tolower() |>
          str_to_title()

        str_wrap(paste0(x[, 1], ": ", pathway_name), width = 40)
      })(Pathway),
      # Reorder the pathways within each contrast for plotting
      Pathway = reorder_within(
        x = Pathway,
        by = EnrichmentScore,
        within = Contrast
      ),
      # Create the tooltip text
      TooltipText = paste0(
        "<b><div style='font-size:16px;'>",
        str_split_i(Pathway, "___", i = 1),
        "</div></b><hr><b>Set size: </b>",
        SetSize,
        "<br><b>Gene set enrichment: </b>",
        sprintf("%.2f", EnrichmentScore),
        "<br><br><b>Description: </b>",
        GSDescription,
        "<a href='",
        GSURL,
        "' target='_blank'> (more information)</a>."
      )
    )

  # ---- Create the ggplot ----
  p <- ggplot(
    data = df,
    aes(
      x = EnrichmentScore,
      y = Pathway,
      size = SetSize,
      color = PVal,
      text = TooltipText,
      customdata = paste0(
        # Get the genes for the modal
        sapply(data, function(x) paste(x$Symbol, collapse = ",")),
        "|",
        # As reorder_within() was used on this column, it contains the pathway and the contrast separated by `___`
        Pathway,
        "|",
        SetSize,
        "|",
        GSDescription,
        "|",
        GSURL
      )
    )
  ) +
    geom_point() +
    labs(
      x = "Enrichment score",
      y = "",
      size = "",
      color = "-log10 p-value"
    ) +
    facet_wrap(
      ~Contrast,
      ncol = 1,
      scales = "free_y"
    ) +
    # This is needed in combination with `tidytext::reorder_within()`
    scale_y_reordered()

  # ---- Add the selected colors ----
  p <- add_selected_colors(
    p = p,
    selected_palette = selected_palette,
    color_scale_order = color_scale_order
  )

  # ---- Convert to ploltly ----
  if (!standalone) {
    p <- ggplotly(
      p,
      height = calculatePlotHeight(
        n_samples = length(unique(df$Contrast)),
        min_size = 75 * length(unique(df$Contrast)),
        per_sample_size = 50 *
          length(unique(df$Pathway)) /
          length(unique(df$Contrast))
      ),
      source = "gene_sets_plot",
      tooltip = "text"
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
      # Attach the custom tooltip from JS
      # And change the cursor to pointer on hover
      onRender(
        "
        function(el, x, tooltipType) {
          enableCustomTooltip(el, tooltipType);
          enablePointerCursorOnHover(el);
        }
      ",
        data = list(tooltipType = "jaccard")
      ) |>
      # Register click events for modals
      event_register("plotly_click")

    # ---- Fine tuning of the plotly object ----
    for (i in seq_along(p$x$data)) {
      tr <- p$x$data[[i]]
      # Remove default tooltip
      if (identical(tr$type, "scatter")) {
        p$x$data[[i]]$hoverinfo <- "none"
      }

      # Adjust the colorbar length and position
      if (!is.null(tr$marker$colorbar)) {
        p$x$data[[i]]$marker$colorbar$lenmode <- "pixels"
        p$x$data[[i]]$marker$colorbar$len <- 300
        p$x$data[[i]]$marker$colorbar$y <- 1
        p$x$data[[i]]$marker$colorbar$yanchor <- "top"
      }
    }
    # ---- Some styling for the ggplot outside of the app ----
  } else {
    p <- standalone_plot_style(p) +
      labs(size = "Gene set size")

    message(
      "Y-axis labels may overlap, when plot height is too small. You may need to adjust this manually."
    )
  }

  return(p)
}
