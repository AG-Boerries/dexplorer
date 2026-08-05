#' @title Create a Volcano Plot
#'
#' @description
#' Generates a volcano plot for differential gene expression analysis using `ggplot2` and/or `plotly`. Highlights significant genes based on user-defined thresholds, annotates user-selected genes, and provides detailed tooltips with gene information. Supports multiple contrasts, custom color palettes, and optimized rendering for large datasets.
#'
#' @param df A data frame containing gene expression results, including columns for gene symbols, log2 fold change, adjusted p-values, and contrast labels.
#'
#' @param p_threshold Numeric or character. The adjusted p-value threshold for significance.
#'
#' @param l2fc_threshold Numeric. The log2 fold-change threshold for significance.
#'
#' @param selected_genes Character vector. Gene symbols to highlight across all contrasts similarly.
#'
#' @param selected_contrast Character vector. Contrast names to display in the plot facets.
#'
#' @param standalone Logical. If `TRUE`, the PCA plot is generated as a standalone plot, which is not interactive. If `FALSE` (required inside DExploreR), the plot is interactive via `plotly`. Defaults to `FALSE`.
#'
#' @param color_up Set the color of the upregulated genes. Defaults to the app's pink color.
#'
#' @param color_down Set the color of the downregulated genes. Defaults to the app's blue color.
#'
#' @param highlight_top Logical. If `TRUE`, highlights the top differentially expressed genes per contrast. Defaults to `TRUE`.
#'
#' @param dot_size Numeric. Size of the points in the plot. Defaults to `1.5`.
#'
#' @return The interactive volcano plot as a `plotly` object.
#'
#' @export
createVolcanoPlot <- function(
  df,
  # selected_palette = "App colors",
  p_threshold = "0.05",
  l2fc_threshold = 1,
  selected_genes = c(),
  selected_contrast = "all",
  standalone = FALSE,
  color_up = get_theme_colors(color = "pink"),
  color_down = get_theme_colors(color = "blue"),
  highlight_top = TRUE,
  dot_size = 1.5
) {
  # Define variables locally for R CMD check
  Symbol <- Log2FC <- LogPValAdj <- GeneID <- EntrezID <- Description <- Alias <- NCBIURL <- PValAdj <- Regulation <- Contrast <- TooltipText <- NULL

  # ---- Data preparation ----
  df <- df |>
    # Create tooltip text for each gene
    mutate(
      TooltipText = paste0(
        "<b><div style='font-size:16px;'>",
        str_split_i(Symbol, "___", i = 1),
        "</div></b><hr><b>Log2 fold change: </b>",
        sprintf("%.1f", Log2FC),
        "<br><b>-Log10 adjusted p-value: </b>",
        sprintf("%.1f", LogPValAdj),
        "<br><br><b>Ensembl ID: </b>",
        GeneID,
        "<br><b>Entrez ID: </b>",
        EntrezID,
        "<br><b>Description: </b>",
        Description,
        "<br><b>Alias: </b>",
        Alias,
        "<hr>",
        "For further information visit <a href='",
        NCBIURL,
        "' target='_blank'>NCBI</a>."
      ),
      # Using user-defined thresholds to define significance
      Regulation = case_when(
        PValAdj < as.numeric(p_threshold) ~ case_when(
          Log2FC > l2fc_threshold ~
            "Upregulated",
          Log2FC < -l2fc_threshold ~
            "Downregulated",
          TRUE ~ "Not significant"
        ),
        PValAdj >= as.numeric(p_threshold) ~ "Not significant"
      )
    ) |>
    # Filter for user selected contrasts
    (\(x) {
      if (!identical(selected_contrast, "all")) {
        filter(x, Contrast %in% selected_contrast)
      } else {
        x
      }
    })()

  # ---- Create the ggplot object ----
  p <- ggplot(
    data = df,
    aes(
      x = Log2FC,
      y = LogPValAdj,
      text = TooltipText,
      color = Regulation
    )
  ) +
    geom_point(alpha = 0.5, size = dot_size) +
    # Highlight the user-selected genes with larger white points and text labels
    geom_point(
      data = df |> filter(Symbol %in% selected_genes),
      aes(x = Log2FC, y = LogPValAdj),
      size = 3,
      color = "white",
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    geom_text(
      data = df |> filter(Symbol %in% selected_genes),
      aes(x = Log2FC, y = LogPValAdj, label = Symbol),
      color = "black",
      inherit.aes = FALSE,
      show.legend = FALSE,
    ) +
    # Add threshold lines
    geom_hline(
      yintercept = -log10(as.numeric(p_threshold)),
      linetype = "dashed",
      show.legend = FALSE
    ) +
    geom_vline(
      xintercept = c(-l2fc_threshold, l2fc_threshold),
      linetype = "dashed",
      show.legend = FALSE
    ) +
    facet_wrap(~Contrast, ncol = 2) +
    labs(
      y = "-Log10 adjusted p-value",
      x = "Log2 fold change",
      color = ""
    ) +
    scale_color_manual(
      values = c(
        "Upregulated" = color_up,
        "Downregulated" = color_down,
        "Not significant" = "grey80"
      )
    ) +
    xlim(c(-max(abs(df$Log2FC)) - 0.5, max(abs(df$Log2FC)) + 0.5))

  # ---- Highlight top differentially expressed genes per contrast by adj. p-value ----
  if (highlight_top) {
    # When used as standalone, then `geom_text_repel()` can be used
    if (standalone) {
      p <- p +
        geom_text_repel(
          data = df |>
            filter(Regulation != "Not significant") |>
            group_by(Contrast) |>
            slice_max(order_by = LogPValAdj, n = 5),
          aes(x = Log2FC, y = LogPValAdj, label = Symbol),
          color = "black",
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    } else {
      # Standard geom_text() has to be used when later converted to plotly
      # This is because `geom_text_repel()` is not compatible with plotly
      p <- p +
        geom_text(
          data = df |>
            filter(Regulation != "Not significant") |>
            group_by(Contrast) |>
            slice_max(order_by = LogPValAdj, n = 5),
          aes(x = Log2FC, y = LogPValAdj, label = Symbol),
          color = "black",
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }
  }

  # ---- Calculate the plot height based on the number of contrasts ----
  p_height <- calculatePlotHeight(
    n_samples = round(length(unique(df$Contrast)) / 2),
    min_size = 600,
    per_sample_size = 600
  )

  # ---- If used in DExploreR, convert to plotly ----
  if (!standalone) {
    p <- ggplotly(
      p,
      tooltip = "text",
      height = p_height
    ) |>
      layout(
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
      # Add the custom tooltip
      onRender(
        "
        function(el, x, tooltipType) {
          enableCustomTooltip(el, tooltipType);
        }
      ",
        data = list(tooltipType = "standard")
      )

    for (i in seq_along(p$x$data)) {
      # Remove default tooltip
      p$x$data[[i]]$hoverinfo <- "none"
      # Avoid cluttering logs by removing 'hoveron' attribute as this is not available in 'scattergl' objects
      p$x$data[[i]]$hoveron <- NULL
    }

    # ---- Adjust the y-position of the legend ----
    # The factor 25 px was determined empirically
    y_legend_pos <- 25 / p_height + 1
    p$x$layoutAttrs[[1]]$legend$y <- y_legend_pos

    # Convert to WebGL for performance with many points
    p <- p |> toWebGL()
  } else {
    p <- standalone_plot_style(p)
  }

  return(p)
}
