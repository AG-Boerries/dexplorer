#' @title Create Interactive Volcano Plot
#'
#' @description
#' Generates an interactive volcano plot for differential gene expression analysis using `ggplot2` and `plotly`. Highlights significant genes based on user-defined thresholds, annotates user-selected genes, and provides detailed tooltips with gene information. Supports multiple contrasts, custom color palettes, and optimized rendering for large datasets.
#'
#' @param df A data frame containing gene expression results, including columns for gene symbols, log2 fold change, adjusted p-values, and contrast labels.
#'
#' @param selected_palette Character. The name of the color palette to use for significance coloring.
#'
#' @param p_threshold Numeric or character. The adjusted p-value threshold for significance.
#'
#' @param l2fc_threshold Numeric. The log2 fold-change threshold for significance.
#'
#' @param selected_genes Character vector. Gene symbols to highlight and annotate on the plot.
#'
#' @param selected_contrast Character vector. Contrast names to display in the plot facets.
#'
#' @param dot_size Numeric. Size of the points in the plot. Default is 1.5.
#'
#' @return The interactive volcano plot as a `plotly` object.
#'
#' @export
createVolcanoPlot <- function(
  df,
  selected_palette,
  p_threshold,
  l2fc_threshold,
  selected_genes,
  selected_contrast,
  dot_size = 1.5
) {
  # Define variables locally for R CMD check
  Symbol <- Log2FC <- LogPValAdj <- GeneID <- EntrezID <- Description <- Alias <- NCBIURL <- PValAdj <- Significant <- Contrast <- TooltipText <- xleft <- ypos <- left_label <- xright <- right_label <- NULL

  df <- df %>%
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
      # `p_threshold` comes from a `sliderTextInput()`, which returns a character
      Significant = PValAdj < as.numeric(p_threshold) &
        abs(Log2FC) > l2fc_threshold,
      Significant = ifelse(Significant, "Yes", "No")
    ) %>%
    # Display only user-selected contrasts
    filter(Contrast %in% selected_contrast)

  # Create a dataframe to annotate the contrasts on the plot
  contrast_annotation <- df %>%
    distinct(Contrast) %>%
    rowwise() %>%
    mutate(
      left_label = str_split_1(Contrast, " vs ")[2],
      right_label = str_split_1(Contrast, " vs ")[1]
    ) %>%
    left_join(
      crossing(
        Contrast = unique(df$Contrast),
        df %>%
          summarise(
            xleft = -max(abs(df$Log2FC)) / 2,
            xright = max(abs(df$Log2FC)) / 2,
            ypos = max(LogPValAdj) * 0.9
          )
      ),
      by = "Contrast"
    )

  p <- ggplot(
    data = df,
    aes(
      x = Log2FC,
      y = LogPValAdj,
      text = TooltipText,
      color = Significant
    )
  ) +
    geom_point(alpha = 0.5, size = dot_size) +
    # Add the addtional user-selected genes as text
    # `geom_label()` would be a better option but this is no supported by plotly
    geom_text(
      data = df %>% filter(Symbol %in% selected_genes),
      aes(x = Log2FC, y = LogPValAdj, label = Symbol),
      color = "black",
      inherit.aes = FALSE,
      show.legend = FALSE,
      nudge_y = 0.5
    ) +
    geom_point(
      data = df %>% filter(Symbol %in% selected_genes),
      aes(x = Log2FC, y = LogPValAdj),
      size = 3,
      alpha = 0.8,
      color = "black",
      inherit.aes = FALSE,
      show.legend = FALSE
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
    geom_text(
      data = contrast_annotation,
      aes(
        x = xleft,
        y = ypos,
        label = left_label
      ),
      inherit.aes = FALSE
    ) +
    geom_text(
      data = contrast_annotation,
      aes(
        x = xright,
        y = ypos,
        label = right_label
      ),
      inherit.aes = FALSE
    ) +
    facet_wrap(~Contrast, ncol = 2) +
    labs(
      y = "-Log10 adjusted p-value",
      x = "Log2 fold change",
      color = "Significant differential expression"
    ) +
    xlim(c(-max(abs(df$Log2FC)) - 0.5, max(abs(df$Log2FC)) + 0.5))

  # Add the selected color scale
  p <- add_selected_colors(p = p, selected_palette = selected_palette)

  p <- ggplotly(
    p,
    tooltip = "text",
    height = calculatePlotHeight(
      n_samples = round(length(unique(df$Contrast)) / 2),
      min_size = 600,
      per_sample_size = 600
    )
  ) %>%
    layout(
      legend = list(
        orientation = "h",
        y = 1.1,
        xanchor = "center",
        xref = "paper",
        x = 0.5
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

  # Convert to WebGL for performance with many points
  # This throws some warnings about missing attributes of `gl`, which could be suppressed by `suppressWarnings()`, but this slows down the rendering significantly
  p <- p %>% toWebGL()

  return(p)
}
