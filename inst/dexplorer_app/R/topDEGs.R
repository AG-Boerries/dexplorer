format_for_top_dgea_genes <- function(
  df,
  selected_contrast,
  selected_number_of_genes,
  selected_direction,
  fc_or_pvalue
) {
  # Extract the column to order the data by
  order_col <- if (fc_or_pvalue) "Log2FC" else "LogPValAdj"

  df <- df %>%
    mutate(Log2FC = abs(Log2FC)) %>%
    # Filter for selected direction
    filter(Direction == ifelse(selected_direction, "up", "down")) %>%
    # Group by contrast, to get top genes per contrast
    group_by(Contrast) %>%
    # Extract only top n genes per contrast
    slice_max(
      order_by = !!sym(order_col),
      n = selected_number_of_genes,
      with_ties = FALSE
    ) %>%
    ungroup() %>%
    filter(Contrast %in% selected_contrast) %>%
    mutate(
      # Sort the symbols within each group using `tidytext::reorder_within()`
      Symbol = reorder_within(
        Symbol,
        !!sym(order_col),
        Contrast
      )
    )

  return(df)
}


plot_top_dgea_genes <- function(df, selected_palette, fc_or_pvalue) {
  # Display empty plot message, if the sample selection returns an empty dataframe
  if (base::nrow(df) == 0) {
    return(empty_plot())
  }

  # Add the tooltip text just before plotting
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
      )
    )

  # Get the column for the x axis and the fill color
  x_col <- if (fc_or_pvalue) "Log2FC" else "LogPValAdj"
  fill <- if (x_col == "Log2FC") "LogPValAdj" else "Log2FC"

  # Get the correpsonding labels
  x_col_lab <- if (x_col == "Log2FC") {
    "Absolute log2 fold change"
  } else {
    "-Log10 adjusted p-value"
  }
  fill_lab <- if (x_col == "Log2FC") {
    "-Log10 adjusted\np-value"
  } else {
    "Absolute log2\nfold change"
  }

  p <- ggplot(
    data = df,
    aes(
      x = !!sym(x_col),
      y = Symbol,
      fill = !!sym(fill),
      text = TooltipText
    )
  ) +
    geom_bar(stat = "identity") +
    facet_wrap(~Contrast, ncol = 2, scales = "free_y") +
    labs(y = "Gene", x = x_col_lab, fill = fill_lab) +
    scale_x_continuous(breaks = pretty_breaks(n = 4)) +
    # This is needed in combination with `tidytext::reorder_within()`
    scale_y_reordered()

  # Add the selected color scale
  p <- add_selected_colors(p = p, selected_palette = selected_palette)

  p <- ggplotly(
    p,
    tooltip = "text",
    height = plot_height(
      n_samples = round(length(unique(df$Contrast)) / 2),
      min_size = 500,
      per_sample_size = 500
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
    # Attach the custom tooltip from JS
    onRender(
      "
        function(el, x, tooltipType) {
          enableCustomTooltip(el, tooltipType);
        }
      ",
      data = list(tooltipType = "top_genes")
    )

  # Remove default tooltip
  for (i in seq_along(p$x$data)) {
    p$x$data[[i]]$hoverinfo <- "none"
  }

  return(p)
}
