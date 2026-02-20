create_venn_diagram <- function(df, selected_palette) {
  # The columns with the contrast names 1 and 2
  groups <- colnames(df[, 1:2])

  # `ComplexUpset::arrange_venn()` can fail if there are too many genes in the intersection
  # So we need to try and catch the error and reduce the number of genes until it works
  attempt_count <- 0
  prop_per_fail <- 0.9

  # Reduce number of genes by 10% until `arrange_venn()` can calculate the grid
  repeat {
    attempt_count <- attempt_count + 1
    attempt <- tryCatch(
      arrange_venn(df, sets = groups),
      error = function(e) NULL
    )

    # If no error message, then return `df`
    if (!is.null(attempt)) {
      df <- attempt
      break
    }

    # Group by the contrasts, which are in columns 1 and 2 and sample within each group
    df <- df %>%
      group_by(pick(1, 2)) %>%
      slice_sample(prop = prop_per_fail) %>%
      ungroup()
  }

  # Calculate the fraction of display genes
  fraction_displayed <- prop_per_fail^(attempt_count - 1)

  # Add the tooltips
  df <- df %>%
    mutate(
      TooltipText = paste0(
        "<b><div style='font-size:16px;'>",
        Symbol,
        "</div></b><hr><b>Ensembl ID: </b>",
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

  # Construct the region labels
  left_region = df %>% filter(x == min(x)) %>% pull(region) %>% unique()
  right_region = df %>% filter(x == max(x)) %>% pull(region) %>% unique()
  middle_region = df %>%
    filter(pick(1) == pick(2)) %>%
    pull(region) %>%
    unique()

  left_region_label = paste0(
    left_region,
    " (",
    round(sum(df$region == left_region) / fraction_displayed),
    " DEGs)"
  )
  right_region_label = paste0(
    right_region,
    " (",
    round(sum(df$region == right_region) / fraction_displayed),
    " DEGs)"
  )
  middle_region_label = paste0(
    middle_region,
    " (",
    round(sum(df %>% pull(1) == df %>% pull(2)) / fraction_displayed),
    " DEGs)"
  )

  # Extract family and get a vector of colors
  col_family <- get_color_family(selected_palette = selected_palette)
  colors <- get_discrete_palette(
    family = col_family,
    palette = selected_palette,
    # Only two colors are needed, one for each group/set
    # The color in the intersection will be automatically calculated by `scale_color_venn_mix()`
    # And is a 1:1 mixture of the other two colors
    n = 2
  )

  # Create the venn diagram using `ComplexUpset`
  p <- ggplot(df) +
    geom_venn_circle(data = df, sets = groups, linewidth = 1) +
    geom_point_quiet(
      aes(x = x, y = y, color = region, text = TooltipText),
      size = 1,
      position = position_jitter(seed = 42)
    ) +
    # Add the colors using the built-in function
    scale_color_venn_mix(
      data = df,
      sets = groups,
      colors = colors
    ) +
    coord_fixed() +
    theme_void() +
    theme(plot.margin = margin(t = 20, b = 30))

  p <- ggplotly(p, tooltip = "text") %>%
    layout(
      showlegend = FALSE,
      paper_bgcolor = "white",
      plot_bgcolor = "white"
    ) %>%
    # Reduce the modebar to only essential tools
    config(
      displaylogo = FALSE,
      modeBarButtons = list(
        list("toImage"),
        list("zoom2d"),
        list("pan2d"),
        list("resetScale2d")
      ),
      toImageButtonOptions = list(
        format = "png",
        filename = "Contrast_intersection",
        height = 720,
        width = 1280
      )
    )

  # Calculate the y-axis limits using the radius of the circles to position the annotations above the plot
  ymax <- max(p$x$data[[1]]$y, na.rm = TRUE) * 1.05
  ymin <- min(p$x$data[[1]]$y, na.rm = TRUE)

  p <- p %>%
    # Add the region labels as annotations with the number of genes
    add_annotations(
      x = min(df$x) / 1.7,
      y = ymax,
      xref = "x",
      yref = "y",
      text = left_region_label,
      showarrow = T,
      arrowcolor = "black",
      ax = -20,
      ay = -40,
      bgcolor = "white"
    ) %>%
    add_annotations(
      x = max(df$x) / 1.7,
      y = ymax,
      xref = "x",
      yref = "y",
      text = right_region_label,
      showarrow = T,
      arrowcolor = "black",
      ax = 20,
      ay = -40,
      bgcolor = "white"
    ) %>%
    add_annotations(
      x = 0,
      y = ymin,
      xref = "x",
      yref = "y",
      text = middle_region_label,
      showarrow = T,
      arrowcolor = "black",
      ax = 0,
      ay = 40,
      bgcolor = "white"
    ) %>%
    # Attach the custom tooltip from JS
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
    # Remove tooltip for the lines of the circles
    if (identical(p$x$data[[i]]$mode, "lines")) {
      p$x$data[[i]]$hoverinfo <- "skip"
    }
  }

  return(p)
}
