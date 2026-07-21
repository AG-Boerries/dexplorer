#' @title Create Top DEGs Bar Plot
#'
#' @description
#' Generates a bar plot of top differentially expressed genes (DEGs) for each contrast using `ggplot2` and `plotly`. Highlights genes by log2 fold change or adjusted p-value, provides detailed tooltips with gene information, supports custom color palettes, and facets by contrast.
#'
#' @param df A data frame of top DEGs per contrast, as returned by \code{formatTopDEGs()}, including gene annotation columns.
#'
#' @param selected_palette Character. The name of the color palette to use for bar fill. Defaults to "App colors". See \code{print(color_choices)} for available palettes.
#'
#' @param standalone Logical. If `TRUE`, the PCA plot is generated as a standalone plot, which is not interactive. If `FALSE` (required inside DExploreR), the plot is interactive via `plotly`. Defaults to `FALSE`.
#'
#' @param color_scale_order Logical. Whether to use the standard order of colors (TRUE) or reverse order (FALSE). Defaults to TRUE.
#'
#' @return A bar plot as a `ggplot2` object if `standalone = TRUE`, or an interactive `plotly` object if `standalone = FALSE`.
#'
#' @export
createTopDEGsPlot <- function(
  df,
  selected_palette = "App colors",
  standalone = FALSE,
  color_scale_order = TRUE
) {
  # Define variables locally for R CMD check
  Symbol <- Log2FC <- LogPValAdj <- GeneID <- EntrezID <- Description <- Alias <- NCBIURL <- TooltipText <- NULL

  # Validate inputs
  match.arg(selected_palette, color_choices_flat)

  # Display empty plot message, if the sample selection returns an empty dataframe
  if (base::nrow(df) == 0) {
    return(empty_plot())
  }

  # ---- Prepare the tooltip text ----
  df <- df |>
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

  # ---- Define x-axis and fill variables based on user selection ----
  # The ordering happens in formatTopDEGs() and the information is contained in the created column `ordered_by`
  x_col <- unique(df$ordered_by)
  fill <- if (x_col == "Log2FC") "LogPValAdj" else "Log2FC"

  # Prepare the x axis and fill labels
  x_col_lab <- if (x_col == "Log2FC") {
    "Log2 fold change"
  } else {
    "-Log10 adjusted p-value"
  }
  fill_lab <- if (x_col == "Log2FC") {
    "-Log10 adjusted\np-value"
  } else {
    "Log2\nfold change"
  }

  # ---- Create the bar plot ----
  p <- ggplot(
    data = df,
    aes(
      x = !!sym(x_col),
      y = Symbol,
      fill = !!sym(fill),
      text = TooltipText
    )
  ) +
    geom_col() +
    facet_wrap(
      ~Contrast,
      ncol = 2,
      scales = "free_y",
      axes = "all"
    ) +
    labs(y = "Gene", x = x_col_lab, fill = fill_lab) +
    # This is needed in combination with `tidytext::reorder_within()`
    scale_y_reordered()

  # Add the selected color scale
  p <- add_selected_colors(
    p = p,
    selected_palette = selected_palette,
    color_scale_order = color_scale_order
  )

  if (!standalone) {
    # ---- Convert to plotly ----
    p <- ggplotly(
      p,
      tooltip = "text",
      height = calculatePlotHeight(
        n_samples = round(length(unique(df$Contrast)) / 2),
        min_size = 500,
        per_sample_size = 500
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
      # Attach the custom tooltip from JS
      onRender(
        "
        function(el, x, tooltipType) {
          enableCustomTooltip(el, tooltipType);
        }
      ",
        data = list(tooltipType = "top_genes")
      )

    for (i in seq_along(p$x$data)) {
      # Remove default tooltip
      p$x$data[[i]]$hoverinfo <- "none"
      # Adjust the colorbar length and position
      if (!is.null(p$x$data[[i]]$marker$colorbar)) {
        p$x$data[[i]]$marker$colorbar$lenmode <- "pixels"
        p$x$data[[i]]$marker$colorbar$len <- 300
        p$x$data[[i]]$marker$colorbar$y <- 1
        p$x$data[[i]]$marker$colorbar$yanchor <- "top"
      }
    }
  } else {
    p <- standalone_plot_style(p)
  }

  return(p)
}
