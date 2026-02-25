#' @title Create Interactive Top Gene Sets Plot
#'
#' @description
#' Generates an interactive dotplot of top gene sets for selected contrasts in gene set enrichment analysis (GSEA) using `plotly`. The plot displays enrichment scores, set sizes, and direction of regulation, with custom tooltips, facetting by contrast, and links to gene set descriptions. This plot is registered for click events, which open a modal showing a volcano plot (\code{\link{createVolcanoPlot}()}) and a heatmap (\code{\link{createGeneExpressionHeatmap}()}) of the selected gene set.
#'
#' @param df A data frame containing gene set enrichment results, including enrichment scores, set sizes, direction, and contrast labels.
#'
#' @param df_genes A data frame with gene set annotation columns: GSName, GSDescription, and GSURL.
#'
#' @param selected_palette Character. The name of the color palette to use for coloring gene sets.
#'
#' @param selected_contrast Character vector. Contrast names to include in the plot.
#'
#' @param selected_gene_sets Character vector. Gene set names to include in the plot.
#'
#' @return The interactive dotplot as a `plotly` object.
#'
#' @export
createTopGeneSetsPlot <- function(
  df,
  df_genes,
  selected_palette,
  selected_contrast,
  selected_gene_sets
) {
  # Define variables locally for R CMD check
  Contrast <- Pathway <- GSName <- GSDescription <- GSURL <- EnrichmentScore <- SetSize <- Direction <- TooltipText <- NULL

  df <- df %>%
    filter(
      Contrast %in% selected_contrast,
      Pathway %in% selected_gene_sets
    ) %>%
    left_join(
      df_genes %>% distinct(GSName, GSDescription, GSURL),
      by = c("Pathway" = "GSName")
    ) %>%
    mutate(
      # Sort the symbols within each group using `tidytext::reorder_within()`
      Pathway = reorder_within(
        x = Pathway,
        by = EnrichmentScore,
        within = Contrast
      ),
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
      ),
    ) %>%
    separate(
      Contrast,
      into = c("contrast_left", "contrast_right"),
      sep = " vs ",
      remove = FALSE
    ) %>%
    mutate(
      Direction = case_when(
        Direction == "up" ~ paste("Upregulated gene sets in", contrast_left),
        Direction == "down" ~ paste("Upregulated gene sets in", contrast_right)
      )
    )

  p <- ggplot(
    data = df,
    aes(
      x = EnrichmentScore,
      y = Pathway,
      size = SetSize,
      color = Direction,
      text = TooltipText
    )
  ) +
    geom_point(alpha = 0.8) +
    labs(
      x = "Gene set enrichment",
      y = "",
      size = "",
      color = ""
    ) +
    facet_wrap(
      ~Contrast,
      ncol = 1,
      scales = "free_y"
    ) +
    # This is needed in combination with `tidytext::reorder_within()`
    scale_y_reordered() +
    theme(strip.text = element_text(size = 12))

  # Add the selected colors
  p <- add_selected_colors(p = p, selected_palette = selected_palette)

  p <- ggplotly(
    p,
    height = calculatePlotHeight(
      n_samples = length(unique(df$Contrast)),
      min_size = 800,
      per_sample_size = 800
    ),
    source = "gene_sets_plot"
  )

  # Get the y-axis tick labels for each facet
  y_tick_labels <- list()
  for (i in seq_along(p$x$layout)) {
    layout_name <- paste0("yaxis", ifelse(i == 1, "", i))
    if (!is.null(p$x$layout[[layout_name]]$ticktext)) {
      y_tick_labels[[layout_name]] <- p$x$layout[[layout_name]]$ticktext
    }
  }

  # Iterate over each trace and assign the custom tooltip text
  for (i in seq_along(p$x$data)) {
    tr <- p$x$data[[i]]
    if (!is.null(tr$y) && !is.null(tr$x)) {
      yaxis_name <- if (!is.null(tr$yaxis)) {
        sub("^y", "yaxis", tr$yaxis)
      } else {
        "yaxis"
      }
      tick_labels <- y_tick_labels[[yaxis_name]]
      y_labels <- if (!is.null(tick_labels)) {
        tick_labels[tr$y]
      } else {
        as.character(tr$y)
      }
      # Remove the suffix added by `tidytext::reorder_within()`
      df$Pathway_base <- str_split_i(as.character(df$Pathway), "___", 1)
      tooltip_contrast_mat <- base::mapply(
        function(yval, xval) {
          match_idx <- base::which(
            as.character(df$Direction) == as.character(tr$name) &
              as.character(df$Pathway_base) == as.character(yval)
          )
          if (length(match_idx) > 0) {
            # Add the actual hover and the contrast to the customdata
            c(
              df$TooltipText[match_idx[1]],
              as.character(df$Contrast[match_idx[1]])
            )
          } else {
            c("", "")
          }
        },
        y_labels,
        tr$x
      )
      p$x$data[[i]]$customdata <- t(tooltip_contrast_mat)
    }

    # Remove default tooltip
    if (identical(tr$type, "scatter")) {
      p$x$data[[i]]$hoverinfo <- "none"
    }
  }

  p <- p %>%
    layout(
      # Put legend above the plot
      legend = list(
        orientation = "h",
        y = 1.02,
        yanchor = "bottom",
        yref = "container",
        xanchor = "center",
        xref = "container",
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
    # Attach the custom tooltip from JS
    onRender(
      "
        function(el, x, tooltipType) {
          enableCustomTooltip(el, tooltipType);
        }
      ",
      data = list(tooltipType = "gene_sets")
    ) %>%
    # Register click events for modals
    event_register("plotly_click")

  return(p)
}
