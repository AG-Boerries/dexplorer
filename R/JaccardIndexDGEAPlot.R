#' @title Create DGEA Contrast Intersection Plot
#'
#' @description
#' Generates an interactive `plotly` visualization of Jaccard indices for all pairs of contrasts in a differential gene expression analysis (DGEA). The plot displays the overlap of differentially expressed genes (DEGs) between contrasts, with dot size and color representing the Jaccard index. Tooltips provide detailed comparison information, and facets show results for different regulation directions. Dots are clickable to extract gene lists and venn diagrams (as produced by \code{\link{createVennDiagram}()} for the selected comparison.
#'
#' @param df A data frame as returned by \code{\link{formatDGEAContrastIntersection}()}, summarizing Jaccard index results for each pair of contrasts and direction.
#'
#' @param selected_palette Character. The name of the color palette to use for the plot. Defaults to "App colors". See \code{print(color_choices)} for available options.
#'
#' @param standalone Logical. If `TRUE`, the PCA plot is generated as a standalone plot, which is not interactive. If `FALSE` (required inside DExploreR), the plot is interactive via `plotly`. Defaults to `FALSE`.
#'
#' @return An interactive dotplot for Jaccard indicies of up- and downregulated genes as a `plotly` object.
#'
#' @export
createDGEAContrastIntersectionPlot <- function(
  df,
  selected_palette = "App colors",
  standalone = FALSE
) {
  # Define variables locally for R CMD check
  Seta <- Setb <- JI <- DEG_both_sets <- DEG_total <- Direction <- TooltipText <- CustomData <- NULL

  # If data frame is empty, then return an empty plot with a message
  if (all(dim(df) == 0)) {
    return(empty_plot("Not enough contrasts provided."))
  }

  # Create new labels for the facets
  facet_labels <- c(
    up = "Upregulated\ngenes",
    down = "Downregulated\ngenes",
    both = "Up- and downregulated\ngenes"
  )

  # ---- Data preparation ----
  df <- df |>
    # Add tooltipp text just before plotting to avoid them being contained in the data download
    mutate(
      TooltipText = paste0(
        "<b><div style='font-size:16px; line-height:1.3;'>Comparison: </b><br>",
        Seta,
        "<br> &nbsp;&nbsp;&nbsp;<i>and</i> <br>",
        Setb,
        "</div><hr><b>Jaccard index: </b>",
        sprintf("%.3f", JI),
        "<br><b>DEGs contained in both: </b>",
        DEG_both_sets,
        "<br><b>Total DEGs: </b>",
        DEG_total
      ),
      # Add genes and direction to custom data for modal
      CustomData = paste0(Seta, "|", Setb, "|", Direction)
    )

  # ---- Create the ggplot ----
  p <- ggplot(
    data = df,
    aes(
      x = DEG_total,
      y = DEG_both_sets,
      color = JI,
      size = JI,
      text = TooltipText,
      customdata = CustomData
    )
  ) +
    geom_point() +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "black"
    ) +
    facet_wrap(~Direction, labeller = as_labeller(facet_labels)) +
    labs(
      x = "Total DEGs in both contrasts (union)",
      y = "DEGs contained in both contrasts (intersection)",
      size = "Jaccard index",
      color = "Jaccard index"
    )

  # Add the selected color scale
  p <- add_selected_colors(p = p, selected_palette = selected_palette)

  # ---- Convert to plotly ----
  if (!standalone) {
    p <- ggplotly(p, tooltip = "TooltipText", source = "dgea_jaccard") |>
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
      layout(
        # Add some space for the titles
        yaxis = list(
          title = list(
            standoff = 20
          )
        ),
        margin = list(t = 50)
      ) |>
      # Register click events for this plot
      event_register("plotly_click") |>
      # Attach the custom tooltip from JS
      onRender(
        "
        function(el, x, tooltipType) {
          enableCustomTooltip(el, tooltipType);
          enablePointerCursorOnHover(el);
        }
      ",
        data = list(tooltipType = "jaccard")
      )

    # Remove default tooltip
    for (i in seq_along(p$x$data)) {
      p$x$data[[i]]$hoverinfo <- "none"
    }
  } else {
    # Add a white background and grey grid lines for the standalone plot
    p <- p +
      theme(
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        strip.background = element_rect(fill = "white"),
      )
  }

  return(p)
}
