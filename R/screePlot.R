#' @title Create a Scree Plot
#'
#' @description
#' Generates an (interactive) scree plot using `ggplot2` and `plotly`, allows to highlight two PCs, for instance, those,which are displayed in the PCA. The plot automatically removes PCs explaining less than 1 % of total variance and orders the PCs by variance explained.
#'
#' @param explained_var A data frame with columns `PC` (principal component names) and `Variance` (explained variance for each component).
#'
#' @param pc_x Character (can also be a vector of characters specifying multiple PCs or empty for no highlighting). The name of the principal component selected for the x-axis in a related PCA plot. Defaults to "PC1".
#'
#' @param pc_y Character (can also be a vector of characters specifying multiple PCs or empty for no highlighting). The name of the principal component selected for the y-axis in a related PCA plot. Defaults to "PC2".
#'
#' @param selected_palette Character. The name of the color palette. Defaults to "App colors".
#'
#' @param standalone Logical. If `TRUE`, the scree plot is generated as a standalone plot, which is not interactive. If `FALSE` (required inside DExploreR), the plot is interactive via `plotly` and highlights the bars of `pc_x` and `pc_y`. Defaults to `TRUE`.
#'
#' @return The (interactive) scree plot as a `ggplot2` or `plotly` object.
#'
#' @export
createScreePlot <- function(
  explained_var,
  pc_x = "PC1",
  pc_y = "PC2",
  selected_palette = "App colors",
  standalone = TRUE
) {
  # Define variables locally for R CMD check
  PC <- Variance <- SelectedPC <- TooltipText <- NULL

  # ---- Prepare the data ----
  explained_var <- explained_var %>%
    # Remove PCs explaining less than 1 % of total variance
    filter(Variance >= 1) %>%
    # Create a column to color the selected PCs
    # If no PC specified, all bars will receive the same color
    mutate(
      SelectedPC = ifelse(
        PC %in% c(pc_x, pc_y),
        "Selected",
        "Not selected"
      ),
      # The tool tip is obviously only relevant when `standalone` is FALSE
      # Otherwise it is ignored
      TooltipText = paste0(
        "<b>Variance explained: </b>",
        sprintf("%.2f", Variance),
        " %"
      )
    )

  # ---- Create the plot ----
  p <- ggplot(
    data = explained_var,
    aes(
      x = reorder(PC, -Variance),
      y = Variance,
      fill = SelectedPC,
      text = TooltipText
    )
  ) +
    geom_col() +
    labs(x = "Principal Component", y = "Variance\nexplained (%)") +
    # Rotate by default to avoid overlap, when there are may PCs
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )

  # Add the selected colors
  p <- add_selected_colors(p = p, selected_palette = selected_palette)

  # ---- Fine tune plot for DExploreR or standalone ----
  if (!standalone) {
    p <- ggplotly(p, tooltip = "text") %>%
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
      onRender(
        "
        function(el, x, tooltipType) {
          enableCustomTooltip(el, tooltipType);
        }
        ",
        data = list(tooltipType = "standard")
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
        panel.grid.major = element_line(color = "grey80")
      )
  }

  return(p)
}
