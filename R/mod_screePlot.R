#' @title Create Interactive Scree Plot
#'
#' @description
#' Generates an interactive scree plot using `ggplot2` and `plotly`.
#'
#' @param explained_var A data frame with columns `PC` (principal component names) and `Variance` (explained variance for each component).
#'
#' @param pc_x Character. The name of the principal component selected for the x-axis in a related PCA plot.
#'
#' @param pc_y Character. The name of the principal component selected for the y-axis in a related PCA plot.
#'
#' @param selected_palette Character. The name of the color palette.
#'
#' @return The interactive scree plot as a `plotly` object.
#'
#' @export
createScreePlot <- function(explained_var, pc_x, pc_y, selected_palette) {
  # Define variables locally for R CMD check
  PC <- Variance <- SelectedPC <- TooltipText <- NULL

  explained_var <- explained_var %>%
    # Create a column to color the selected PCs
    mutate(
      SelectedPC = ifelse(
        PC %in% c(pc_x, pc_y),
        "Selected",
        "Not selected"
      ),
      TooltipText = paste0(
        "<b>Variance explained: </b>",
        sprintf("%.2f", Variance),
        " %"
      )
    )

  p <- ggplot(
    data = explained_var,
    aes(x = PC, y = Variance, fill = SelectedPC, text = TooltipText)
  ) +
    geom_bar(stat = "identity") +
    labs(x = "Principal Component", y = "Variance\nexplained (%)") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, hjust = 1)
    )

  # Add the selected colors
  p <- add_selected_colors(p = p, selected_palette = selected_palette)

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

  return(p)
}
