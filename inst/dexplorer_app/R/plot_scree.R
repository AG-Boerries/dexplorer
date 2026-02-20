create_scree_plot <- function(explained_var, pc_x, pc_y, selected_palette) {
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

  for (i in seq_along(p$x$data)) {
    # Remove default tooltip
    p$x$data[[i]]$hoverinfo <- "none"
  }

  return(p)
}
