# Create an empty plot with a custom message
empty_plot <- function(message = "Nothing to display ...") {
  p <- ggplot() +
    annotate(
      "text",
      x = 0,
      y = 0.2,
      label = message,
      size = 10,
      fontface = "bold",
      color = "grey50"
    ) +
    annotate(
      "text",
      x = 0,
      y = -0.2,
      label = "\U2639",
      size = 20,
      color = "grey50"
    ) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void()

  p <- ggplotly(p) %>%
    # Remove the modebar, when the plot is empty
    config(
      displaylogo = FALSE,
      displayModeBar = FALSE
    )

  return(p)
}
