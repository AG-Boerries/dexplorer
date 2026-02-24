app_startup <- function() {
  theme_set(
    theme_minimal() +
      theme(
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = margin(l = 20, b = 10),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12)
      )
  )
}
