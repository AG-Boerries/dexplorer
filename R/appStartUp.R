#' @title DExploreR App Startup Functions
#'
#' @description
#' Performs global setup actions for the DExploreR Shiny app, such as setting the default ggplot2 theme.
#'
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
