#' Safe wrapper around plotly::renderPlotly
#'
#' Ensures plot rendering errors do not break the app and instead show a
#' user-friendly empty plot message.
renderPlotly <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }

  plotly::renderPlotly({
    tryCatch(
      eval(expr, env),
      error = function(e) {
        if (inherits(e, c("shiny.silent.error", "shiny.output.cancel"))) {
          stop(e)
        }

        empty_plot(
          paste0(
            "oops, could not generate this plot,\n",
            "most likely because of wrong or missing input data.\n\n",
            "Error details: ",
            conditionMessage(e)
          )
        )
      }
    )
  }, env = env, quoted = TRUE)
}
