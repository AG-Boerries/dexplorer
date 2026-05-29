#' Execute analysis code with safe error handling
#'
#' Wraps analysis expressions to avoid app-wide failures from localized data
#' inconsistencies. Non-silent errors trigger a dismissible notification.
safe_analysis_step <- function(
  expr,
  context,
  data_hint = NULL,
  fallback = function(e) NULL,
  quoted = FALSE
) {
  if (!quoted) {
    expr <- substitute(expr)
  }

  tryCatch(
    eval(expr, parent.frame()),
    error = function(e) {
      if (inherits(e, c("shiny.silent.error", "shiny.output.cancel"))) {
        stop(e)
      }

      details <- paste0(
        "oops, could not generate this result in ",
        context,
        ". most likely because of wrong or missing input data",
        if (!is.null(data_hint)) paste0(" (data: ", data_hint, ")") else "",
        ". Error: ",
        conditionMessage(e)
      )

      shiny::showNotification(
        ui = details,
        type = "error",
        duration = NULL,
        closeButton = TRUE
      )

      fallback(e)
    }
  )
}

#' Safe analysis wrapper for plot creation
safe_analysis_plot <- function(expr, context, data_hint = NULL) {
  safe_analysis_step(
    expr = substitute(expr),
    context = context,
    data_hint = data_hint,
    quoted = TRUE,
    fallback = function(e) {
      empty_plot(
        paste0(
          "oops, could not generate this plot,\n",
          "most likely because of wrong or missing input data.\n",
          "Section: ",
          context,
          if (!is.null(data_hint)) paste0("\nData: ", data_hint) else "",
          "\n\nError details: ",
          conditionMessage(e)
        )
      )
    }
  )
}
