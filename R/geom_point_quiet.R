#' @title Quiet version of `ggplot2::geom_point()`
#'
#' @description
#' A wrapper for \code{\link[ggplot2]{geom_point}()} that suppresses warnings, for instance when using `text` aesthetics to map tooltips for ggplotly.
#'
#' @param ... Arguments passed to \code{\link[ggplot2]{geom_point}()}.
#'
#' @return A ggplot2 layer.
#'
#' @export
geom_point_quiet <- function(...) {
  suppressWarnings(geom_point(...))
}
