#' @title Create PCA Plot
#'
#' @description
#' Generates a PCA plot using `ggplot2`.
#'
#' @param df_pca A data frame containing PCA results, including columns for sample names, group assignments, and principal component scores.
#'
#' @param explained_var A data frame with columns `PC` and `Variance`, giving the explained variance for each principal component.
#'
#' @param pc_x Character. The name of the principal component to plot on the x-axis. Default is "PC1".
#'
#' @param pc_y Character. The name of the principal component to plot on the y-axis. Default is "PC2".
#'
#' @param selected_palette Character. The name of the color palette to use for group coloring.
#'
#' @param group_overlay Character. Overlay type for group visualization: "Ellipse" for ellipses with 95 % confidence interval, "Convex hull" for convex hulls, or any of  "none", "no overlay", "remove overlay", "Don't overlay" or "w/o" to hide the group overlay. Default is "Convex hull".
#'
#' @param standalone Logical. If `TRUE`, the PCA plot is generated as a standalone plot, which is not interactive. If `FALSE` (required inside DExploreR), the plot is interactive via `plotly`. Defaults to `FALSE`.
#'
#' @return The PCA plot as `ggplot2` object.
#'
#' @export
createPCAPlot <- function(
  df_pca,
  explained_var,
  pc_x = "PC1",
  pc_y = "PC2",
  selected_palette = "App colors",
  group_overlay = "Convex hull",
  standalone = FALSE
) {
  # Define variables locally for R CMD check
  SampleNameUser <- Group <- .data <- TooltipText <- NULL

  # ---- Validate inputs ----
  match.arg(
    group_overlay,
    choices = c(
      "Ellipse",
      "Convex hull",
      "none",
      "no overlay",
      "remove overlay",
      "w/o",
      "Don't overlay"
    )
  )

  match.arg(
    selected_palette,
    color_choices_flat
  )

  # ---- Prepare tooltip ----
  # This is no harm when used standalone
  df_pca <- df_pca |>
    mutate(
      TooltipText = paste0(
        "<b>Sample name: </b>",
        SampleNameUser,
        "<br><b>Group: </b>",
        Group,
        "<hr><b>",
        pc_x,
        ": </b>",
        sprintf("%.1f", .data[[pc_x]]),
        "<br><b>",
        pc_y,
        ": </b>",
        sprintf("%.1f", .data[[pc_y]])
      )
    )

  # ---- Create the plot ----
  p <- ggplot(
    data = df_pca,
    aes(
      x = .data[[pc_x]],
      y = .data[[pc_y]],
      fill = Group,
      color = Group
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    # Add the PCs as axis labels with explained variance
    labs(
      x = paste0(
        pc_x,
        " (Variance explained: ",
        explained_var[explained_var$PC == pc_x, "Variance"],
        " %)"
      ),
      y = paste0(
        pc_y,
        " (Variance explained: ",
        explained_var[explained_var$PC == pc_y, "Variance"],
        " %)"
      )
    ) +
    guides(
      fill = "none",
      color = guide_legend(title = "")
    )

  # ---- Add group overlay if specified ----
  if (group_overlay == "Ellipse") {
    # Add an statistical grouping by an elliples with 95% CI
    p <- p +
      stat_ellipse(
        color = NA,
        geom = "polygon",
        alpha = 0.2,
        show.legend = FALSE
      )
  } else if (group_overlay == "Convex hull") {
    # Add an statistical grouping by convex hull
    # https://datavizpyr.com/how-to-highlight-groups-with-convex-hull-in-ggplot2/
    p <- p +
      geom_polygon(
        # Calculate the convex hull
        data = df_pca |>
          group_by(Group) |>
          slice(chull(.data[[pc_x]], .data[[pc_y]])),
        aes(
          fill = Group,
          group = Group
        ),
        alpha = 0.2,
        show.legend = FALSE
      )
  }

  # ---- Add the points ----
  # This ensures in the interactive plot that the points are on top of the ellipses or convex hulls
  # And that the hovers work as expected
  p <- p +
    geom_point(
      aes(text = TooltipText),
      fill = NA,
      show.legend = TRUE
    )

  # ---- Add the selected colors ----
  p <- add_selected_colors(p = p, selected_palette = selected_palette)

  if (!standalone) {
    p <- ggplotly(p, tooltip = "text") |>
      layout(
        legend = list(
          orientation = "h",
          yref = "paper",
          yanchor = "bottom",
          y = 1.0,
          xanchor = "center",
          xref = "paper",
          x = 0.5,
          itemclick = FALSE,
          itemdoubleclick = FALSE
        )
      ) |>
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
  } else {
    p <- standalone_plot_style(p)
  }

  return(p)
}
