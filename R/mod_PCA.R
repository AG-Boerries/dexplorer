#' @title Create Interactive PCA Plot
#'
#' @description
#' Generates an interactive PCA plot using `ggplot2` and `plotly`.
#'
#' @param df_pca A data frame containing PCA results, including columns for sample names, group assignments, and principal component scores.
#'
#' @param explained_var A data frame with columns `PC` and `Variance`, giving the explained variance for each principal component.
#'
#' @param pc_x Character. The name of the principal component to plot on the x-axis (e.g., "PC1").
#'
#' @param pc_y Character. The name of the principal component to plot on the y-axis (e.g., "PC2").
#'
#' @param selected_palette Character. The name of the color palette to use for group coloring.
#'
#' @param group_overlay Character. Overlay type for group visualization: "Ellipse" for confidence ellipses, "Convex hull" for convex hulls, or `NULL` for no overlay.
#'
#' @return The interactive PCA plot as a `plotly` object.
#'
#' @export
createPCAPlot <- function(
  df_pca,
  explained_var,
  pc_x,
  pc_y,
  selected_palette,
  group_overlay
) {
  # Define variables locally for R CMD check
  SampleNameUser <- Group <- .data <- TooltipText <- NULL

  # Construct the tooltip text
  df_pca <- df_pca %>%
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

  p <- ggplot(
    data = df_pca,
    aes(
      x = .data[[pc_x]],
      y = .data[[pc_y]],
      fill = Group,
      color = Group
    )
  ) +
    geom_point_quiet(
      aes(text = TooltipText),
      fill = NA,
      show.legend = TRUE
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

  # Add group overlays if selected
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
        data = df_pca %>%
          group_by(Group) %>%
          slice(chull(.data[[pc_x]], .data[[pc_y]])),
        aes(
          fill = Group,
          group = Group
        ),
        alpha = 0.2,
        show.legend = FALSE
      )
  }

  # Add the selected color scale to the plot
  p <- add_selected_colors(p = p, selected_palette = selected_palette)

  p <- ggplotly(p, tooltip = "text") %>%
    layout(
      legend = list(
        orientation = "h",
        y = 1.1,
        xanchor = "center",
        xref = "paper",
        x = 0.5
      )
    ) %>%
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
