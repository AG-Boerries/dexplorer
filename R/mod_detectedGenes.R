#' @title Create Gene Count Bar Plot
#'
#' @description
#' Generates a horizontal bar plot showing the number of recorded genes per sample and group. Adds tooltips for interactive display, highlights the combined group, and facets by group.
#'
#' @param df A data frame containing columns for sample names, group assignments, and the number of recorded genes.
#'
#' @return A `ggplot2` object, ready for interactive use with `plotly`.
#'
#' @export
createGeneCountPlot <- function(df) {
  # Define variables locally for R CMD check
  Group <- SampleNameUser <- GenesRecorded <- ColorGroup <- TooltipText <- NULL

  df <- df %>%
    # Add the tooltip text
    mutate(
      # Ensure that "All samples" is the first level
      Group = relevel(as.factor(Group), ref = "All samples"),
      TooltipText = ifelse(
        # Tooltip text shall be different for the combined group
        SampleNameUser == "All samples",
        paste0("<b>", GenesRecorded, " genes recorded in total.</b>"),
        paste0(
          "<b>Sample name: </b>",
          SampleNameUser,
          "<br><b>Group: </b>",
          Group,
          "<hr><b>Recorded genes: </b>",
          GenesRecorded
        )
      ),
      # Add a column for the color mapping
      ColorGroup = ifelse(SampleNameUser == "All samples", "All", "Single")
    )

  p <- ggplot(
    data = df,
    aes(
      x = SampleNameUser,
      y = GenesRecorded,
      fill = ColorGroup,
      text = TooltipText
    )
  ) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      y = "Number of recorded genes",
      x = "Sample name",
      fill = ""
    ) +
    theme(legend.position = "none") +
    facet_wrap(vars(Group), ncol = 1, scales = "free_y", space = "free_y")

  return(p)
}
