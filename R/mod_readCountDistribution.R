#' @title Create Read Count Distribution Plot
#'
#' @description
#' Generates a density ridge plot using `ggridges` showing the distribution of read counts across recorded genes for each sample and group. Calculates median read counts per gene for tooltips, highlights quartiles, and facets by group.
#'
#' @param df A data frame containing columns for sample names, group assignments, and read counts per gene.
#'
#' @return A `ggplot2` object, ready for interactive use with `plotly`.
#'
#' @export
createCountDistributionPlot <- function(df) {
  # Define variables locally for R CMD check
  SampleNameUser <- Group <- Counts <- MedianReadCountsPerGene <- TooltipText <- NULL

  # Calculate median read counts per gene for tooltip
  df <- df %>%
    group_by(SampleNameUser) %>%
    mutate(MedianReadCountsPerGene = median(Counts)) %>%
    ungroup() %>%
    mutate(
      TooltipText = paste0(
        "<b>Sample name: </b>",
        SampleNameUser,
        "<br><b>Group: </b>",
        Group,
        "<hr><b>Median read counts per gene: </b>",
        MedianReadCountsPerGene
      )
    )

  # Display empty plot message, if the sample selection returns an empty dataframe
  if (nrow(df) == 0) {
    return(empty_plot())
  }

  p <- ggplot(
    df,
    aes(
      x = Counts,
      y = SampleNameUser,
      fill = factor(after_stat(quantile)),
      text = TooltipText
    )
  ) +
    # Plot density ridges with quartiles
    geom_density_ridges(
      calc_ecdf = TRUE,
      quantile_lines = TRUE,
      quantiles = 4
    ) +
    scale_x_log10() +
    labs(
      x = "Read distribution across recorded genes",
      y = "Sample name"
    ) +
    theme(legend.position = "none") +
    facet_wrap(vars(Group), ncol = 1, scales = "free_y", space = "free_y")

  return(p)
}
