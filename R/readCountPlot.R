#' @title Create Read Count Stacked Bar Plot
#'
#' @description
#' Generates a horizontal stacked bar plot showing the distribution of assigned, unassigned/mapped, and unassigned/unmapped reads for each sample and group. Tooltips provide detailed read statistics per sample.
#'
#' @param df A data frame containing columns for sample names, group assignments, assigned reads, unassigned mapped reads, unassigned unmapped reads, and total reads.
#'
#' @param standalone Logical. If `TRUE`, the plot is generated as a standalone plot. If `FALSE` (required inside DExploreR). Defaults to `FALSE`.
#'
#' @return A `ggplot2` object, ready for interactive use with `plotly`.
#'
#' @export
createReadCountPlot <- function(df, standalone = FALSE) {
  # Define variables locally for R CMD check
  SampleNameUser <- Group <- AssignedReads <- UnassignedMappedReads <- UnassignedUnmappedReads <- TotalReads <- NumberOfReads <- ReadType <- TooltipText <- NULL

  # This can be an empty dataframe, when the data was not aligned with STAR
  if (nrow(df) == 0) {
    return(empty_plot(
      message = "No read statistics available. This can happen when your sequencing run was not aligned with STAR."
    ))
  }

  # Tooltips are generated on the fly, allows cleaner download formats
  df <- df |>
    filter(SampleNameUser != "All samples") |>
    mutate(
      TooltipText = paste0(
        "<b>Sample name: </b>",
        SampleNameUser,
        "<br><b>Group: </b>",
        Group,
        "<hr><b>Assigned reads: </b>",
        sprintf("%.1f", AssignedReads / TotalReads * 100),
        " %",
        "<br><b>Unassigned / mapped reads: </b>",
        sprintf("%.1f", UnassignedMappedReads / TotalReads * 100),
        " %",
        "<br><b>Unassigned / unmapped reads: </b>",
        sprintf("%.1f", UnassignedUnmappedReads / TotalReads * 100),
        " %"
      )
    ) |>
    pivot_longer(
      cols = c(
        AssignedReads,
        UnassignedMappedReads,
        UnassignedUnmappedReads
      ),
      names_to = "ReadType",
      values_to = "NumberOfReads"
    ) |>
    mutate(
      ReadType = case_when(
        ReadType == "AssignedReads" ~ "Assigned reads",
        ReadType == "UnassignedMappedReads" ~ "Unassigned / mapped reads",
        ReadType == "UnassignedUnmappedReads" ~ "Unassigned / unmapped reads"
      )
    )

  # This can also be an empty dataframe, when the user filtered out all samples
  if (nrow(df) == 0) {
    return(empty_plot())
  }

  # Plot a stacked bar plot
  p <- ggplot(
    data = df,
    aes(
      x = NumberOfReads,
      y = SampleNameUser,
      fill = ReadType,
      text = TooltipText
    )
  ) +
    geom_col() +
    labs(
      x = "Total number of reads",
      y = "Sample name",
      fill = ""
    ) +
    facet_wrap(vars(Group), ncol = 1, scales = "free_y", space = "free_y")

  # ---- Fine tune plot for usage outside of DExploreR ----
  if (standalone) {
    p <- p +
      theme(
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        legend.position = "top",
        strip.background = element_rect(fill = "white")
      )

    p <- add_selected_colors(p = p, selected_palette = "App colors")
  }

  return(p)
}
