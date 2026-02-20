read_count_plot <- function(df) {
  # Tooltips are generated on the fly, allows cleaner download formats
  df <- df %>%
    filter(SampleNameUser != "All samples") %>%
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
    ) %>%
    pivot_longer(
      cols = c(
        AssignedReads,
        UnassignedMappedReads,
        UnassignedUnmappedReads
      ),
      names_to = "ReadType",
      values_to = "NumberOfReads"
    ) %>%
    mutate(
      ReadType = case_when(
        ReadType == "AssignedReads" ~ "Assigned reads",
        ReadType == "UnassignedMappedReads" ~ "Unassigned / mapped reads",
        ReadType == "UnassignedUnmappedReads" ~ "Unassigned / unmapped reads"
      )
    )

  # Display empty plot message, if the sample selection returns an empty dataframe
  if (nrow(df) == 0) {
    return(empty_plot())
  }

  # Plot a stacked bar plot
  p <- ggplot(
    data = df,
    aes(
      x = SampleNameUser,
      y = NumberOfReads,
      fill = ReadType,
      text = TooltipText
    )
  ) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      y = "Total number of reads",
      x = "Sample name",
      fill = ""
    ) +
    facet_wrap(vars(Group), ncol = 1, scales = "free_y", space = "free_y")

  return(p)
}
