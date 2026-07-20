#' @title Format Top Differentially Expressed Genes (DEGs) for Bar Plot
#'
#' @description
#' Selects and formats the top differentially expressed genes (DEGs) for each contrast based on user-defined direction, number of genes, and ranking by log2 fold change or adjusted p-value. Applies grouping, filtering, and ordering, and prepares gene symbols for faceted plotting.
#'
#' @param df A data frame containing DEG information, including columns for gene symbols, log2 fold change, adjusted p-value, direction, and contrast.
#'
#' @param selected_contrast Character vector. Contrast names to include. Defaults to "all", which retains all contrasts.
#'
#' @param selected_number_of_genes Integer. Number of top genes to select per contrast. Defaults to 10.
#'
#' @param selected_direction Character. If "up", displays top up-regulated genes; if "down", displays top down-regulated genes; if "both", displays top differentially expressed genes across both directions. Defaults to "both".
#'
#' @param fc_or_pvalue Logical. If TRUE, reorders each contrast by absolute log2 fold change; if FALSE, by -log10 adjusted p-value. The ordered value will be put on the x-axis of the bar plot.Defaults to TRUE.
#'
#' @return A data frame of top DEGs per contrast, formatted for \code{\link{createTopDEGsPlot}()}.
#'
#' @export
formatTopDEGs <- function(
  df,
  selected_contrast = "all",
  selected_number_of_genes = 10,
  selected_direction = "both",
  fc_or_pvalue = TRUE
) {
  # Define variables locally for R CMD check
  Log2FC <- Direction <- Contrast <- Symbol <- LogPValAdj <- NULL

  # Validate the direction
  match.arg(selected_direction, choices = c("up", "down", "both"))

  # Extract the column to order the data by
  order_col <- if (fc_or_pvalue) "Log2FC" else "LogPValAdj"

  df <- df |>
    # Filter for the selected contrast(s) if they are specified
    # Otherwise keep all contrasts
    (\(x) {
      if (!identical(selected_contrast, "all")) {
        filter(x, Contrast %in% selected_contrast)
      } else {
        x
      }
    })() |>
    # Group by contrast, to get top genes per contrast
    group_by(Contrast) |>
    # When ordering by p-value, pre-filter by direction upfront
    (\(x) {
      if (order_col == "LogPValAdj" && selected_direction != "both") {
        filter(x, Direction == selected_direction)
      } else {
        x
      }
    })() |>
    (\(x) {
      # Filter by log2FC
      if (order_col == "Log2FC") {
        # Use slice_max() to get top genes in condition
        if (selected_direction == "up") {
          slice_max(
            x,
            order_by = Log2FC,
            n = selected_number_of_genes,
            with_ties = FALSE
          )
          # Use slice_min() to get top genes in control
        } else if (selected_direction == "down") {
          slice_min(
            x,
            order_by = Log2FC,
            n = selected_number_of_genes,
            with_ties = FALSE
          )
        } else {
          # If both, then get the top genes in both directions by absolute log2FC
          slice_max(
            x,
            order_by = abs(Log2FC),
            n = selected_number_of_genes,
            with_ties = FALSE
          )
        }
      } else {
        # When ordering by p-value, get top genes by -log10 adjusted p-value
        # Filtering happens earlier because the p-value is > 0 for all genes
        slice_max(
          x,
          order_by = LogPValAdj,
          n = selected_number_of_genes,
          with_ties = FALSE
        )
      }
    })() |>
    ungroup() |>
    mutate(
      # Sort the symbols within each group using `tidytext::reorder_within()`
      Symbol = reorder_within(
        Symbol,
        !!sym(order_col),
        Contrast
      ),
      # Add a column to indicate the order of the genes for plotting
      ordered_by = order_col
    )

  return(df)
}
