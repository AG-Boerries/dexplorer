#' @title Prepare data form the gene set enrichment plot
#'
#' @description
#' Filters for provided contrasts and collections, then slices for top n absolutely enriched gene sets or when specific gene sets are provided, filters for those.
#'
#' @param df A data frame containing gene set enrichment results, including enrichment scores, set sizes, direction and also a column with the genes of the gene set as a nested tibble.
#'
#' @param selected_contrast Character vector. Contrast names to include in the plot. Defaults to "all".
#'
#' @param selected_gene_sets Character vector. Gene set names to include in the plot. Defaults to `character(0)`, so by default does not filter on specific gene sets.
#'
#' @param selected_collections Character vector. Gene set collection names to include in the plot. Defaults to "all".
#'
#' @param top_gene_sets Numeric. The number of top absolute enriched gene sets to slice per contrast. Defaults to 15.
#'
#' @return A data frame filtered and prepared for plotting.
#'
#' @export
formatForGeneSetsPlot <- function(
  df,
  selected_contrast = "all",
  selected_gene_sets = character(0),
  top_gene_sets = 15,
  selected_collections = "all"
) {
  # Define variables locally for R CMD check
  # Contrast <- NULL

  # ---- Prepare data ----
  if (length(selected_gene_sets) == 0) {
    # If not specific gene sets selected, apply contrast and collection filter
    # And slice top n absolute enriched genesets per contrast
    df <- df |>
      # Filter for selected contrasts, if selected
      (\(x) {
        if (!identical(selected_contrast, "all")) {
          filter(x, Contrast %in% selected_contrast)
        } else {
          x
        }
      })() |>
      # Filter to selected genes, if selected
      (\(x) {
        if (!identical(selected_collections, "all")) {
          filter(x, GSCollectionName %in% selected_collections)
        } else {
          x
        }
      })() |>
      # Group by contrast and selected the absolute top n gene sets
      group_by(Contrast) |>
      slice_max(
        order_by = abs(EnrichmentScore),
        n = top_gene_sets,
        with_ties = FALSE
      ) |>
      ungroup()
  } else {
    # If specific gene sets selected, only filter for those
    df <- df |>
      filter(Pathway %in% selected_gene_sets)
  }

  return(df)
}
