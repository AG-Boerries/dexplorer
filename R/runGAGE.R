#' @title Run GAGE for Gene Set Enrichment Analysis
#'
#' @description
#' This function is a wrapper for \code{\link[gage]{gage}()} from the `gage` package using the provided contrasts and samples.
#'
#' @param condition The name of the first contrast, i.e. the condition.
#' @param control The name of the second contrast, i.e. the control.
#' @param norm_count The normalized count matrix sample names as columns and row names as gene IDs, subset to the well-annotate genes.
#' @param gene_sets A list of gene sets with genes as ensembl IDs. These will be analaysed for enrichment.
#' @param samples_groups A list of contrasts with assigned sample names.
#'
#' @return A tidy data frame with significantly enriched (up- and down-regulated) gene sets. The column names match the ones used by DExploreR for compatibility.
#'
runGAGE <- function(
  condition,
  control,
  norm_count,
  gene_sets,
  samples_groups
) {
  # Define variables locally for R CMD check
  gsets <- pathway <- p.geomean <- stat.mean <- p.val <- q.val <- direction <- contrast <- set.size <- Direction <- Contrast <- NULL

  # Get the sample names for the each group
  condition_samples <- samples_groups[[condition]]
  control_samples <- samples_groups[[control]]

  # Convert sample names into column indices
  condition_idx <- match(condition_samples, colnames(norm_count))
  control_idx <- match(control_samples, colnames(norm_count))

  # Run GAGE
  res <- gage(
    exprs = norm_count,
    gsets = gene_sets,
    ref = control_idx,
    samp = condition_idx,
    # Use unpaired test since samples are independent and can also vary in number
    compare = "unpaired",
    rank.test = TRUE
  )

  # Extract the significant gene sets
  res <- sigGeneSet(
    res,
    qpval = "q.val",
    cutoff = 0.05,
    heatmap = FALSE
  )

  # Combine up and down regulated genes sets
  df <- bind_rows(
    res$greater %>%
      as.data.frame() %>%
      rownames_to_column("pathway") %>%
      mutate(Direction = "up"),
    res$less %>%
      as.data.frame() %>%
      rownames_to_column("pathway") %>%
      mutate(Direction = "down")
  ) %>%
    mutate(
      Contrast = paste0(condition, "_vs_", control)
    ) %>%
    # Select and rename relevant columns
    dplyr::select(
      "Pathway" = "pathway",
      "EnrichmentScore" = "stat.mean",
      "Pval" = "p.val",
      "QVal" = "q.val",
      Direction,
      Contrast,
      "SetSize" = "set.size"
    )

  return(df)
}
