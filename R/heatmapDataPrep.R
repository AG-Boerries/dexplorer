#' @title Format Data for Gene Expression Heatmap
#'
#' @description
#' Prepares and formats a gene expression data frame for a heatmap. Selects top genes by median or variance, ensures user-selected genes are included, removes duplicates, z-scores expression values, and adds gene annotation columns for tooltips and downloads.
#'
#' @param df A data frame containing Z-scored gene expression values (CPMs) of all samples and genes and additional gene annotation columns.
#'
#' @param selected_samples Character vector of sample names to include in the heatmap. Defaults to "keep all", which retains all samples.
#'
#' @param selected_subset_size Integer. Number of top genes to select based on median or variance. Defaults to 20.
#'
#' @param selected_genes Character vector of gene symbols to always include in the heatmap. This in only relevant in the DExploreR app. Defaults to `NULL`.
#'
#' @param gene_selection_by Logical. If `TRUE`, select top genes by variance; if `FALSE`, by median expression. Defaults to `TRUE`.
#'
#' @return A data frame formatted for \code{\link[heatmaply]{heatmaply}()}, including z-scored expression values and gene annotation columns.
#'
#' @export
formatForHeatmap <- function(
  df,
  selected_samples = "keep all",
  selected_subset_size = 20,
  selected_genes = NULL,
  gene_selection_by = TRUE
) {
  # Define variables locally for R CMD check
  Symbol <- .data <- GeneID <- EntrezID <- Description <- Alias <- NCBIURL <- NULL

  # ---- Check input parameters for validity ----
  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "selected_samples must be a character vector of sample names or 'keep all'" = is.character(
      selected_samples
    ) &&
      all(selected_samples %in% c("keep all", colnames(df))),
    "selected_subset_size must be a non-negative integer" = is.numeric(
      selected_subset_size
    ) &&
      selected_subset_size >= 0,
    "selected_genes must be a character vector of gene symbols or NULL" = is.null(
      selected_genes
    ) ||
      is.character(selected_genes),
    "gene_selection_by must be a boolean" = is.logical(gene_selection_by)
  )

  # ---- Always included user-selected genes ----
  # Only relevant in DExploreR, where this comes from the corresponing UI input
  df_genes_selected <- if (
    !is.null(selected_genes) && length(selected_genes) > 0
  ) {
    df |> filter(Symbol %in% selected_genes)
  } else {
    # If selection is empty, then create an empty data frame with the same columns as `df`
    df[0, ]
  }

  # ---- Prepare data for `heatmaply::heatmaply()` ----
  df <- df |>
    # Show only top n genes with highest median expression or highest variance
    slice_max(
      order_by = .data[[
        if (gene_selection_by) "Rowvariance" else "Rowmedian"
      ]],
      n = selected_subset_size
    ) |>
    # Add the user selected genes
    bind_rows(df_genes_selected) |>
    # Ensure to remove duplicates, if genes were already in top n
    distinct(Symbol, .keep_all = TRUE) |>
    column_to_rownames(var = "Symbol") |>
    # Remove unselected samples and non-numeric columns for z-scoring
    (\(x) {
      if (identical(selected_samples, "keep all")) {
        select(x, where(is.numeric), -Rowvariance, -Rowmedian)
      } else {
        select(x, all_of(selected_samples))
      }
    })() |>
    # Z-score cpm values for better depiction
    base::t() |>
    scale() |>
    base::t() |>
    as.data.frame() |>
    rownames_to_column(var = "Symbol") |>
    # Re-add the further information, required for hover labels and comprehensive downloadable .csv
    left_join(
      df |>
        dplyr::select(
          Symbol,
          GeneID,
          EntrezID,
          Description,
          Alias,
          NCBIURL
        ),
      by = "Symbol"
    )

  return(df)
}
