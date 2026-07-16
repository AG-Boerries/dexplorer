#' @title Format Data for DGEA Contrast Intersection
#'
#' @description
#' Formats a data frame of differentially expressed genes (DEGs) for Jaccard index calculation between pairs of contrasts. Filters by direction, p-value, and log2 fold-change thresholds, computes gene intersections/unions, and returns a summary table with gene lists and statistics for each pair.
#'
#' @param df A data frame containing DEG information, including columns for gene IDs, contrasts, direction, adjusted p-values, and log2 fold changes.
#'
#' @param p_threshold Numeric. The adjusted p-value threshold for filtering DEGs.
#'
#' @param l2fc_threshold Numeric. The log2 fold-change threshold for filtering DEGs.
#'
#' @return A data frame summarizing Jaccard index results for each pair of contrasts and direction, including gene lists and statistics.
#'
#' @export
formatDGEAContrastIntersection <- function(
  df,
  p_threshold = 0.05,
  l2fc_threshold = 1
) {
  # Define variables locally for R CMD check
  Direction <- PValAdj <- Log2FC <- Contrast <- GeneID <- Var1 <- Var2 <- Contrast.a <- Contrast.b <- . <- NULL

  # Possible directions
  directions <- c("up", "down", "both")

  # For the calculation of the Jaccard index, at least two contrasts are required
  if (length(unique(df$Contrast)) < 2) {
    return(data.frame())
  }

  # Prepare data frame for ballon plot
  df_jaccard_results <- bind_rows(
    lapply(directions, function(dir) {
      df_jaccard <- df |>
        filter(
          # Filter by direction
          (dir == "both" | Direction == dir),
          # Get the user-defined thresholds
          PValAdj < as.numeric(p_threshold),
          abs(Log2FC) > l2fc_threshold
        ) |>
        group_by(Contrast) |>
        # Extract the unique list of genes for each contrast
        summarise(GeneID = list(unique(GeneID)), .groups = "drop")

      # Calculate the possible combinations of contrasts
      pairs <- base::expand.grid(
        df_jaccard$Contrast,
        df_jaccard$Contrast,
        stringsAsFactors = FALSE
      ) |>
        # This removes duplicates and self-comparisons
        filter(Var1 < Var2)

      # Compute Jaccard index for each pair
      jaccard_results <- map_dfr(1:base::nrow(pairs), function(i) {
        seta <- df_jaccard |> filter(Contrast == pairs[i, 1])
        setb <- df_jaccard |> filter(Contrast == pairs[i, 2])

        inter <- length(base::intersect(seta$GeneID[[1]], setb$GeneID[[1]]))
        uni <- length(base::union(seta$GeneID[[1]], setb$GeneID[[1]]))

        df_genes <- full_join(
          # Combine the gene ids from both sets
          unnest(seta, cols = "GeneID"),
          unnest(setb, cols = "GeneID"),
          by = "GeneID",
          suffix = c(".a", ".b"),
        ) |>
          # Create two columns with the names of the contrasts
          # Each row is a gene and check, which genes are differentially expressed in which contrast
          mutate(
            !!seta$Contrast := ifelse(
              !is.na(Contrast.a),
              TRUE,
              FALSE
            ),
            !!setb$Contrast := ifelse(
              !is.na(Contrast.b),
              TRUE,
              FALSE
            )
          ) |>
          # Remove the original contrast columns
          dplyr::select(-c(Contrast.a, Contrast.b)) |>
          # Sort genes, so that differentially expressed genes appear on top
          arrange(desc(.[2]), desc(.[3])) |>
          # Add further gene information for tooltip
          left_join(
            df |>
              dplyr::select(all_of(c(
                "GeneID",
                "Symbol",
                "Alias",
                "EntrezID",
                "Description",
                "NCBIURL"
              ))) |>
              # This dataframe is inflated, because of the occurance of the same gene in multiple comparisons
              distinct(.keep_all = TRUE),
            by = "GeneID"
          )

        # Rearrange column order using indices because column names are user-defined contrast names
        df_genes <- df_genes[, c(2, 3, 1, 4, 5, 6, 7, 8)]

        tibble(
          Seta = seta$Contrast,
          Setb = setb$Contrast,
          DEG_both_sets = inter,
          DEG_total = uni,
          JI = inter / uni,
          Direction = dir,
          # To not expand the dataframe, save it as a list
          Genes = list(df_genes)
        )
      })
    })
  ) |>
    as.data.frame()

  return(df_jaccard_results)
}
