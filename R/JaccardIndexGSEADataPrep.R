#' @title Format Data for GSEA Contrast Intersection
#'
#' @description
#' Formats a data frame of enriched gene sets for Jaccard index calculation between pairs of contrasts. Returns a summary table with gene set lists and statistics for each pair.
#'
#' @param df A data frame containing GSEA information, including columns for contrasts, direction and patways.
#'
#' @return A data frame summarizing Jaccard index results for each pair of contrasts and direction, including gene set lists and statistics.
#'
#' @export
formatGSEAContrastIntersection <- function(df) {
  # Define variables locally for R CMD check
  Direction <- Contrast <- Pathway <- Var1 <- Var2 <- Contrast.a <- Contrast.b <- NULL

  # Possible directions
  directions <- c("up", "down", "both")

  # For the calculation of the Jaccard index, at least two contrasts are required
  if (length(unique(df$Contrast)) < 2) {
    return(data.frame())
  }

  # ---- Prepare data ----
  df_jaccard_results <- bind_rows(
    lapply(directions, function(dir) {
      df_jaccard <- df |>
        # Filter for direction
        filter((dir == "both" | Direction == dir)) |>
        group_by(Contrast) |>
        # Extract the unique list of pathways for each contrast
        summarise(paths = list(unique(Pathway)), .groups = "drop")

      # ---- Calculate the possible combinations of contrasts ----
      pairs <- base::expand.grid(
        df_jaccard$Contrast,
        df_jaccard$Contrast,
        stringsAsFactors = FALSE
      ) |>
        # This removes duplicates and self-comparisons
        filter(Var1 < Var2)

      # ---- Compute Jaccard index for each pair ----
      jaccard_results <- map_dfr(1:base::nrow(pairs), function(i) {
        seta <- df_jaccard |> filter(Contrast == pairs[i, 1])
        setb <- df_jaccard |> filter(Contrast == pairs[i, 2])

        # Get the metrics to calculate the Jaccard index
        inter <- length(base::intersect(seta$paths[[1]], setb$paths[[1]]))
        uni <- length(base::union(seta$paths[[1]], setb$paths[[1]]))

        # Create a data frame with the results for this pair of contrasts
        df_paths <- full_join(
          # Combine the pathways from both sets
          unnest(seta, cols = "paths"),
          unnest(setb, cols = "paths"),
          by = "paths",
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
          ) |> # Remove the original contrast columns
          dplyr::select(-c(Contrast.a, Contrast.b)) |>
          # Sort genes, so that differentially expressed genes appear on top
          arrange(across(c(2, 3), desc))

        # Rearrange column order using indices because column names are user-defined contrast names
        df_paths <- df_paths[, c(2, 3, 1)]

        tibble(
          Seta = seta$Contrast,
          Setb = setb$Contrast,
          Pathways_both_sets = inter,
          Pathways_total = uni,
          JI = inter / uni,
          Direction = dir,
          Pathways = list(df_paths)
        )
      })
    })
  ) |>
    as.data.frame()

  return(df_jaccard_results)
}
