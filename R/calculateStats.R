#' @title Extract Alignment Statistics For STAR
#'
#' @param Tab This is a single ReadsPerGene.out.tab file as a data frame, after read by \code{\link[readr]{read_tsv}()} from the `readr` package.
#'
#' @return A data data frame with a single row, containing the sequencing statistics per ReadsPerGene.out.tab tab. This is only used internally by \code{\link{createRawCountsWithStats}()}.
#'
calculateStats <- function(Tab) {
  # Define variables locally for R CMD check
  GeneID <- Counts <- NULL

  # Each tab contains 4 rows with:
  # number of unmapped reads (N_unmapped),
  # number of multimapping reads (N_multimapping),
  # number of reads not overlapping any gene (N_noFeature),
  # number of reads overlapping multiple genes (N_ambiguous).
  # The residual rows contain gene IDs usually starting with "ENS" for Ensembl

  # Sum of all reads assigned to known genes
  AssignedReads <- sum(
    Tab %>% filter(str_detect(GeneID, "^ENS")) %>% dplyr::select(-GeneID)
  )

  # Sum of all reads
  TotalReads <- AssignedReads +
    sum(Tab %>% filter(!str_detect(GeneID, "^ENS")) %>% dplyr::select(-GeneID))

  # Sum of all mapped reads
  MappedReads <- AssignedReads +
    sum(
      Tab %>%
        filter(GeneID %in% c("N_noFeature", "N_ambiguous")) %>%
        dplyr::select(-GeneID)
    )

  # Number of recorded genes with at least one assigned read
  GenesRecorded <- nrow(
    Tab %>% filter(str_detect(GeneID, "^ENS"), Counts > 0)
  )

  return(
    data.frame(
      UnassignedUnmappedReads = TotalReads - MappedReads,
      UnassignedMappedReads = MappedReads - AssignedReads,
      AssignedReads = AssignedReads,
      TotalReads = TotalReads,
      GenesRecorded = GenesRecorded
    )
  )
}
