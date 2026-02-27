#' @title Get Gene Symbols
#'
#' @description
#' Uses a given list of Ensembl Gene IDs to query the appropriate annotation database (mouse or human) and retrieve gene symbols, Entrez IDs, descriptions, and aliases.
#'
#' @import org.Mm.eg.db
#' @import org.Hs.eg.db
#'
#' @param raw_counts A data frame containing the raw counts, must contain a `GeneID` column containing ensembl identifiers. This can be for instance `RawCounts` from \code{\link{createRawCountsWithStats}()}.
#'
#' @param species Specify the species for which the gene symbols should be retrieved. At the moment, options are "mouse" or "human". Default is "mouse".
#'
#' @return A list containing:
#'
#' * `Symbols`: A data frame containing the following columns `GeneID`, `Symbol`, `EntrezID`, `Description`, and `Alias`. `GeneID`s can be duplicated when muliple aliases exist, thus, this is an inflated dataframe but it comes in handy for the alias-based search in |Raw data| -> |Gene expression heatmap|. Gene IDs with missing symbols or missing entrez IDs are removed as well as poorly annotated genes (defined by the occurance of the terms "cdna", "pseudogene" or "predicted" in `Description`).
#'
#' * `CountsSymbols`: The cleaned, depulicated count data frame with unique symbols, ensemble and entrez IDs. Deduplication of symbols is performed using \code{\link[stats]{mad}()} from `stats`.
#'
#' @export
addGeneSymbols <- function(raw_counts, species = c("mouse", "human")) {
  # Define variables locally for R CMD check
  SYMBOL <- ENSEMBL <- ENTREZID <- GENENAME <- ALIAS <- Symbol <- GeneID <- EntrezID <- Description <- Alias <- NCBIURL <- RunID <- Counts <- MAD <- NULL

  # Check if the user specified a correct value for `species`
  species <- match.arg(species)

  # Assign required database
  if (species == "mouse") {
    db <- org.Mm.eg.db::org.Mm.eg.db
  } else {
    db <- org.Hs.eg.db::org.Hs.eg.db
  }

  # Query the database for gene symbols
  GeneSymbols <- AnnotationDbi::select(
    db,
    # When using `RawCounts$GeneID` from `createRawCountsWithStats()`, IDs will be mulitplicated by the number of samples.
    # Thus, using only unique IDs here is sufficient.
    keys = unique(raw_counts$GeneID),
    columns = c(
      "SYMBOL",
      "ENTREZID",
      "GENENAME",
      # This is very important to capture all different names a user might associate with a gene
      "ALIAS"
    ),
    keytype = "ENSEMBL"
  ) %>%
    dplyr::rename(
      GeneID = ENSEMBL,
      Symbol = SYMBOL,
      EntrezID = ENTREZID,
      Description = GENENAME,
      Alias = ALIAS
    ) %>%
    filter(
      # Remove genes with missing symbols or missing entrez IDs
      !is.na(Symbol),
      !is.na(EntrezID),
      # Remove poorly annotated genes
      !str_detect(Description, regex("cdna", ignore_case = TRUE)),
      !str_detect(Description, regex("predicted", ignore_case = TRUE)),
      !str_detect(Description, regex("pseudogene", ignore_case = TRUE))
    ) %>%
    mutate(
      # Add the URL to NCBI for each entrez ID
      # The URLs have the format: https://www.ncbi.nlm.nih.gov/datasets/gene/ENTREZID/
      # Sometimes some links don't work, this may be related to limited government funding for NIH
      NCBIURL = paste0(
        "https://www.ncbi.nlm.nih.gov/datasets/gene/",
        EntrezID,
        "/"
      ),
      # Ensure that entrez IDs are characters, so that they are used in calculations with numeric columns
      EntrezID = as.character(EntrezID)
    ) %>%
    as.data.frame()

  # Create another data frame with aliases collapsed into a single string per GeneID
  # This is required for some hover information
  GeneSymbolsCollapsed <- GeneSymbols %>%
    group_by(GeneID) %>%
    summarise(
      # Collapse all aliases and different symbols of the same ID into a single string
      Alias = paste(na.omit(unique(c(Alias, Symbol))), collapse = ", "),
      # Collapse all other Entrez IDs into a single string
      OtherEntrezIDs = paste(na.omit(unique(EntrezID)), collapse = ", "),
      # From the remaining columns, just take the first entry
      # This creates a unique row per gene ID, while symbols can still be duplicated
      Symbol = first(Symbol),
      EntrezID = first(EntrezID),
      Description = first(Description),
      NCBIURL = first(NCBIURL),
      .groups = "drop"
    ) %>%
    # Add the counts for each gene ID
    left_join(raw_counts, by = "GeneID", relationship = "one-to-many")

  # Get duplicated symbols
  DuplicatedSymbols <- GeneSymbolsCollapsed %>%
    group_by(RunID, Symbol) %>%
    filter(n() > 1) %>%
    pull(Symbol) %>%
    unique()

  # For duplicated symbols, keep only the gene ID with the highest MAD across samples
  DuplicatedSymbolsCleaned <- GeneSymbolsCollapsed %>%
    filter(Symbol %in% DuplicatedSymbols) %>%
    group_by(Symbol, GeneID) %>%
    summarise(MAD = mad(Counts)) %>%
    ungroup() %>%
    group_by(Symbol) %>%
    # Avoid ties and return first row in this case
    slice_max(order_by = MAD, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-MAD)

  # Finally, remove the duplicated symbols
  GeneSymbolsCollapsedDeduplicated <- GeneSymbolsCollapsed %>%
    semi_join(DuplicatedSymbolsCleaned, by = c("Symbol", "GeneID")) %>%
    bind_rows(
      GeneSymbolsCollapsed %>%
        # Keep symbols that were never duplicated
        anti_join(DuplicatedSymbolsCleaned, by = "Symbol")
    ) %>%
    as.data.frame()

  return(list(
    Symbols = GeneSymbols,
    CountsSymbols = GeneSymbolsCollapsedDeduplicated
  ))
}
