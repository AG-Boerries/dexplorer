#' @title Calculate Gene Set Enrichment Analysis
#'
#' @description
#' This function performs Gene Set Enrichment Analysis (GSEA) using the \code{\link[gage]{gage}()} from the `gage` based on provided normalized count data or using \code{\link[clusterProfiler]{GSEA}()} from the `clusterProfiler` on provided differential expression analysis tables. Both tables can be obtained from \code{\link{calculateDEG}()}. It supports both mouse and human gene sets, which are collected by the `msigdbr` package.
#'
#' @param normalized_count_data A data frame with normalized counts with sample names as column names and an additional column called `GeneID`. This data frame can be `NormData` from \code{\link{calculateDEG}()}, when using `deg_tool = "limma"`. Provide this data frame if you want to use \code{\link[gage]{gage}()} for GSEA.
#'
#' @param dge_table A data frame with the results of the differential expression analysis containing at least the columns `GeneID`, `t`, and `Contrast`. This data frame can be `DEGs` from \code{\link{calculateDEG}()}, when using `deg_tool = "limma"`. Provide this data frame if you want to use \code{\link[clusterProfiler]{GSEA}()} for GSEA.
#'
#' @param species Select the species for gene set enrichment analysis. Options are `mouse` (default) or `human`.
#'
#' @param collection Select the collection of gene sets for enrichment analysis. Options are `H` (hallmark gene sets), `GO:BP` (Gene Ontology Biological Process), `GO:CC` (Gene Ontology Cellular Component), and `GO:MF` (Gene Ontology Molecular Function). Multiple collections can be selected at once by providing a vector.
#'
#' @param contrasts The contrasts on which to perform GESA. This can be the exact same vector used by \code{\link{calculateDEG}()}.
#'
#' @param samples_groups A data frame assiging samples to groups. For instance, this can be `SamplesGroups` from \code{\link{prepareDfs}()}.
#'
#' @return A list containing:
#'
#' * `ResultsGAGE`: A tidy data frame with the GSEA results (significantly up- and down-regulated gene sets) for all defined contrast.
#'
#' * `SignificantGeneSetsGenes`: A data frame containing the gene symbols and IDs contained in the significantly enriched gene sets with links to data bases.
#'
#' @export
calculateGSEA <- function(
  normalized_count_data = NULL,
  dge_table = NULL,
  species = c("mouse", "human"),
  collection = c("H", "GO:BP", "GO:CC", "GO:MF"),
  contrasts,
  samples_groups
) {
  # Define variables locally for R CMD check
  Symbol <- Description <- GeneID <- . <- contrast <- Group <- SampleNameUser <- ensembl_gene <- gene_symbol <- gs_collection <- gs_collection_name <- gs_description <- gs_name <- gs_subcollection <- gs_url <- NCBIURL <- Alias <- Contrast <- GSName <- OtherEntrezIDs <- EntrezID <- ncbi_gene <- NES <- Direction <- Rank <- NULL

  # Check arguments
  species <- match.arg(species)
  collection <- match.arg(collection, several.ok = TRUE)

  # Prevent user from providing both variables
  # The presence of one variable later defines which method to use for GSEA
  if (is.null(normalized_count_data) && is.null(dge_table)) {
    stop("Please provide either `normalized_count_data` or `dge_table`.")
  }

  if (!is.null(normalized_count_data) && !is.null(dge_table)) {
    stop(
      "Please provide either `normalized_count_data` or `dge_table`, not both."
    )
  }

  # For mouse, hallmark gene sets are called "MH" in `msigdbr`
  if ("H" %in% collection && species == "mouse") {
    collection[which(collection == "H")] <- "MH"
  }

  # Get the genes per gene set of interest
  GeneSets <- bind_rows(
    lapply(collection, function(coll) {
      # Hallmark gene sets have no subcollection and need to be retrieved by collection
      if (coll == "H" || coll == "MH") {
        msigdbr(
          db_species = ifelse(species == "mouse", "MM", "HS"),
          species = species,
          collection = coll
        )
      } else {
        # The GO terms are subcollection existing for both species
        msigdbr(
          db_species = ifelse(species == "mouse", "MM", "HS"),
          species = species,
          subcollection = coll
        )
      }
    })
  )

  # Create named list of gene sets with GeneIDs as values
  GeneSetsList <- GeneSets %>%
    split(.$gs_name) %>%
    lapply(function(x) unique(x$ensembl_gene))

  # Run GSEA with `gage()` if normalized count data is provided
  if (!is.null(normalized_count_data)) {
    message("Running GSEA with `gage()` from the `gage` package.")

    # Extract gene information from the normalized count data
    WellAnnotatedGenes <- normalized_count_data %>%
      dplyr::select(GeneID, Alias:NCBIURL)

    # Extract only the normalized counts for GSEA
    NormCountData <- normalized_count_data %>%
      dplyr::select(-Alias:-NCBIURL) %>%
      column_to_rownames(var = "GeneID")

    # Use the same vector for the contrast as for DGEA
    # Transform it so that `gage()` can use it
    Contrasts <- data.frame(contrast = contrasts) %>%
      separate(contrast, into = c("condition", "control"), sep = "-")

    # Turn data frame into list of lists, each containing sample names for a group
    SamplesGroups <- samples_groups %>%
      group_by(Group) %>%
      summarise(samples = list(SampleNameUser)) %>%
      deframe()

    # Run `gage()` for each set of contrast provided by the user
    ResultsGAGE <- map2(
      Contrasts[, "condition"],
      Contrasts[, "control"],
      ~ runGAGE(
        condition = .x,
        control = .y,
        norm_count = NormCountData,
        gene_sets = GeneSetsList,
        samples_groups = SamplesGroups
      )
    )

    # Change the `-` in the contrasts names to `_vs_`, which is what `gage()` outputs
    contrasts <- gsub("-", "_vs_", contrasts)

    # Create tidy data frame
    ResultsGAGEDf <- bind_rows(ResultsGAGE) %>%
      mutate(
        # Use the names of the contrasts provided by the user for better readability in DExploreR
        Contrast = recode(
          Contrast,
          !!!setNames(names(contrasts), unname(contrasts))
        )
      )

    # Data frame containing the gene symbols and ID contained in the gene sets
    GeneSetsSig <- GeneSets %>%
      # Select and rename only relevant columns
      dplyr::select(
        "Symbol" = "gene_symbol",
        "GeneID" = "ensembl_gene",
        "GSName" = "gs_name",
        "GSCollection" = "gs_collection",
        "GSSubcollection" = "gs_subcollection",
        "GSCollectionName" = "gs_collection_name",
        "GSDescription" = "gs_description",
        "GSURL" = "gs_url"
      ) %>%
      # Subset to significantly enriched gene sets only
      filter(GSName %in% unique(ResultsGAGEDf$Pathway)) %>%
      # Also add the information for each individual gene, if available
      left_join(
        WellAnnotatedGenes %>% dplyr::select(-Symbol),
        by = "GeneID"
      ) %>%
      select(-OtherEntrezIDs) %>%
      as.data.frame()

    return(
      list(
        Results = ResultsGAGEDf,
        SignificantGeneSetsGenes = GeneSetsSig
      )
    )
  }

  # `clusterProfiler::GSEA()` requires a ranked gene list, which can be obtained from the DGE table. The `t` statistic is used for ranking, which is a common choice for GSEA.
  if (!is.null(dge_table)) {
    message("Running GSEA with `GSEA()` from the `clusterProfiler` package.")

    # Extract gene information from the DGE table
    WellAnnotatedGenes <- dge_table %>%
      dplyr::select(GeneID:NCBIURL) %>%
      # Unlike above, this has gene IDs duplicated when multiple contrasts are used
      distinct(GeneID, .keep_all = TRUE)

    ResultsGSEADf <- sapply(
      names(contrasts),
      function(contrast) {
        # Sort gene list by Rank statistic from DGE table for the contrast of interest
        # Rank is `t` or `stat` from `limma` and `DESeq2`, respectively
        geneList <- dge_table %>%
          filter(Contrast == contrast) %>%
          select(EntrezID, Rank) %>%
          arrange(desc(Rank)) %>%
          deframe()

        # `GSEA()` almost runs with default parameters
        res <- GSEA(
          geneList = geneList,
          TERM2GENE = GeneSets %>%
            select(gs_name, ncbi_gene),
          eps = 0
        ) %>%
          as.data.frame()
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    ) %>%
      # Collapse the list of data frames into a single data frame with an additional column for the contrast
      bind_rows(.id = "Contrast") %>%
      # Add direction of regulation
      mutate(Direction = ifelse(NES > 0, "up", "down")) %>%
      remove_rownames() %>%
      dplyr::select(
        "Pathway" = "ID",
        "EnrichmentScore" = "enrichmentScore",
        "PVal" = "p.adjust",
        "QVal" = "qvalue",
        Direction,
        Contrast,
        "SetSize" = "setSize"
      ) %>%
      as.data.frame()

    # Data frame containing the gene symbols and ID contained in the gene sets
    GeneSetsSig <- GeneSets %>%
      # Select and rename only relevant columns
      dplyr::select(
        "Symbol" = "gene_symbol",
        "GeneID" = "ensembl_gene",
        "GSName" = "gs_name",
        "GSCollection" = "gs_collection",
        "GSSubcollection" = "gs_subcollection",
        "GSCollectionName" = "gs_collection_name",
        "GSDescription" = "gs_description",
        "GSURL" = "gs_url"
      ) %>%
      # Subset to significantly enriched gene sets only
      filter(GSName %in% unique(ResultsGSEADf$Pathway)) %>%
      # Also add the information for each individual gene, if available
      left_join(
        WellAnnotatedGenes %>% dplyr::select(-Symbol),
        by = "GeneID"
      ) %>%
      as.data.frame()

    return(
      list(
        Results = ResultsGSEADf,
        SignificantGeneSetsGenes = GeneSetsSig
      )
    )
  }
}
