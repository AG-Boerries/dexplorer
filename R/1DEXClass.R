# Define required columns for each data frame
# Additional column are allowed, but they will have no impact on functionality or visualization in DExploreR
.dex_requirements <- list(
  # As the provided data frame to the class get joined with SamplesGroups, this will also contain all the columns form `meta_data_lab`
  # These additional columns cannot be checked here as they vary between data sets
  # Same logic applies to `RawCounts` below
  QualityControl = c(
    "UnassignedUnmappedReads",
    "UnassignedMappedReads",
    "AssignedReads",
    "TotalReads",
    "GenesRecorded",
    "RunID",
    "SampleNameUser",
    "Group"
  ),
  RawCounts = c("RunID", "GeneID", "Counts", "SampleNameUser", "Group"),
  # This data frame will also contain the sample names as columns
  # These cannot be checked as they vary between data sets
  NormalizedCounts = c(
    "GeneID",
    "Alias",
    "OtherEntrezIDs",
    "Symbol",
    "EntrezID",
    "Description",
    "NCBIURL"
  ),
  VarianceExplained = c("PC", "Variance"),
  # At least the first two PCs should be present
  # However, the number of PCs can vary depending on the dataset
  PCA = c("PC1", "PC2", "RunID", "SampleNameUser", "Group"),
  SamplesGroups = c("RunID", "SampleNameUser", "Group"),
  GeneInfoAliases = c(
    "GeneID",
    "Symbol",
    "EntrezID",
    "Description",
    "Alias",
    "NCBIURL"
  ),
  DGEAnalysis = c(
    "GeneID",
    "Symbol",
    "Alias",
    "EntrezID",
    "Description",
    "Contrast",
    "Direction",
    "AveExpr",
    "Log2FC",
    "PVal",
    "PValAdj",
    "LogPValAdj"
  ),
  GeneSets = c(
    "Pathway",
    "EnrichmentScore",
    "PVal",
    "QVal",
    "Direction",
    "Contrast",
    "SetSize"
  ),
  GeneSetsGenes = c(
    "Symbol",
    "GeneID",
    "GSName",
    "GSCollection",
    "GSSubcollection",
    "GSCollectionName",
    "GSDescription",
    "GSURL",
    "Alias",
    "EntrezID",
    "Description",
    "NCBIURL"
  )
)

# Some information on the required slots for `dexDataSet` objects
.dex_info <- list(
  QualityControl = "Quality metrics for each sample from the STAR aligner.",
  RawCounts = "Raw counts for each sample and gene with at least one read assigned (long format: one row per gene/sample).",
  NormalizedCounts = "Normalized gene expression values and gene annotation (wide format: one row per gene).",
  VarianceExplained = "Variance explained by each principal component (PCA). Must match the number of PCs passed to the PCA data frame.",
  PCA = "Principal component scores for each sample but least PC1 and PC2 must be contained (samples as rows and PCs as columns).",
  SamplesGroups = "Sample metadata: sample names, user-defined names, and group assignments.",
  GeneInfoAliases = "Gene annotation: ensembl IDs, symbols, aliases, Entrez IDs, descriptions, NCBI URLs (one row per gene alias).",
  DGEAnalysis = "Differential gene expression results for all contrasts of interest (long format: one row per gene/contrast).",
  GeneSets = "Gene set enrichment results for all contrasts and gene set collections of interest (long format: one row per gene set/contrast).",
  GeneSetsGenes = "Gene set membership: which genes belong to which sets, with annotation.",
  sessionInfo = "R session info, automatically captured when creating the data set object using `createDataSet()`.",
  workingDirectory = "Working directory, automatically captured when creating the data set object using `createDataSet()`.",
  pathRawCounts = "File path to raw counts input file.",
  pathMetaDataLab = "File path to lab metadata input file (optional).",
  pathMetaDataSeq = "File path to sequencing metadata input file (optional)."
)

#' @title Check DExploreR Data Set
#'
#' @description
#' Check during class initialization if all required columns are present in the data frame slots of a "dexDataSet" object.
#'
#' @param object An object of class "dexDataSet", which will be checked for required columns in the data frame slots.
#'
check_dataset <- function(object) {
  errors <- character()

  # Check data frame slots for required columns
  for (slot in names(.dex_requirements)) {
    cols <- .dex_requirements[[slot]]
    missing <- setdiff(cols, colnames(slot(object, slot)))

    if (length(missing) > 0) {
      errors <- c(
        errors,
        sprintf(
          "%s is missing required columns: %s",
          slot,
          paste(missing, collapse = ", ")
        )
      )
    }
  }

  if (length(errors) == 0) TRUE else errors
}

#' @title Session Info Class
#'
#' @description
#' Explicitly define an S4 wrapper class for `sessionInfo`, so that this can be set in the slot definitions.
#'
setClass("sessionInfo", contains = "list")

#' @title DExploreR Data Set Class
#'
#' @description
#' This class defines the structure of a DExploreR data set. On top of the actual data, this class also stores metadata such as the working directory and paths to input files and the sessionInfo.
#'
setClass(
  "dexDataSet",
  slots = list(
    QualityControl = "data.frame",
    RawCounts = "data.frame",
    NormalizedCounts = "data.frame",
    VarianceExplained = "data.frame",
    PCA = "data.frame",
    SamplesGroups = "data.frame",
    GeneInfoAliases = "data.frame",
    DGEAnalysis = "data.frame",
    GeneSets = "data.frame",
    GeneSetsGenes = "data.frame",
    sessionInfo = "sessionInfo",
    workingDirectory = "character",
    pathRawCounts = "character",
    # These two parameters are optional, since not all analyses will have these files
    pathMetaDataLab = "ANY",
    pathMetaDataSeq = "ANY"
  ),
  validity = check_dataset
)

#' @title Print requirements for DExploreR data set
#'
#' @description
#' This function prints the class of each slot of the `dexDataSet` class and if the slot is a dataframe it prints the required columns. This can be useful for users to check if their data frames meet the requirements before creating a `dexDataSet` object.
#'
#' @export
printDataSetReqs <- function() {
  # Get the class and the slots
  cls <- getClass("dexDataSet")
  slot_classes <- cls@slots

  reqs <- list()

  for (slot in names(slot_classes)) {
    # Extract the class of the slot
    slot_class <- slot_classes[[slot]]
    # Extract the required columns
    slot_cols <- .dex_requirements[[slot]]
    # Extract the information about the slot
    slot_info <- .dex_info[[slot]]

    reqs[[slot]] <- list(
      class = slot_class,
      columns = slot_cols,
      info = slot_info
    )
  }

  reqs
}
