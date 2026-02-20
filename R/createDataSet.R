#' @title Create A dexDataSet Object
#'
#' @description
#' This function creates a `dexDataSet` object, which is the data structure used by DExploreR. However, all data sets complying this structure can be used as DExploreR input. Creating of a `dexDataSet` object captures also `sessionInfo()` and `getwd()` for reproducibility. You can get the required classes of all slots and the required columns using \code{\link{printDataSetReqs}()}.
#'
#' @param QualityControl A data frame containing quality control metrics per sample, for instance, `Stats` from \code{\link{createRawCountsWithStats}()}. Used in |Raw data| -> |Quality control| -> |Number of genes| and |Number of reads|.
#'
#' @param RawCounts A data frame containing raw counts per gene and sample, for instance, `RawCountsAboveZero` from \code{\link{createRawCountsWithStats}()}. Used in |Raw data| -> |Quality control| -> |Read count distribution|.
#'
#' @param NormalizedCounts A data frame containing normalized counts per gene and sample. Used in |Raw data| -> |Gene expression heatmap| and in |GSEA| -> |Top scoring gene sets| -> `shiny::modalDialog()`.
#'
#' @param VarianceExplained A data frame containing the variance explained by each principal component, for instance, `VarianceExplained` from \code{\link{prepareDfs}()}. Used in |Raw data| -> |Dimensionalty reduction|.
#'
#' @param PCA A data frame containing them values principal components with sample and group information, for instance `DfPCA` from \code{\link{prepareDfs}()}. Used in |Raw data| -> |Dimensionalty reduction|.
#'
#' @param SamplesGroups A data frame containing the sample names, their assigned groups and further information for `meta_data_lab`, for instance, `SamplesGroups` from \code{\link{prepareDfs}()}.
#'
#' @param GeneInfoAliases A data frame containing gene annotations such as gene symbols, entrez IDs, descriptions, and aliases and NCBI urls, for instance, `GeneSymbols` from \code{\link{addGeneSymbols}()}. This data frame is inflated as each alias gets a separate row.
#'
#' @param DGEAnalysis A data frame containing differential gene expression analysis results, for instance, `DEGs` from \code{\link{calculateDEG}()}. Used in |DGEA| and in |GSEA| -> |Top scoring gene sets| -> `shiny::modalDialog()`
#'
#' @param GeneSets A data frame containing gene set enrichment analysis results, for instance, `ResultsGAGE` from \code{\link{calculateGSEA}()}. Used in |GSEA|.
#'
#' @param GeneSetsGenes A data frame containing genes of enriched gene sets in `GAGE`, for instance, `SignificantGeneSetsGenes` from \code{\link{calculateGSEA}()}.
#'
#' @param pathRawCounts The path to the raw counts directory from STAR alignment.
#'
#' @param pathMetaDataLab (Optional) The path to the lab metadata input file. Usually this file contains information about grouping of samples and is any how required during preparation of the data.
#'
#' @param pathMetaDataSeq (Optional) The path to the sequencing run metadata input file. This file usually contains information about the sequencing run and is any how required during preparation of the data.
#'
#' @param data_path (Optional) Specify the path to the data directory of the DExploreR app, for instance `/path/to/app/data/data_set_name`, where `data/` is mandatory as it is accessed by DExploreR. Providing `data_path` also requires providing `Cell_line_or_tissue`, `Study_target`, `Authors`, `Date`, `Details`, `Sequencing_details`, `DGEA_details`, and `GSEA_details` for the data set overview in the app (see below). This is information you deliver to a user of DExploreR. When `data_path` is provided, then the created data set is saved as an RDS file in the specified path as well as a single row data frame as a .csv with the provided meta information and no return is given. When `data_path` is not provided, then the created data set is returned as a list of data frames.
#'
#' @param Cell_line_or_tissue What is the cell line or tissue used in the study?
#'
#' @param Study_target What is the target of this study, for instance, cancer entitiy?
#'
#' @param Authors Which PI, group, or lab performed the study?
#'
#' @param Date When was the study performed?
#'
#' @param Details What are further details that should be available for the user in DExploreR? This information is displayed as hover information in the data sets overview
#'
#' @param Sequencing_details What are details about the sequencing, for instance, library protocol, strategy, ...?
#'
#' @param DGEA_details What are details about differential gene expression analysis, for instance, which method was used, which parameters were used, ...?
#'
#' @param GSEA_details What are details about gene set enrichment analysis, for instance, which method was used, which parameters were used, ...?
#'#'
#' @return A `dexDataSet` object containing all provided data and metadata or no return, when `data_path` is provided.
#'
#' @export
createDataSet <- function(
  QualityControl,
  RawCounts,
  NormalizedCounts,
  VarianceExplained,
  PCA,
  SamplesGroups,
  GeneInfoAliases,
  DGEAnalysis,
  GeneSets,
  GeneSetsGenes,
  pathRawCounts,
  data_path = NULL,
  Cell_line_or_tissue = NULL,
  Study_target = NULL,
  Authors = NULL,
  Date = NULL,
  Details = NULL,
  Sequencing_details = NULL,
  DGEA_details = NULL,
  GSEA_details = NULL,
  pathMetaDataLab = NULL,
  pathMetaDataSeq = NULL
) {
  # Define variables locally for R CMD check
  RunID <- SampleNameUser <- Group <- RDSPath <- metaInfoPath <- metaInfo <- NULL

  # Check if RunID is contained for joint prior to class initialization
  assert_that(
    "RunID" %in% colnames(QualityControl),
    msg = "QualityControl must contain 'RunID' column."
  )
  assert_that(
    "RunID" %in% colnames(RawCounts),
    msg = "RawCounts must contain 'RunID' column."
  )

  # Make sure all meta information is provided, when `data_path` is provided, since this is used for the data set overview in the app
  if (!is.null(data_path)) {
    assert_that(
      !is.null(Cell_line_or_tissue),
      !is.null(Study_target),
      !is.null(Authors),
      !is.null(Date),
      !is.null(Details),
      !is.null(Sequencing_details),
      !is.null(DGEA_details),
      !is.null(GSEA_details),
      msg = "All metadata fields (Cell_line_or_tissue, Study_target, Authors, Date, Details, Sequencing_details, DGEA_details, GSEA_details) must be provided when data_path is specified. This is the information display in DExploreR for the data set overview."
    )
  }

  # Warn user if paths to metadata files are not provided
  if (is.null(pathMetaDataLab)) {
    warning("You did not provide a path to meta data file form the lab.")
  }
  if (is.null(pathMetaDataSeq)) {
    warning("You did not provide a path to the sequencing run metadata file.")
  }

  # Contruct the paths for the RDS file and the .csv file, if `data_path` is provided
  # And construct the .csv itself
  if (!is.null(data_path)) {
    RDSPath <- paste0(data_path, ".rds")
    metaInfoPath <- paste0(data_path, ".csv")

    # Create single row data frame and save as .csv
    # Each created data set has an accompanying .csv file with the same name, which contains the metadata
    # This is used to display the data set overview in the app
    metaInfo <- data.frame(
      "Cell line or tissue" = Cell_line_or_tissue,
      "Study target" = Study_target,
      "Authors" = Authors,
      "Date" = Date,
      "Details" = Details,
      "Sequencing Details" = Sequencing_details,
      "DGEA Details" = DGEA_details,
      "GSEA Details" = GSEA_details,
      # This is the path to the RDS file, which is used to load the data set in the app.
      "data_path" = sub(".*(data/.*.rds)$", "\\1", RDSPath),
      check.names = FALSE
    )
  }

  # Add the sample names and groups to the quality control
  QualityControl <- QualityControl %>%
    left_join(SamplesGroups, by = "RunID") %>%
    mutate(
      SampleNameUser = ifelse(
        RunID == "All samples",
        "All samples",
        SampleNameUser
      ),
      Group = ifelse(
        RunID == "All samples",
        "All samples",
        Group
      )
    )

  # Add the sample names and groups to the raw counts
  RawCounts <- RawCounts %>%
    left_join(SamplesGroups, by = "RunID")

  DataSet <- new(
    "dexDataSet",
    QualityControl = QualityControl,
    RawCounts = RawCounts,
    NormalizedCounts = NormalizedCounts,
    VarianceExplained = VarianceExplained,
    PCA = PCA,
    SamplesGroups = SamplesGroups,
    GeneInfoAliases = GeneInfoAliases,
    DGEAnalysis = DGEAnalysis,
    GeneSets = GeneSets,
    GeneSetsGenes = GeneSetsGenes,
    # Metadata
    sessionInfo = sessionInfo(),
    workingDirectory = getwd(),
    pathRawCounts = pathRawCounts,
    pathMetaDataLab = ifelse(is.null(pathMetaDataLab), NA, pathMetaDataLab),
    pathMetaDataSeq = ifelse(is.null(pathMetaDataSeq), NA, pathMetaDataSeq)
  )

  # Convert to list to avoid dependency on this package
  DataSet <- toList(DataSet)

  # When no `data_path` provided, then return the data set, else save RDS and metaInfo for the app
  if (!is.null(data_path)) {
    saveRDS(DataSet, file = RDSPath)
    write.csv(metaInfo, file = metaInfoPath, row.names = FALSE)
  } else {
    return(DataSet)
  }
}
