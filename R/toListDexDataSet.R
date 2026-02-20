#' @title Convert DexDataSet object to a list
#'
#' @description
#' This function converts a `dexDataSet` object into a standard R list, preserving all its components. This avoids dependency on on this package, when working with the data.
#'
#' @param object A `dexDataSet` object to be converted.
#'
toList <- function(object) {
  list(
    QualityControl = object@QualityControl,
    RawCounts = object@RawCounts,
    NormalizedCounts = object@NormalizedCounts,
    VarianceExplained = object@VarianceExplained,
    PCA = object@PCA,
    SamplesGroups = object@SamplesGroups,
    GeneInfoAliases = object@GeneInfoAliases,
    DGEAnalysis = object@DGEAnalysis,
    GeneSets = object@GeneSets,
    GeneSetsGenes = object@GeneSetsGenes,
    sessionInfo = object@sessionInfo,
    workingDirectory = object@workingDirectory,
    pathRawCounts = object@pathRawCounts,
    pathMetaDataLab = object@pathMetaDataLab,
    pathMetaDataSeq = object@pathMetaDataSeq
  )
}
