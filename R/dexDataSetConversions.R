#' @title Convert DexDataSet Object to a List
#'
#' @description
#' This function converts a `dexDataSet` object into a standard R list, preserving all its components. This avoids dependency on on this package, when working with the data.
#'
#' @param object A `dexDataSet` object to be converted.
#'
#' @export
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

#' @title Convert List of Data frames to a DexDataSet Object
#'
#' @description
#' This functions turns a list of data frames into a `dexDataSet` object. This is especially useful, when checking the validity of user-provided data sets. These are provided as lists of data frames as an `.rds` file.
#'
#' @param list A list of data frames, which should be converted to a `dexDataSet` object. The list should contain the same components as the `dexDataSet` object, which are: `QualityControl`, `RawCounts`, `NormalizedCounts`, `VarianceExplained`, `PCA`, `SamplesGroups`, `GeneInfoAliases`, `DGEAnalysis`, `GeneSets`, `GeneSetsGenes`, `sessionInfo`, `workingDirectory`, `pathRawCounts`, `pathMetaDataLab` and `pathMetaDataSeq`. The class operator will then check if all necessary columns are available, so if the data set is valid for loading into the app.
#'
#' @export
toObject <- function(list) {
  new(
    "dexDataSet",
    QualityControl = list$QualityControl,
    RawCounts = list$RawCounts,
    NormalizedCounts = list$NormalizedCounts,
    VarianceExplained = list$VarianceExplained,
    PCA = list$PCA,
    SamplesGroups = list$SamplesGroups,
    GeneInfoAliases = list$GeneInfoAliases,
    DGEAnalysis = list$DGEAnalysis,
    GeneSets = list$GeneSets,
    GeneSetsGenes = list$GeneSetsGenes,
    sessionInfo = list$sessionInfo,
    workingDirectory = list$workingDirectory,
    pathRawCounts = list$pathRawCounts,
    pathMetaDataLab = list$pathMetaDataLab,
    pathMetaDataSeq = list$pathMetaDataSeq
  )
}
