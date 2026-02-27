#' @title Prepare Data Frames For DExploreR
#'
#' @description
#' This function combines the `CountsSymbols` from \code{\link{addGeneSymbols}()} with meta data from the lab and the sequencing run. Furthermore, it provides some functionality for filtering. It calculates CPMs using \code{\link[edgeR]{cpm}()} from `edgeR` and feeds them into \code{\link[stats]{prcomp}()} from `stats` for PCA. The data frames genereated herewith are required for all further anylses in the DExploreR shiny app.
#'
#' @param raw_counts A data frame containing raw counts for all samples along with sample names, gene IDs and further information (like symbols, alias ... ) as ouputed by \code{\link{addGeneSymbols}()} as `CountsSymbols`.
#'
#' @param meta_data_lab A data frame with sample names, grouping information and other relevevant metadata from the lab. This may require custom curation by the user.
#'
#' @param meta_data_seq A data frame with the sequencing related metadata. This may require custom curation by the user. This data frame is required to match the groups from `meta_data_lab` to the samples in `CountsSymbols` via the sequencing run IDs. When sample names from `meta_data_lab` can directly be mapped to the `RunID`s in `CountsSymbols`, this parameter can be left as `NULL`.
#'
#' @param join_meta_data_lab Define how to join `meta_data_lab` to `CountsSymbols`. Ideally, the `SAMPLE_NAME` column from `meta_data_seq` matches a user-defined sample name column in `meta_data_lab`. However, this implementation can also handle partial containments. If the user-defined sample column contains only a fraction of the strings in the `SAMPLE_NAME`.
#'
#' @param sample_name_column Specify the column to be used for the sample names. You can choose between `RunID`, `SAMPLE_NAME` and user defined column name. `RunID` comes from `CountsSymbols` of \code{\link{addGeneSymbols}()} but this is the sequencing run IDs, or the STAR output directory names. `SAMPLE_NAME` comes from `meta_data_seq` and the sample name column you specified comes from `meta_data_lab`. Most likely, this is the one you want to use since this is how your collaborator knows their samples. Ideally, this (partially) matches the `SAMPLE_NAME` in `meta_data_seq`.
#'
#' @param group_column `meta_data_lab` should contain columns, which define groups of samples. This parameter should not be left empty, but you can also only specify one column. The resulting column will get the generic name `Group`.
#'
#' @param join_meta_data_seq Define how to join `meta_data_seq` to `CountsSymbols`. The default assumes that the `RunID` column in `CountsSymbols` contains the sequencing run IDs, which can be matched to the `FASTQ_FILE` column in `meta_data_seq`. Yet, you need to make sure that `FASTQ_FILE` does not contain the `_R[12].fastq.gz` suffix. You can use: `mutate(FASTQ_FILE = sub("_R[12].fastq.gz", "", FASTQ_FILE))`, for instance. This parameter will be ignored when `meta_data_seq == NULL`.
#'
#' @param min_counts_per_gene Define the minimum number of read counts per gene for this to be considered expression.
#'
#' @param min_number_of_samples_per_group_with_gene_expressed Define the minimum number of samples per group that need to express a gene (as defined by `min_counts_per_gene`) for this gene to be retained for PCA and further analysis.
#'
#' @param min_average_gene_expression_per_group Define the minimum average gene expression per group for a gene to be retained for PCA and further analysis.
#'
#' @param replace_zeros For PCA, the raw counts are converted to log-CPM values using \code{\link[edgeR]{cpm}()} from `edgeR`. To avoid issues with \eqn{log(0)}, all zero values are replaced by this value prior to log-CPM conversion.
#'
#' @param log_transform Toggle \eqn{log2} transformation during CPM calculation. Default is `TRUE`.
#'
#' @param centering Toggle centering during PCA calculation. Default is `TRUE`.
#'
#' @param scaling Toggle scaling during PCA calculation. Default is `FALSE`.
#'
#' @return A list containing:
#'
#' * `RawCountsMeta`: This data frame is very similar to `CountsSymbols` from \code{\link{addGeneSymbols}()} but contains user-defined columns with sample names and grouping information. Furthermore, this can also be filtered based on the number of samples per group that need to have a certain gene expressed and based on the average gene expression per group. Setting `min_number_of_samples_per_group_with_gene_expressed` and `min_average_gene_expression_per_group` to \eqn{0} will disable this filtering.
#'
#' * `SamplesGroups`: This assignes each sample to its group, which was defined by `group_column`. This data frame contains the columnes `Group` and `SampleNameUser` along with the `RunID` and other columns from `meta_data_lab` that are related to sample names and groups. This data frame can be used to translate between different sample names and groups.
#'
#' * `CPMCounts`: This data frame contains the CPM values calculated from the raw counts after filtering.
#'
#' * `RawPCA`: This is the raw result of the PCA as outputed by \code{\link[stats]{prcomp}()} from `stats`.
#'
#' * `DfPCA`: This is the data frame containing the x and y coordinates for each sample and PC. It also contains the `RunID` and `Group` columns for plotting. This data frame is required for the PCA plot in |Raw data| -> |Dimensionaly reduction|.
#'
#' * `VarianceExplained`: This data frame contains the percentage of variance explained by each PC. It will be used for the Scree plot in |Raw data| -> |Dimensionaly reduction| and for axis labels in the PCA plot.
#'
#' @export
prepareDfs <- function(
  raw_counts,
  meta_data_lab,
  join_meta_data_lab,
  sample_name_column,
  group_column,
  meta_data_seq = NULL,
  join_meta_data_seq = c("RunID" = "FASTQ_FILE"),
  min_counts_per_gene = 1,
  min_number_of_samples_per_group_with_gene_expressed = 3,
  min_average_gene_expression_per_group = 1,
  replace_zeros = 0.5,
  log_transform = c("TRUE", "FALSE"),
  centering = c("TRUE", "FALSE"),
  scaling = c("TRUE", "FALSE")
) {
  # Define variables locally for R CMD check
  Group <- GeneID <- Counts <- NumMinCounts <- AverageCount <- RunID <- Variance <- PC <- Rowmedian <- Rowvariance <- EntrezID <- Symbol <- Alias <- Description <- SampleNameUser <- . <- n_matches <- NCBIURL <- OtherEntrezIDs <- NULL

  # Check if the user specified correct values for `log_transform`, `centering` and `scaling`
  log_transform <- match.arg(log_transform)
  centering <- match.arg(centering)
  scaling <- match.arg(scaling)

  # Convert values to logicals to use the in the function calls later
  log_transform <- as.logical(log_transform)
  centering <- as.logical(centering)
  scaling <- as.logical(scaling)

  # Create the raw counts with added meta data
  RawCountsMeta <- raw_counts %>%
    {
      if (!is.null(meta_data_seq)) {
        # Adding of sequencing meta data is optional, when the sample names from meta_data_lab can directly be mapped to the RunIDs in raw_counts
        left_join(., meta_data_seq, by = join_meta_data_seq)
      } else {
        .
      }
    } %>%
    # Add the groups
    fuzzy_left_join(
      meta_data_lab,
      by = join_meta_data_lab,
      # Ensure partial matching of sample names
      match_fun = function(col_x, col_lab) str_detect(col_x, fixed(col_lab))
    ) %>%
    # Rename the user-specified column name for easier handling
    dplyr::rename("SampleNameUser" = sample_name_column) %>%
    # Check if fuzzy join produced multiple matches for any RunID
    {
      # Keep original data frame, because the check applies some re-fromatting
      OriginalDf <- .

      # Check for multiple matches
      CheckDf <- OriginalDf %>%
        distinct(RunID, SampleNameUser, ) %>%
        group_by(RunID) %>%
        mutate(n_matches = n())

      # Throw an error if any RunID matched multiple SampleNameUsers
      if (any(CheckDf$n_matches > 1)) {
        BadIDs <- CheckDf %>% filter(n_matches > 1)
        stop(
          "Ambiguous matches detected for RunIDs:\n",
          paste(BadIDs$RunID, collapse = ", ")
        )
      }

      # Return unmodified data into the pipe
      OriginalDf
    } %>%
    mutate(
      # Create a grouping column by user defined combination of columns
      # It makes sense the columns you want to join as groups are present in `meta_data_lab`
      Group = do.call(
        paste,
        c(across(all_of(group_column)), sep = "_")
      )
    ) %>%
    # Use newly defined grouping column and `GeneID` to calculate filtering statistics
    group_by(Group, GeneID) %>%
    filter(
      # Remove gene IDs that have lass than `min_number_of_samples_per_group_with_gene_expressed` samples in this group with at least `min_counts_per_gene` counts
      sum(Counts >= min_counts_per_gene, na.rm = TRUE) >=
        min_number_of_samples_per_group_with_gene_expressed,
      # Remove geneIDs that have an average expression in this group below `min_average_gene_expression_per_group`
      mean(Counts, na.rm = TRUE) >= min_average_gene_expression_per_group
    ) %>%
    ungroup() %>%
    as.data.frame()

  # This will eventually allow to translate between different sample names and groups
  SamplesGroups <- RawCountsMeta %>%
    # Select run ID and all columns related to samples names and groups, that originate from `meta_data_lab`
    # Because of the way of the data frames are joined together, this is a reliable way to select the relevant columns
    dplyr::select(RunID, SampleNameUser, Group) %>%
    distinct() %>%
    as.data.frame()

  CPMCounts <- RawCountsMeta %>%
    # Transform data frame into wide format, required for CPM calculation
    pivot_wider(
      id_cols = GeneID,
      names_from = SampleNameUser,
      values_from = Counts,
      values_fill = 0
    ) %>%
    column_to_rownames("GeneID") %>%
    cpm(
      prior.count = replace_zeros,
      log = log_transform
    ) %>%
    scale(scale = FALSE) %>%
    as.data.frame() %>%
    rownames_to_column(var = "GeneID") %>%
    # Calculate row median and variance to be able to select top n genes with highest median expression or highest variance
    rowwise() %>%
    mutate(
      Rowmedian = median(c_across(-GeneID)),
      Rowvariance = var(c_across(-GeneID))
    ) %>%
    ungroup() %>%
    # Re-add the symbols, entrez IDs, descriptions, aliases and URLs
    left_join(
      raw_counts %>%
        # Use unique gene IDs because they multiplied by the number of samples
        distinct(GeneID, .keep_all = TRUE) %>%
        dplyr::select(-RunID, -Counts),
      by = "GeneID"
    ) %>%
    as.data.frame()

  RawPCA <- CPMCounts %>%
    # Keep the GeneIDs but remove the other non-numeric columns
    column_to_rownames("GeneID") %>%
    dplyr::select(!(Rowmedian:NCBIURL)) %>%
    t() %>%
    prcomp(center = centering, scale. = scaling)

  # Calculate the explained variance by each PC
  # This can be used for axis labels in PCA plots
  VarianceExplained <- data.frame(
    PC = factor(colnames(RawPCA$x), levels = colnames(RawPCA$x)),
    Variance = round(RawPCA$sdev^2 / sum(RawPCA$sdev^2) * 100, 2)
  ) %>%
    # Remove PCs with explained variance < 1%
    filter(Variance >= 1) %>%
    # Remove the factor levels from PC column
    mutate(PC = fct_drop(PC))

  # Create a data frame with PCs, sample names and groups
  # This can easily be used by `ggplot2`
  DfPCA <- RawPCA$x %>%
    as.data.frame() %>%
    rownames_to_column("SampleNameUser") %>%
    left_join(SamplesGroups, by = "SampleNameUser") %>%
    # Remove PCs that explain less than 1 % of the variance
    dplyr::select(
      all_of(
        c(
          as.character(VarianceExplained$PC),
          "SampleNameUser",
          "RunID",
          "Group"
        )
      )
    )

  return(
    list(
      RawCountsMeta = RawCountsMeta,
      SamplesGroups = SamplesGroups,
      CPMCounts = CPMCounts,
      RawPCA = RawPCA,
      DfPCA = DfPCA,
      VarianceExplained = VarianceExplained
    )
  )
}
