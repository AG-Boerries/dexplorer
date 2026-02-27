#' @title Calculate Differentially Expressed Genes
#'
#' @description
#' This function calculates differentially expressed genes (DEGs) between specified sample groups using the `edgeR`-`limma`-workflow. It calculated DEGs based on the provided raw counts, contrasts and formula.
#'
#' @param raw_counts A data frame in the format of `RawCountsMeta` from \code{\link{prepareDfs}()}.
#'
#' @param samples_groups A data frame in the format of `SamplesGroups` from \code{\link{prepareDfs}()}.
#'
#' @param de_fun A formula specifying the design for the differential expression analysis. For example, `~ 0 + Group` to compare groups defined in the `Group` column of `samples_groups`. Additional covariates can be included as needed.
#'
#' @param contrasts A named vector of contrasts specifying which group comparisons to perform. Cannot be `NULL`.
#'
#' @param deg_tool Specify which tool to use for DEG calculation. Options are `limma` (default) or `deseq2`.
#'
#' @param method_for_normalization Method of normalization to be used by \code{\link[edgeR]{calcNormFactors}()}. Options are `TMM` (default), `TMMwsp`, `RLE`, `upperquartile`, or `none`. This parameter is only used when `deg_tool = "limma"`.
#'
#' @param sum_trim Corresponds to `sumTrim` parameter of \code{\link[edgeR]{calcNormFactors}()}. Default is `0.15`. This parameter is only used when `deg_tool = "limma"` and `method_for_normalization = "TMM"`.
#'
#' @param lm_fit_method Corresponds to `method` parameter of \code{\link[limma]{lmFit}()} from `limma`. Options are `robust` (default) or `ls`. This parameter is only used when `deg_tool = "limma"`. Cave: Using `ls` is not validated yet. This may require further paramters or other settings than the ones provided.
#'
#' @return A list containing:
#'
#' * `DEGs`: A tidy data frame with the DEG results for all or the specified contrasts with the following columns: `GeneID`, `Symbol`, `Alias`, `EntrezID`, `Description`, `NCBIURL`, `Contrast`, `Direction`, `AveExpr`, `Log2FC`, `PVal`, `PValAdj`, `Rank` and `LogPValAdj`.
#'
#' * `NormCounts`: A normalized expression matrix, calculated by \code{\link[limma]{voom}()} from `limma` or by \code{\link[DESeq2]{counts}()} from `DESeq2`, with all gene information available added.
#'
#' @export
calculateDEG <- function(
  raw_counts,
  samples_groups,
  de_fun,
  contrasts = NULL,
  deg_tool = c("limma", "deseq2"),
  method_for_normalization = c("TMM", "TMMwsp", "RLE", "upperquartile", "none"),
  sum_trim = 0.15,
  lm_fit_method = c("robust", "ls")
) {
  # Define variables locally for R CMD check
  GeneID <- Counts <- log2FoldChange <- pvalue <- padj <- baseMean <- Symbol <- Alias <- EntrezID <- Description <- Contrast <- Direction <- AveExpr <- Log2FC <- PVal <- PValAdj <- SampleNameUser <- logFC <- P.Value <- adj.P.Val <- . <- NCBIURL <- contrast <- NULL

  # Check if the optional parameters are correct
  method_for_normalization <- match.arg(method_for_normalization)
  lm_fit_method <- match.arg(lm_fit_method)
  deg_tool <- match.arg(deg_tool)

  # Check if contrasts are provided
  assert_that(
    !is.null(contrasts),
    msg = paste(
      "Please provide contrasts to calculate DEGs. You can use the values from any column in `sample_groups`:",
      paste(colnames(samples_groups), collapse = ", ")
    )
  )

  # Create the design formula
  de_fun <- as.formula(de_fun)

  # Extract variables
  de_vars <- attr(terms(de_fun), "term.labels")

  for (var in de_vars) {
    # Ensure that the variables are availble in samples_groups
    assert_that(
      var %in% colnames(samples_groups),
      msg = paste0(
        "Variable '",
        var,
        "' from de_fun not found in samples_groups."
      )
    )
    # Convert them factors
    samples_groups[[var]] <- factor(samples_groups[[var]])
  }

  # Create group factor based on `de_fun`
  if (length(de_vars) == 1) {
    group_factor <- samples_groups[[de_vars]]
  } else {
    stop(
      "Currently, only one variable can be used in `de_fun`. Please provide a formula with only one variable, e.g. `~ 0 + Group`."
    )
    # When in addition to Group other covariates are added, a new factor is created
    # Combine all variables in `de_fun` into a single factor
    # group_factor <- interaction(samples_groups[de_vars], sep = "_", drop = TRUE)
  }

  # Create count matrix
  RawCountsWide <- raw_counts %>%
    # Frist, remove the irrelevant columns for this step
    dplyr::select(all_of(c("GeneID", "Counts", "SampleNameUser"))) %>%
    # Pivot to wide format
    pivot_wider(
      id_cols = GeneID,
      names_from = SampleNameUser,
      values_from = Counts,
      values_fill = 0
    ) %>%
    column_to_rownames("GeneID")

  # Get the gene information from the genes in the count matrix
  GeneInfos <- raw_counts %>%
    # Extract the relevant information
    dplyr::select(1:7) %>%
    # Remove duplicates, which are present because of long format
    filter(!duplicated(GeneID)) %>%
    column_to_rownames("GeneID")

  # Check if the order is the same
  stopifnot(
    "GeneInfos and RawCountsWide do not have the same order of genes." = all(
      rownames(GeneInfos) == rownames(RawCountsWide)
    )
  )

  #### DESeq2 ####

  if (deg_tool == "deseq2") {
    # Prepare colData for DESeq2
    ColData <- samples_groups %>%
      column_to_rownames(var = "SampleNameUser") %>%
      mutate(across(all_of(de_vars), as.factor))

    stopifnot(
      "Row names not equal to column names" = all(
        rownames(ColData) == colnames(RawCountsWide)
      )
    )

    # Create DESeq2 dataset
    DDS <- DESeqDataSetFromMatrix(
      countData = RawCountsWide,
      colData = ColData,
      design = de_fun
    )

    # Run DESeq2
    DDS <- DESeq(DDS)

    # Create data frame with contrasts
    Contrasts <- enframe(
      contrasts,
      name = "name",
      value = "contrast"
    ) %>%
      separate(contrast, into = c("groupA", "groupB"), sep = "-") %>%
      # Ensure that the groups in the contrasts are present in the design formula
      mutate(group_var = de_vars)

    # Get log2 normalized counts, similar to `limma::voom()`, which is used for the GSEA with `gage()`
    NormCounts <- log2(counts(DDS, normalized = TRUE) + 1) %>%
      as.data.frame() %>%
      rownames_to_column(var = "GeneID") %>%
      left_join(GeneInfos %>% rownames_to_column(var = "GeneID"), by = "GeneID")

    # Run all contrasts and collect results
    Df <- pmap_dfr(
      Contrasts,
      function(groupA, groupB, name, group_var) {
        res <- results(
          DDS,
          contrast = c(group_var, groupA, groupB)
        ) %>%
          as.data.frame() %>%
          rownames_to_column(var = "GeneID") %>%
          mutate(
            Contrast = name,
            Direction = case_when(
              log2FoldChange > 0 ~ "up",
              log2FoldChange < 0 ~ "down",
              TRUE ~ "zero"
            )
          ) %>%
          left_join(
            GeneInfos %>% rownames_to_column(var = "GeneID"),
            by = "GeneID"
          )
      }
    ) %>%
      dplyr::select(
        GeneID,
        Symbol,
        Alias,
        EntrezID,
        Description,
        NCBIURL,
        Contrast,
        Direction,
        "AveExpr" = "baseMean",
        "Log2FC" = "log2FoldChange",
        "PVal" = "pvalue",
        "PValAdj" = "padj",
        "Rank" = "stat"
      )

    #### limma ####
  } else if (deg_tool == "limma") {
    # Assign samples to groups
    Design <- model.matrix(de_fun, data = samples_groups)

    # Remove variable prefixes from column names to ensure the user can easily define contrasts
    for (var in de_vars) {
      colnames(Design) <- sub(paste0("^", var, "_?"), "", colnames(Design))
    }

    # Use user-defined contrasts and turn them into a contrast matrix
    ContrastMatrix <- makeContrasts(
      contrasts = contrasts,
      levels = Design
    )

    DGEObject <- DGEList(
      counts = RawCountsWide,
      genes = GeneInfos,
      group = group_factor
    )

    DGEObject <- calcNormFactors(
      DGEObject,
      method = method_for_normalization,
      sumTrim = sum_trim
    )

    NormCountObject <- voom(DGEObject, Design)

    NormCounts <- as.data.frame(NormCountObject$E) %>%
      rownames_to_column(var = "GeneID") %>%
      left_join(GeneInfos %>% rownames_to_column(var = "GeneID"), by = "GeneID")

    DGEFitObject <- lmFit(
      NormCountObject,
      Design,
      method = lm_fit_method,
      maxit = 5000
    )

    DEGFitContrastsObject <- contrasts.fit(DGEFitObject, ContrastMatrix)

    DEGFitContrastsObject <- eBayes(DEGFitContrastsObject)

    Df <- map2_dfr(
      .x = seq_len(ncol(ContrastMatrix)),
      .y = colnames(ContrastMatrix),
      .f = function(i, contrast) {
        tt <- topTable(
          DEGFitContrastsObject,
          coef = i,
          number = Inf
        ) %>%
          rownames_to_column(var = "GeneID") %>%
          as.data.frame() %>%
          mutate(
            Contrast = contrast,
            Direction = case_when(
              logFC > 0 ~ "up",
              logFC < 0 ~ "down",
              TRUE ~ "zero"
            )
          )
      }
    ) %>%
      dplyr::select(
        GeneID,
        Symbol,
        Alias,
        EntrezID,
        Description,
        NCBIURL,
        Contrast,
        Direction,
        AveExpr,
        "Log2FC" = "logFC",
        "PVal" = "P.Value",
        "PValAdj" = "adj.P.Val",
        # This column is required for the ranked geneList of `clusterProfiler::GSEA()`
        "Rank" = "t"
      ) %>%
      mutate(
        # Use the names of the contrasts provided by the user for better readability in DExploreR
        Contrast = recode(
          Contrast,
          !!!setNames(names(contrasts), unname(contrasts))
        )
      )
  }

  # Calculate -log10 of adjusted p-value for better visualization in DExploreR
  Df <- Df %>%
    mutate(LogPValAdj = -log10(PValAdj))

  return(
    list(
      DEGs = Df,
      NormCounts = NormCounts
    )
  )
}
