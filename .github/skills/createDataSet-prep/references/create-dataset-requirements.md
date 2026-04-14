# createDataSet() requirements

Use this file as the source of truth when mapping user data to `dexplorer::createDataSet()`.

## Required arguments

`createDataSet()` expects these primary inputs:

- `QualityControl`
- `RawCounts`
- `NormalizedCounts`
- `VarianceExplained`
- `PCA`
- `SamplesGroups`
- `GeneInfoAliases`
- `DGEAnalysis`
- `GeneSets`
- `GeneSetsGenes`
- `pathRawCounts`

Optional path metadata:

- `pathMetaDataLab`
- `pathMetaDataSeq`

If `data_path` is supplied, these metadata fields also become mandatory:

- `Cell_line_or_tissue`
- `Study_target`
- `Authors`
- `Date`
- `Details`
- `Sequencing_details`
- `DGEA_details`
- `GSEA_details`

## Exact dataframe column requirements

These are the slot requirements enforced by the `dexDataSet` validity check after `createDataSet()` constructs the object.

### QualityControl

Required columns:

- `UnassignedUnmappedReads`
- `UnassignedMappedReads`
- `AssignedReads`
- `TotalReads`
- `GenesRecorded`
- `RunID`
- `SampleNameUser`
- `Group`

Practical note:
`createDataSet()` will join `SamplesGroups` into `QualityControl` by `RunID`, so a source QC table is still usable before the call if it has the QC metrics plus `RunID`.

### RawCounts

Required columns:

- `RunID`
- `GeneID`
- `Counts`
- `SampleNameUser`
- `Group`

Practical note:
`createDataSet()` joins `SamplesGroups` into `RawCounts` by `RunID`, so a source raw-count table can still be usable before the call if it has at least `RunID`, `GeneID`, and `Counts`.

### NormalizedCounts

Required columns:

- `GeneID`
- `Alias`
- `OtherEntrezIDs`
- `Symbol`
- `EntrezID`
- `Description`
- `NCBIURL`
- `Rowmedian`
- `Rowvariance`

Also require one expression column per sample.

Common upstream note:
NCBIURL can be created by a simple addition of the EntrezID to the base URL "https://www.ncbi.nlm.nih.gov/datasets/gene/". If no EntrezID is available, it can be set to NA.

### VarianceExplained

Required columns:

- `PC`
- `Variance`

### PCA

Required columns:

- `PC1`
- `PC2`
- `RunID`
- `SampleNameUser`
- `Group`

Additional principal component columns are allowed.

### SamplesGroups

Required columns:

- `RunID`
- `SampleNameUser`
- `Group`

### GeneInfoAliases

Required columns:

- `GeneID`
- `Symbol`
- `EntrezID`
- `Description`
- `Alias`
- `NCBIURL`

This table is intentionally inflated so each alias can have its own row.

### DGEAnalysis

Required columns:

- `GeneID`
- `Symbol`
- `Alias`
- `EntrezID`
- `Description`
- `Contrast`
- `Direction`
- `AveExpr`
- `Log2FC`
- `PVal`
- `PValAdj`
- `LogPValAdj`

Common upstream note:
The standard dexplorer workflow also carries `NCBIURL` and `Rank`, but the class validity check only enforces the columns above.

### GeneSets

Required columns:

- `Pathway`
- `EnrichmentScore`
- `PVal`
- `QVal`
- `Direction`
- `Contrast`
- `SetSize`

Common upstream note:
Some GSEA outputs use `Pval` instead of `PVal`. Rename carefully and verify the semantics match.

### GeneSetsGenes

Required columns:

- `Symbol`
- `GeneID`
- `GSName`
- `GSCollection`
- `GSSubcollection`
- `GSCollectionName`
- `GSDescription`
- `GSURL`
- `Alias`
- `EntrezID`
- `Description`
- `NCBIURL`

## Behavior of createDataSet()

`createDataSet()` performs these checks or transformations:

1. Assert that `QualityControl` contains `RunID`.
2. Assert that `RawCounts` contains `RunID`.
3. If `data_path` is given, assert that all dataset-overview metadata fields are present.
4. Join `SamplesGroups` into `QualityControl` by `RunID`.
5. Join `SamplesGroups` into `RawCounts` by `RunID`.
6. Construct the `dexDataSet` object, which triggers slot validation.
7. Convert the object to a plain list before returning or saving.

## Practical source mapping from the standard dexplorer workflow

If the user followed the usual dexplorer preparation path, these are the typical sources:

- `QualityControl`: `raw_data$Stats`
- `RawCounts`: `raw_data$RawCounts`
- `NormalizedCounts`: `preprocessed$CPMCounts`
- `VarianceExplained`: `preprocessed$VarianceExplained`
- `PCA`: `preprocessed$DfPCA`
- `SamplesGroups`: `preprocessed$SamplesGroups`
- `GeneInfoAliases`: `gene_symbols$Symbols`
- `DGEAnalysis`: `deg_results$DEGs`
- `GeneSets`: `gsea_results$Results`
- `GeneSetsGenes`: `gsea_results$SignificantGeneSetsGenes`

## Gap classification

Classify problems into one of these buckets:

- `reshape_or_rename`: the information exists but needs column renaming, pivoting, joining, or splitting
- `reconcile_ids`: the information exists but identifiers or sample names need harmonization
- `missing_required_artifact`: a required slot has no trustworthy source artifact
- `missing_upstream_analysis`: the user does not have a needed analysis output such as PCA, DGEA, GSEA, or gene annotation

Only the first two categories are safe to solve without new biological analysis.
