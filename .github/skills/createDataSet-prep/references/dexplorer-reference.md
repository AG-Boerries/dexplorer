# dexplorer Function Reference

This file is read by the Copilot agent before generating R scripts.
Do not modify unless the dexplorer package API changes.

---

## `createRawCountsWithStats(path_to_STAR_directory, strandedness, fix_runid)`

**Input format:** STAR only (`ReadsPerGene.out.tab` files).
**`strandedness` valid values:** `"CountsUnstranded"` (default), `"CountsForward"`, `"CountsReverse"`.

**Returns:** `list($RawCounts, $Stats)`
- `$RawCounts`: long data frame, columns `RunID` (character), `GeneID` (character), `Counts` (numeric). Only genes with `Counts > 0` and `GeneID` matching `^ENS` are retained.
- `$Stats`: data frame, columns `RunID`, `UnassignedUnmappedReads`, `UnassignedMappedReads`, `AssignedReads`, `TotalReads`, `GenesRecorded`.

**For featureCounts input:** write custom loading code. The output list must have the same structure: `$RawCounts` and `$Stats` with the exact column names above.

---

## `addGeneSymbols(raw_counts, species)`

**Input:** `raw_counts` — long data frame with at minimum columns `GeneID` (Ensembl IDs: `^ENSG` for human / `^ENSMUSG` for mouse), `RunID`, `Counts`.
**`species`:** `"mouse"` | `"human"`

**Returns:** `list($Symbols, $CountsSymbols)`
- `$Symbols`: inflated (one row per alias), columns `GeneID`, `Symbol`, `EntrezID`, `Description`, `Alias`, `NCBIURL`.
- `$CountsSymbols`: deduplicated (one row per gene per sample), columns `GeneID`, `Symbol`, `EntrezID`, `Description`, `Alias`, `OtherEntrezIDs`, `NCBIURL`, `RunID`, `Counts`. Pass this as `raw_counts` to `prepareDfs()`.

**Save the full list** as `gene_symbols.rds` so both `$Symbols` and `$CountsSymbols` are available downstream.

---

## `prepareDfs(raw_counts, meta_data_lab, join_meta_data_lab, sample_name_column, group_column, ...)`

**Input:**
- `raw_counts` = `$CountsSymbols` from `addGeneSymbols()`.
- `meta_data_lab` = metadata data frame with sample names and grouping columns.
- `join_meta_data_lab` = named vector, e.g. `c("RunID" = "sample_id")` — maps a column in `raw_counts` (left) to a column in `meta_data_lab` (right). Matching uses `str_detect(left_value, fixed(right_value))`: the left-column value must **contain** the right-column value as a substring. Metadata sample names (right) should be substrings of the RunIDs (left), e.g. RunID `"Ctrl_103"` contains metadata name `"Ctrl_103"`.
- `sample_name_column` = string — column name in `meta_data_lab` to use as `SampleNameUser`.
- `group_column` = character vector — one or more column names in `meta_data_lab` pasted together (`_`-separated) to form the `Group` column.
- `meta_data_seq = NULL` when sample names can be directly matched from `RunID`s.
- `min_counts_per_gene` = integer (from `analysis-defaults.yml` `thresholds.min_counts`).
- `min_number_of_samples_per_group_with_gene_expressed` = infer from metadata (number of replicates in smallest group).

**Returns:** `list($RawCountsMeta, $SamplesGroups, $CPMCounts, $RawPCA, $DfPCA, $VarianceExplained)`
- `$SamplesGroups`: columns `RunID`, `SampleNameUser`, `Group` — required by `calculateDEG()` and `calculateGSEA()`.
- `$RawCountsMeta`: same structure as `raw_counts` but filtered, with `SampleNameUser` and `Group` columns added.
- `$DfPCA`: columns `PC1`, `PC2`, ..., `SampleNameUser`, `RunID`, `Group` (only PCs explaining ≥ 1% variance).
- `$VarianceExplained`: columns `PC` (factor), `Variance` (numeric %).

---

## `calculateDEG(raw_counts, samples_groups, de_fun, contrasts, deg_tool, ...)`

**Input:**
- `raw_counts` = `$RawCountsMeta` from `prepareDfs()`.
- `samples_groups` = `$SamplesGroups` from `prepareDfs()`.
- `de_fun` = formula string, e.g. `"~ 0 + Group"`. Only single-variable formulas are currently supported.
- `contrasts` = always a *named* character vector for both tools, e.g. `c("PBRM1_vs_Ctrl" = "PBRM1-Ctrl")`. Names become readable `Contrast` labels in `$DEGs`. Unnamed vectors cause DESeq2 to use numeric labels (`"1"`, `"2"`, ...). Group names in values come from `$SamplesGroups$Group` (limma strips the variable prefix automatically; DESeq2 uses plain group names).
- `deg_tool` = `"limma"` (default) | `"deseq2"`.

**Returns:** `list($DEGs, $NormCounts)`
- `$DEGs`: columns `GeneID`, `Symbol`, `Alias`, `EntrezID`, `Description`, `NCBIURL`, `Contrast`, `Direction`, `AveExpr`, `Log2FC`, `PVal`, `PValAdj`, `Rank`, `LogPValAdj`.
- `$NormCounts`: wide matrix with gene info columns (`GeneID`, `Symbol`, `Alias`, `EntrezID`, `Description`, `NCBIURL`) plus one column per sample named by `SampleNameUser`.

---

## `calculateGSEA(normalized_count_data, dge_table, species, collection, contrasts, samples_groups)`

**Two mutually exclusive modes — provide exactly one of:**
- `normalized_count_data` = `$NormCounts` from `calculateDEG()` → uses `gage()`. Must have a `GeneID` column and one column per sample (named by `SampleNameUser`); gene-info columns are stripped internally.
- `dge_table` = `$DEGs` from `calculateDEG()` → uses `clusterProfiler::GSEA()`. Requires `EntrezID` and `Rank` columns. Gene sets without EntrezID matches are silently dropped.

**`collection`:** character vector, valid codes: `"H"` (hallmark MSigDB), `"GO:BP"`, `"GO:CC"`, `"GO:MF"`. For mouse, hallmark is internally remapped to `"MH"`. Reactome is not supported.
**`contrasts`:** same named vector passed to `calculateDEG()`.
**`samples_groups`:** `$SamplesGroups` from `prepareDfs()`.

**Returns:** `list($Results, $SignificantGeneSetsGenes)`
- `$Results` **gage mode**: columns `Pathway`, `EnrichmentScore`, `Pval` (lowercase `val`), `QVal`, `Direction`, `Contrast`, `SetSize`.
- `$Results` **clusterProfiler mode**: columns `Pathway`, `EnrichmentScore`, `PVal` (capital `Val`), `QVal`, `Direction`, `Contrast`, `SetSize`.
- `$SignificantGeneSetsGenes`: genes in significantly enriched sets, with gene symbols, Ensembl IDs, and database links.

---

## `createDataSet(QualityControl, RawCounts, NormalizedCounts, VarianceExplained, PCA, SamplesGroups, GeneInfoAliases, DGEAnalysis, GeneSets, GeneSetsGenes, pathRawCounts, ...)`

Assembles all analysis outputs into a `dexDataSet` list for the DExploreR Shiny app.

**Required slot sources:**

| Argument | Source |
|----------|--------|
| `QualityControl` | `raw_data$Stats` (must have `RunID` column) |
| `RawCounts` | `raw_data$RawCounts` (must have `RunID` column) |
| `NormalizedCounts` | `preprocessed$CPMCounts` |
| `VarianceExplained` | `preprocessed$VarianceExplained` |
| `PCA` | `preprocessed$DfPCA` |
| `SamplesGroups` | `preprocessed$SamplesGroups` |
| `GeneInfoAliases` | `gene_symbols$Symbols` (load `gene_symbols.rds`; that file stores the full `addGeneSymbols()` list) |
| `DGEAnalysis` | `deg_results$DEGs` |
| `GeneSets` | `gsea_results$Results` |
| `GeneSetsGenes` | `gsea_results$SignificantGeneSetsGenes` |
| `pathRawCounts` | path string to the raw count directory |

When `data_path = NULL` (default), returns the assembled list. When `data_path` is provided, saves RDS + metadata CSV and returns nothing — also requires: `Cell_line_or_tissue`, `Study_target`, `Authors`, `Date`, `Details`, `Sequencing_details`, `DGEA_details`, `GSEA_details`.
