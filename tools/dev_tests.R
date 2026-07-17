# Load package functions
library(devtools)
load_all()

# ---- Test DExploreR after modifications ----
# Using two "bigger" datasets
# AG_Frew was created using the built-in functions
# ucare mouse organoids was created using the accompanying CoPilot SKILL
dir <- "~/Documents/Projects/07_DExploreR/test_data/"
runDExploreR(data = dir)

# ---- Manually load datasets for development ----
df1 <- readRDS("inst/extdata/data/GSE273186.rds")
df2 <- readRDS(
  "~/Documents/Projects/07_DExploreR/test_data/AG_Frew_RCC_mouse_model_2022.rds"
)
df3 <- readRDS(
  "~/Documents/Projects/07_DExploreR/test_data/ucare_mouse_organoid_dexplorer_dataset.rds"
)

# ---- Scree plot ----
createScreePlot(
  explained_var = df3$VarianceExplained,
  pc_x = c("PC1", "PC2"),
  pc_y = "",
  standalone = TRUE
)

# ---- Read counts plot ----
createReadCountPlot(
  df = df2$QualityControl,
  standalone = TRUE
)

# ---- Detected genes plot ----
createGeneCountPlot(
  df = df2$QualityControl,
  standalone = TRUE
)

# ---- Read count distribution plot ----
createCountDistributionPlot(
  df = df2$RawCounts,
  standalone = TRUE
)

# ---- PCA plot ----
createPCAPlot(
  df_pca = df2$PCA,
  explained_var = df2$VarianceExplained,
  pc_x = "PC1",
  pc_y = "PC2",
  selected_palette = "Set1",
  group_overlay = "Ellipse",
  standalone = TRUE
)

# ---- Heatmap ----
df_heatmap <- formatForHeatmap(
  df = df2$NormalizedCounts
)

createGeneExpressionHeatmap(
  df = df_heatmap,
  heatmap_colors = "RdBu",
  samples_groups = df2$SamplesGroups,
  heatmap_heights = heatmapHeights(
    n_genes = nrow(df_heatmap),
    dendro_type = "Samples and genes"
  )
)

# ---- Top DEGs bar plot ----
createTopDEGsPlot(
  df = formatTopDEGs(
    df = df2$DGEAnalysis,
    selected_number_of_genes = 20,
    fc_or_pvalue = TRUE
  ),
  standalone = TRUE
)

# ---- Volcano plot ----
createVolcanoPlot(
  df = df2$DGEAnalysis,
  standalone = TRUE,
  highlight_top = TRUE,
  selected_genes = c("Ccl2", "Ccl5", "Cxcl10", "Cxcl11", "Cxcl13"),
)

# ---- Jaccard index DGEA ----
df <- formatDGEAContrastIntersection(
  df = df3$DGEAnalysis,
  p_threshold = 0.05,
  l2fc_threshold = 1
)

createDGEAContrastIntersectionPlot(df = df, standalone = TRUE)

# ---- Top enriched gene sets ----
# This should be done in the preprocessing
df <- df2$GeneSets |>
  # Add the genes as a nested column
  left_join(
    df2$GeneSetsGenes %>% group_by(GSName) %>% nest(),
    by = c("Pathway" = "GSName")
  ) |>
  # Add information for the tooltip
  left_join(
    df2$GeneSetsGenes %>%
      distinct(GSName, GSCollectionName, GSDescription, GSURL),
    by = c("Pathway" = "GSName")
  )

df_test <- formatForGeneSetsPlot(
  df = df,
  selected_collections = "Hallmark",
  # selected_gene_sets = "GOCC_MYOSIN_FILAMENT",
  top_gene_sets = 20,
  selected_contrast = "WT female vs WT male"
)
GeneSetsPlot(df = df_test)
