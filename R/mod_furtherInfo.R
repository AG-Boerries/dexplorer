# This list contains the information displayed in a modal when clicking the "Further information" button
# The name of each list element has to correspond to the id of the `actionButton()` that shows the modal
further_info_contents <- list(
  info_pca = tagList(
    tags$p(
      "Dimensionality reduction is the process of describing a data set with a small number of data set patterns instead of the full number of data set variables (here: instead of the full number of genes); or, in other words, with a small number of dimensions instead of the full number of dimensions. Typically, only a few combinations of a data set's variables are informative and contribute to the data set's information content. The aim of dimensionality reduction is to extract these informative data set patterns."
    ),
    tags$p(
      HTML(
        "PCA (Principal Component Analysis) is an unsupervised method which separates data (here: samples) based on differences found within the data (here: differences in gene expression). <i>Unsupervised</i> means that the method has no knowledge on the underlying context, (here: for instance, which samples belonges to which group)."
      )
    ),
    tags$p(
      "After dimensionality reduction, the data set is described by so-called principal components (PCs). Each PC represents a data pattern which explains a certain amount of the variance (differences) found in the data set. By definition, the first PC (PC1) explains the largest amount of variance, the second PC (PC2) explains the second largest amount of variance, and so on. The scree plot (upper plot) shows how much of the total variance is explained by each PC. It only shows PCs down to 1% explained variance. However, there are always as many PCs as samples in the data set. You can select the PCs to be displayed in the settings on the right."
    ),
    tags$p(
      "You can overlay your provided grouping variable either with statistical ellipses covering 95% confidence intervals (cave: at least four samples are required) or convex hulls."
    ),
    tags$p(
      HTML(
        "<i>PCs are pre-computed using all samples of the data set (except they failed in quality control). If you want to excluded samples from the PCA, <a href='mailto:tobias.hundertmark@uniklinik-freiburg.de?subject=DExploreR - Request for PCA adjustment' target='_blank'>send us an email</a>.</i>"
      )
    )
  ),
  info_heatmap = tagList(
    tags$p(
      "This heatmap shows the normalized gene expression (centered and library size-normalized log-counts, i.e. counts per million (CPM)) of all selected samples. Samples are automatically clustered based on the selected genes as indicated by the dendrogram."
    ),
    tags$p(
      HTML(
        "By default the heatmap shows you the top 20 genes with the highest variance across all samples. However, you can toggle between genes with highest variance and genes with highest median expression across all samples. You can also change the number of genes to be displayed. <i>Variance and median are pre-computed, which means that unselecting some samples, will not change the genes shown in the heatmap even if the variances changed.</i>
      "
      )
    ),
    tags$p(
      "Furthermore, you can customize the y-axis label. You can display gene symbols or IDs. If a gene has no symbol, the corresponding ensembl ID will be shown instead. This is the case for predicted genes or pseudogenes, for instance."
    ),
    tags$p(
      HTML(
        "Finally, you can display your genes of interest either by using the drop down menu in the plot settings on the right, or by uploading a list of gene symbols as a plain <i>.txt</i> or <i>.csv</i> file (no column name needed, all gene symbols in one column). This search is based on gene symbol aliases, trying to increase the chances of finding your gene. There will be a message for genes that could not be found in the data set either because they are not expressed or the symbol was not found.<br>In some cases there are multiple hits. This will also be indicated, yet all multiple hits will be included in the heatmap but you can unselect the unwanted hits in the drop down menu in the plot settings.<br>If you cannot find you genes and have the feeling this is not how it is supposed to be, <a href='mailto:tobias.hundertmark@uniklinik-freiburg.de?subject=DExploreR - Request for gene expression heatmap' target='_blank'>send us an email</a>."
      )
    ),
  ),
  info_top_genes = tagList(
    tags$p(
      HTML(
        "The bar plot(s) show the top differentially expressed genes (up- or down-regulated) for the selected contrast(s), either by log2 fold change or by adjusted p-value (log-transformed, meaning large values correspond to small original p-values). Contrasts are pre-computed, if you are missing one, <a href='mailto:tobias.hundertmark@uniklinik-freiburg.de?subject=Request for different contrasts in DGEA (DExploreR)' target='_blank'>send us an email</a>."
      )
    ),
    tags$p(
      "By default, genes without a symbol, cDNA, predicted and pseudogenes are removed from the plot, but you can include them if desired."
    )
  ),
  info_volcano = tagList(
    tags$p(
      "The Volcano plot shows the results of the differential gene expression analysis (DGEA) for the selected contrast. In the plot settings on the right, you can select p-value and log2 fold change thresholds to highlight genes that are considered significantly differentially expressed."
    ),
    tags$p(
      "You can highlight genes of interest by selecting them from the drop down menu. If you uploaded a list of genes of interest in the heatmap section, you can transfer this list to the volcano plot and see how these genes behave in the differential gene expression analysis."
    ),
    tags$p(
      HTML(
        "Contrasts are pre-computed, if you are missing one, <a href='mailto:tobias.hundertmark@uniklinik-freiburg.de?subject=Request for different contrasts in DGEA  (DExploreR)' target='_blank'>send us an email</a>."
      )
    ),
  ),
  info_contrast_intersection = tagList(
    tags$p(
      HTML(
        " The <i>Jaccard Index</i> is a measure for the overlap between two sets of elements. In this case, the overlap of differentially expressed genes per contrast (So, this is a comparison of differential expression analyses). A value of 1 indicates that both contrasts contain the exact same set of differentially expressed genes, a value of 0 indicates that there is no overlap at all."
      )
    ),
    tags$p(
      "Jaccacrd indices are calculated for all contrasts and separately for up- and down-regulated genes as well as for up- and down-regulated genes together. The dotplot shows a the corresponding value with a black circle, which depicts a Jaccard index of 1. Fill color and size depict the value of the Jaccard index for this comparison."
    ),
    tags$p(
      HTML(
        "Contrasts are pre-computed, if you are missing one, <a href='mailto:tobias.hundertmark@uniklinik-freiburg.de?subject=Request for different contrasts in DGEA (DExploreR)' target='_blank'>send us an email</a>."
      )
    ),
  ),
  info_top_gene_sets = tagList(
    tags$p(
      "This doplot displays significantly enriched gene sets (p-value < 0.05) for the selected contrast(s), sorted by the magnitude of gene-set level change (a proxy for a fold change). The size of the dot scales with the number of genes in this gene set."
    ),
    tags$p(),
    tags$p(
      HTML(
        "Contrasts are pre-computed, if you are missing one, <a href='mailto:tobias.hundertmark@uniklinik-freiburg.de?subject=DExploreR - Request for different contrasts in GSEA' target='_blank'>send us an email</a>."
      )
    ),
    tags$p(
      HTML(
        "You can select up to 60 gene sets from <a href='https://www.gsea-msigdb.org/gsea/msigdb/human/genesets.jsp?collection=H' target='_blank'>Hallmarks</a>, <a href='https://www.gsea-msigdb.org/gsea/msigdb/human/genesets.jsp?collection=GO:BP' target='_blank'>GO Bio Process</a>, <a href='https://www.gsea-msigdb.org/gsea/msigdb/human/genesets.jsp?collection=GO:MF' target='_blank'>GO Molecular Function</a> and <a href='https://www.gsea-msigdb.org/gsea/msigdb/human/genesets.jsp?collection=GO:CC' target='_blank'>GO Cellular Component</a>. If you are interested in further gene sets, <a href='mailto:tobias.hundertmark@uniklinik-freiburg.de?subject=DExploreR - Request for further genes sets in GSEA' target='_blank'>send us an email</a>."
      )
    ),
  ),
  info_contrast_intersection_sets = tagList(
    tags$p(
      HTML(
        " The <i>Jaccard Index</i> is a measure for the overlap between two sets of elements. In this case, the overlap of significantly enriched gene sets per contrast. A value of 1 indicates that both contrasts contain the exact same set of differentially expressed genes, a value of 0 indicates that there is no overlap at all."
      )
    ),
    tags$p(
      "Jaccacrd indices are calculated for all contrasts and separately for up- and down-regulated gene sets as well as for up- and down-regulated gene sets together. The dotplot shows a the corresponding value with a black circle, which depicts a Jaccard index of 1. Fill color and size depict the value of the Jaccard index for this comparison."
    ),
    tags$p(
      HTML(
        "Contrasts are pre-computed, if you are missing one, <a href='mailto:tobias.hundertmark@uniklinik-freiburg.de?subject=DExploreR - Request for different contrasts in GSEA' target='_blank'>send us an email</a>."
      )
    )
  )
)
