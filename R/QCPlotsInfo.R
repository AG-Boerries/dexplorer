#' @title Lookup table for QC plots and info
#'
#' @description
#' This is the lookup table for the plotting functions and the info texts of the `moduleServer` in `R/QCServer.R`. It is wrapped in a function so that function references are resolved at call time, not at parse time.
#'
assign_format_plot_info <- function() {
  list(
    "Number of reads" = list(
      plot = createReadCountPlot,
      info = tagList(
        tags$b("Assigned reads:"),
        "Number of mapped reads that could be assigned unambiguously to an annotated genomic region.",
        br(),
        br(),
        tags$b("Unassigned / mapped reads:"),
        "Number of reads that could be mapped to the reference but without unique assignement, for instance, because of overlapping annotated genomic regions or no available annotation.",
        br(),
        br(),
        tags$b("Unassigned / unmapped reads:"),
        "Number of reads that did not map to the reference or that mapped to many locations.",
        br(),
        br(),
        "In the case of paired-end sequencing, read pairs are counted instead of single reads."
      )
    ),
    "Number of genes" = list(
      plot = createGeneCountPlot,
      info = HTML(
        "To be considered as detected, a gene must have at least one read assigned, i.e. at least one count. The bar <i>All samples</i> depicts the overall number of distinct detected genes across all samples."
      )
    ),
    "Read count distribution" = list(
      plot = createCountDistributionPlot,
      info = "The read count distribution shows how the reads are distributed across all recorded genes in a sample. The vertical lines indicate the quartiles separating the distribution into equal proportions with 25 % of the data. Then central line indicates the median read counts per gene."
    )
  )
}
