#' @title Create Interactive Gene Expression Heatmap
#'
#' @description
#' Generates an interactive gene expression heatmap using \code{\link[heatmaply]{heatmaply}()}, with custom color palettes, group annotations, dendrogram options, and detailed tooltips for each cell. Supports dynamic row/column clustering, custom group colors, and advanced layout adjustments for publication-quality visualization.
#'
#' @param df A data frame formatted by \code{\link{formatForHeatmap}()}.
#'
#' @param id_or_sym Character. The gene identifier (EntrezID, Ensembl ID, or even the "long" gene name) or the gene symbol to use as row names in the heatmap. Options are "Ensembl ID", "Entrez ID", "Gene symbol", and "Gene name". Defaults to "Gene symbol".
#'
#' @param samples_groups A data frame mapping sample names to group labels for column annotations.
#'
#' @param heatmap_colors Character. The color palette to use for the heatmap tiles. Defaults to "App colors".
#'
#' @param group_colors Character. The color palette to use for group labels. Defaults to "inferno".
#'
#' @param dendrogram_type Character. Dendrogram display option: "Samples", "Genes", "Samples and genes", or "None". Defaults to "Samples and genes".
#'
#' @param heatmap_heights A list of height and domain settings for the heatmap and its components, created by \code{\link{heatmapHeights}()}.
#'
#' @param color_scale_order Logical. Whether to use the standard order of colors (TRUE) or reverse order (FALSE). Defaults to TRUE.
#'
#' @return The interactive heatmap as a `plotly` object.
#'
#' @export
createGeneExpressionHeatmap <- function(
  df,
  samples_groups,
  heatmap_heights,
  id_or_sym = "Gene symbol",
  heatmap_colors = "App colors",
  group_colors = "inferno",
  dendrogram_type = "Samples and genes",
  color_scale_order = TRUE
) {
  # Define variables locally for R CMD check
  SampleNameUser <- Group <- . <- NULL

  # ---- Check input parameters for validity ----
  match.arg(
    id_or_sym,
    choices = c("Ensembl ID", "Entrez ID", "Gene symbol", "Gene name")
  )
  match.arg(
    dendrogram_type,
    choices = c("Samples", "Genes", "Samples and genes", "None")
  )
  match.arg(heatmap_colors, choices = color_choices_flat)
  match.arg(group_colors, choices = color_choices_flat)
  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "samples_groups must be a data frame with columns 'SampleNameUser' and 'Group'" = is.data.frame(
      samples_groups
    ) &&
      all(c("SampleNameUser", "Group") %in% colnames(samples_groups)),
    "heatmap_heights must be a list with the names 'total_height', 'dendro_domain', 'group_domain', and 'tiles_domain'" = is.list(
      heatmap_heights
    ) &&
      all(
        c("total_height", "dendro_domain", "group_domain", "tiles_domain") %in%
          names(heatmap_heights)
      )
  )

  # ---- Check if the data frame fulfills the minimum requirements for a heatmap ----
  if (nrow(df) < 2 || ncol(df) < 2) {
    return(empty_plot("Please select at least\ntwo genes and two samples."))
  }

  # ---- Tranlations from UI inputs to function parameters ----
  # Translate values from `input$heatmap_dendrogram` to parameters of heatmaply
  dend_translate <- c(
    "Samples" = "column",
    "Genes" = "row",
    "Samples and genes" = "both",
    "None" = "none"
  )

  # Translate values from `input$switch_id_symbols_heatmap` to column names in `df`
  id_translate <- list(
    "Ensembl ID" = "GeneID",
    "Entrez ID" = "EntrezID",
    "Gene symbol" = "Symbol",
    "Gene name" = "Description"
  )

  # ---- Create custom color functions for heatmap and groups ----
  color_funs <- lapply(
    c(heatmap_colors, group_colors),
    create_heatmap_color_function,
    standard_order = color_scale_order
  )

  # ---- Create the expression matrix with the selected identifier as rownames ----
  expression_mat <- df |>
    column_to_rownames(var = id_translate[[id_or_sym]]) |>
    select(where(is.numeric)) |>
    data.matrix(rownames.force = TRUE)

  # ---- Create the hover labels ----
  # Create a data frame with gene information for hover labels
  row_info <- df |>
    select(-where(is.numeric))

  # Create a data frame with sample and group information for hover labels
  col_info <- left_join(
    data.frame(SampleNameUser = base::colnames(expression_mat)),
    samples_groups,
    by = "SampleNameUser"
  )

  # Create custom hover labels
  hover_labels <- matrix(
    paste0(
      "<b><div style='font-size:16px;'>Sample: </b>",
      base::colnames(expression_mat)[col(expression_mat)],
      "<br><b>Group: </b>",
      col_info$Group[col(expression_mat)],
      "<br><b>Gene: </b>",
      base::rownames(expression_mat)[row(expression_mat)],
      "</div><hr><b>Z-score: </b>",
      sprintf('%.2f', expression_mat),
      "<br><br><b>Ensembl ID: </b>",
      row_info$GeneID[row(expression_mat)],
      "<br><b>Entrez ID: </b>",
      row_info$EntrezID[row(expression_mat)],
      "<br><b>Description: </b>",
      row_info$Description[row(expression_mat)],
      "<br><b>Alias: </b>",
      row_info$Alias[row(expression_mat)],
      "<hr>",
      "For further information visit <a href='",
      row_info$NCBIURL[row(expression_mat)],
      "' target='_blank'>NCBI</a>."
    ),
    nrow = base::nrow(expression_mat),
    ncol = base::ncol(expression_mat),
    dimnames = base::dimnames(expression_mat)
  )

  # ---- Create group labels ----
  group_labels <- samples_groups |>
    dplyr::select(SampleNameUser, Group) |>
    column_to_rownames(var = "SampleNameUser") |>
    # Ensure the order of the samples matches the order in the heatmap
    (\(x) x[base::colnames(expression_mat), , drop = FALSE])()

  # ---- Build the heatmap ----
  p <- heatmaply(
    expression_mat,
    # Color of the heatmap tiles
    colors = color_funs[[1]],
    column_text_angle = 270,
    xlab = "Samples",
    ylab = "Genes",
    key.title = "Z-score",
    # Defined which dendrograms to show
    dendrogram = dend_translate[[dendrogram_type]],
    hclust_method = "ward.D2",
    dist_method = "euclidean",
    branches_lwd = 0.3,
    custom_hovertext = hover_labels,
    grid_gap = 1,
    plot_method = "plotly",
    dend_hoverinfo = FALSE,
    colorbar_yanchor = "top",
    # Fix at a specific position rather towards the top of the heatmap, otherwise this floats in nirwana
    colorbar_ypos = 1 - (400 / heatmap_heights$total_height),
    margins = c(50, 50, 20, 0),
    # Add row do heatmap containing the group labels
    ColSideColors = group_labels,
    # Color function for the group labels, can be different from the heatmap tiles
    col_side_palette = color_funs[[2]],
    # Allow reordering of the columns and rows
    Colv = TRUE,
    Rowv = TRUE
  )

  # ---- Adjust the domains of the heatmap ----
  if (!base::grepl("Samples", dendrogram_type, fixed = TRUE)) {
    # When there is no dendrogram for the columns, the axis indices are shifted
    # Domains need to be added differently
    p <- p |>
      layout(
        # Heatmap tiles
        yaxis2 = list(
          title = list(
            text = "Genes",
            font = list(color = "black", size = 16),
            # Add some space between axis title and tick labels
            standoff = 20
          ),
          tickfont = list(color = "black"),
          domain = heatmap_heights$tiles_domain
        ),
        # Group labels
        yaxis = list(
          tickfont = list(color = "black"),
          domain = heatmap_heights$group_domain
        )
      )
  } else {
    p <- p |>
      layout(
        # Change the color of the y-axis title and tick labels
        # Heatmap tiles
        yaxis3 = list(
          title = list(
            text = "Genes",
            font = list(color = "black", size = 16),
            # Add some space between axis title and tick labels
            standoff = 20
          ),
          tickfont = list(color = "black"),
          domain = heatmap_heights$tiles_domain
        ),
        # Group labels
        yaxis2 = list(
          tickfont = list(color = "black"),
          domain = heatmap_heights$group_domain
        ),
        # Dendrogram
        yaxis = list(domain = heatmap_heights$dendro_domain)
      )
  }

  # ---- Group hover labels ----
  # Extract the index of the heatmap trace
  main_heatmap_idx <- base::which(
    vapply(
      p$x$data,
      function(tr) identical(tr$type, "heatmap") && base::nrow(tr$z) > 1,
      logical(1)
    )
  )

  # Extract the sample order after clustering by `heatmaply()`
  if (length(main_heatmap_idx) > 0) {
    main_heatmap <- p$x$data[[main_heatmap_idx[1]]]
    clustered_sample_order <- base::colnames(main_heatmap$z)
  }

  # Reorder the `samples_groups` data frame according to the clustered sample order
  ord <- base::match(
    base::colnames(main_heatmap$z),
    samples_groups$SampleNameUser
  )
  samples_groups_reordered <- samples_groups[ord, ]

  # Create the tooltips content for the group row
  group_row_tooltips <- paste0(
    "<b>Sample: </b>",
    samples_groups_reordered$SampleNameUser,
    "<br>",
    "<b>Group: </b>",
    samples_groups_reordered$Group
  )

  # ---- Plot fine tuning ----
  # Via layout and config
  p <- p |>
    layout(
      # Change the color of the xaxis title and tick labels
      xaxis = list(
        title = list(
          text = "Samples",
          font = list(color = "black", size = 16),
          # Add some space between axis title and tick labels
          standoff = 20
        ),
        tickfont = list(color = "black")
      )
    ) |>
    config(
      modeBarButtonsToRemove = c(
        "zoomIn2d",
        "zoomOut2d",
        "autoScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    ) |>
    # Enable custom tooltip for the heatmap
    onRender(
      "
        function(el, x, tooltipType) {
          enableCustomTooltip(el, tooltipType);
        }
      ",
      data = list(tooltipType = "heatmap")
    )

  # Via direct manipulation of the `plotly` object
  for (i in seq_along(p$x$data)) {
    tr <- p$x$data[[i]]
    if (identical(tr$type, "heatmap") && !is.null(tr$colorbar)) {
      # Manual adjustments of the color bar
      # Harmonize x position
      p$x$data[[i]]$colorbar$x <- 1.1
      p$x$data[[i]]$colorbar$xanchor <- "left"
      # Change text color to black
      p$x$data[[i]]$colorbar$titlefont <- list(color = "black")
      p$x$data[[i]]$colorbar$tickfont <- list(color = "black")
      # Fix length in pixels
      p$x$data[[i]]$colorbar$lenmode <- "pixels"
      p$x$data[[i]]$colorbar$len <- 300
    }

    # Add the custom tooltip for the group row
    if (identical(tr$type, "heatmap")) {
      if (base::nrow(tr$z) == 1) {
        p$x$data[[i]]$text <- group_row_tooltips
        p$x$data[[i]]$name <- "group_row"
      }
    }
    # Remove default tooltip
    p$x$data[[i]]$hoverinfo <- "none"
  }

  return(p)
}
