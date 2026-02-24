format_for_gene_expression_heatmap <- function(
  df,
  selected_samples,
  selected_subset_size,
  selected_genes,
  gene_selection_by
) {
  # Get the user selected genes to always include them in the heatmap
  df_genes_selected <- if (
    !is.null(selected_genes) && length(selected_genes) > 0
  ) {
    df %>% filter(Symbol %in% selected_genes)
  } else {
    # If selection is empty, then create an empty data frame with the same columns as `df`
    df[0, ]
  }

  # Prepare the data frame for `heatmaply()`
  # `df` contains the cpm values of all genes and samples
  df <- df %>%
    # Show only top n genes with highest median expression or highest variance
    slice_max(
      order_by = .data[[
        if (gene_selection_by) "Rowvariance" else "Rowmedian"
      ]],
      n = selected_subset_size
    ) %>%
    # Add the user selected genes
    bind_rows(df_genes_selected) %>%
    # Ensure to remove duplicates, if genes were already in top n
    distinct(Symbol, .keep_all = TRUE) %>%
    # Use the column as row names that the user selected
    column_to_rownames(var = "Symbol") %>%
    # Remove unselected samples and non-numeric columns for z-scoring
    select(all_of(selected_samples)) %>%
    # Z-score cpm values for better depiction
    base::t() %>%
    scale() %>%
    base::t() %>%
    as.data.frame() %>%
    rownames_to_column(var = "Symbol") %>%
    # Re-add the further information, required for hover labels and comprehensive downloadable .csv
    left_join(
      df %>%
        dplyr::select(
          Symbol,
          GeneID,
          EntrezID,
          Description,
          Alias,
          NCBIURL
        ),
      by = "Symbol"
    )

  return(df)
}

gene_expression_heatmap <- function(
  df,
  id_or_sym,
  samples_groups,
  heatmap_colors,
  group_colors,
  dendrogram_type,
  heatmap_heights
) {
  # Check if the data frame fulfills the minimum requirements for a heatmap
  if (nrow(df) < 2 || ncol(df) < 2) {
    return(empty_plot("Please select at least\ntwo genes and two samples."))
  }

  # Translate values from `input$heatmap_dendrogram` to parameters of heatmaply
  dend_translate <- c(
    "Samples" = "column",
    "Genes" = "row",
    "Samples and genes" = "both",
    "None" = "none"
  )

  # Translate values from `input$switch_id_symbols_heatmap` to column names in `df`
  id_translate = list(
    "Ensembl ID" = "GeneID",
    "Entrez ID" = "EntrezID",
    "Gene symbol" = "Symbol",
    "Gene name" = "Description"
  )

  # Create custom color functions for heatmap and groups
  color_funs <- lapply(
    c(heatmap_colors, group_colors),
    create_heatmap_color_function
  )

  # Create the expression matrix with the selected identifier as rownames
  expression_mat <- df %>%
    column_to_rownames(var = id_translate[[id_or_sym]]) %>%
    select(where(is.numeric)) %>%
    data.matrix(rownames.force = TRUE)

  # Create a data frame with gene information for hover labels
  row_info <- df %>%
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

  # Create a data frame with group labels for the columns
  group_labels <- samples_groups %>%
    dplyr::select(SampleNameUser, Group) %>%
    column_to_rownames(var = "SampleNameUser") %>%
    # Ensure the order of the samples matches the order in the heatmap
    .[base::colnames(expression_mat), , drop = FALSE]

  # Build the heatmap
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
    colorbar_ypos = 1 - (350 / heatmap_heights$total_height),
    margins = c(50, 50, 20, 0),
    # Add row do heatmap containing the group labels
    ColSideColors = group_labels,
    # Color function for the group labels, can be different from the heatmap tiles
    col_side_palette = color_funs[[2]],
    # Allow reordering of the columns and rows
    Colv = TRUE,
    Rowv = TRUE
  )

  if (!base::grepl("Samples", dendrogram_type, fixed = TRUE)) {
    # When there is no dendrogram for the columns, the axis indices are shifted
    # Domains need to be added differently
    p <- p %>%
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
    p <- p %>%
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

  # Some further formatting, which is independent of the domains
  p <- p %>%
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
    ) %>%
    config(
      modeBarButtonsToRemove = c(
        "zoomIn2d",
        "zoomOut2d",
        "autoScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )

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

  # Further custom formatting of the heatmap
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

    if (identical(tr$type, "heatmap")) {
      if (base::nrow(tr$z) == 1) {
        # This is the custom tooltip for the group row, which is added by `heatmaply()` when using `ColSideColors`
        p$x$data[[i]]$text <- group_row_tooltips
        p$x$data[[i]]$name <- "group_row"
      }
    }
    # Remove default tooltip
    p$x$data[[i]]$hoverinfo <- "none"
  }

  # Enable custom tooltip
  p <- p %>%
    onRender(
      "
        function(el, x, tooltipType) {
          enableCustomTooltip(el, tooltipType);
        }
      ",
      data = list(tooltipType = "heatmap")
    )

  return(p)
}
