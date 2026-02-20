format_for_dgea_contrast_intersection <- function(
  df,
  selected_directions,
  p_threshold,
  l2fc_threshold
) {
  # Possible directions
  directions <- c("up", "down", "both")

  # For the calculation of the Jaccard index, at least two contrasts are required
  if (length(unique(df$Contrast)) < 2) {
    return(data.frame())
  }

  # Prepare data frame for ballon plot
  df_jaccard_results <- bind_rows(
    lapply(directions, function(dir) {
      df_jaccard <- df %>%
        filter(
          # Filter by direction
          (dir == "both" | Direction == dir),
          # Get the user-defined thresholds
          PValAdj < as.numeric(p_threshold),
          abs(Log2FC) > l2fc_threshold
        ) %>%
        group_by(Contrast) %>%
        # Extract the unique list of genes for each contrast
        summarise(GeneID = list(unique(GeneID)), .groups = "drop")

      # Calculate the possible combinations of contrasts
      pairs <- expand.grid(
        df_jaccard$Contrast,
        df_jaccard$Contrast,
        stringsAsFactors = FALSE
      ) %>%
        # This removes duplicates and self-comparisons
        filter(Var1 < Var2)

      # Compute Jaccard index for each pair
      jaccard_results <- map_dfr(1:nrow(pairs), function(i) {
        seta <- df_jaccard %>% filter(Contrast == pairs[i, 1])
        setb <- df_jaccard %>% filter(Contrast == pairs[i, 2])

        inter <- length(intersect(seta$GeneID[[1]], setb$GeneID[[1]]))
        uni <- length(union(seta$GeneID[[1]], setb$GeneID[[1]]))

        df_genes <- full_join(
          # Combine the gene ids from both sets
          unnest(seta, cols = "GeneID"),
          unnest(setb, cols = "GeneID"),
          by = "GeneID",
          suffix = c(".a", ".b"),
        ) %>%
          # Create two columns with the names of the contrasts
          # Each row is a gene and check, which genes are differentially expressed in which contrast
          mutate(
            !!seta$Contrast := ifelse(
              !is.na(Contrast.a),
              TRUE,
              FALSE
            ),
            !!setb$Contrast := ifelse(
              !is.na(Contrast.b),
              TRUE,
              FALSE
            )
          ) %>%
          # Remove the original contrast columns
          select(-c(Contrast.a, Contrast.b)) %>%
          # Sort genes, so that differentially expressed genes appear on top
          arrange(desc(.[2]), desc(.[3])) %>%
          # Add further gene information for tooltip
          left_join(
            df %>%
              select(all_of(c(
                "GeneID",
                "Symbol",
                "Alias",
                "EntrezID",
                "Description",
                "NCBIURL"
              ))) %>%
              # This dataframe is inflated, because of the occurance of the same gene in multiple comparisons
              distinct(.keep_all = TRUE),
            by = "GeneID"
          )

        # Rearrange column order
        df_genes <- df_genes[, c(2, 3, 1, 4, 5, 6, 7, 8)]

        tibble(
          Seta = seta$Contrast,
          Setb = setb$Contrast,
          DEG_both_sets = inter,
          DEG_total = uni,
          JI = inter / uni,
          Direction = dir,
          # To not expand the dataframe, save it as a list
          Genes = list(df_genes)
        )
      })
    })
  ) %>%
    as.data.frame()

  return(df_jaccard_results)
}

plot_dgea_contrast_intersection <- function(df, selected_palette) {
  # If data frame is empty, then return an empty plot with a message
  if (all(dim(df) == 0)) {
    return(empty_plot("Not enough contrasts provided."))
  }

  # Create new labels for the facets
  facet_labels <- c(
    up = "Upregulated\ngenes",
    down = "Downregulated\ngenes"
  )

  # Add tooltipp text just before plotting to avoid them being contained in the data download
  df <- df %>%
    mutate(
      TooltipText1 = paste0(
        "<b><div style='font-size:16px;'>Comparison: </b>",
        Seta,
        " and ",
        Setb,
        "</div><hr><b>Jaccard index: </b>",
        sprintf("%.3f", JI),
        "<br><b>DEGs contained in both: </b>",
        DEG_both_sets,
        "<br><b>Total DEGs: </b>",
        DEG_total
      ),
      TooltipText2 = paste0(
        "<b>Maximum Jaccard index:</b><br><i>All genes would be similarly expressed.</i>"
      ),
      JIMax = 1,
      # Create one column with the unique information to extract the genes for this JI
      CustomData = paste0(Seta, "|", Setb, "|", Direction)
    )

  p_up_and_down <- ggplot(
    df %>%
      filter(Direction == "both") %>%
      mutate(Direction = "Up- and down-regulated genes"),
    aes(x = Seta, y = Setb)
  ) +
    geom_point_quiet(
      aes(
        size = JI,
        fill = JI,
        text = TooltipText1,
        customdata = CustomData
      ),
      shape = 21,
      color = "black",
      show.legend = FALSE
    ) +
    # Plot the maximum Jaccard index as reference
    geom_point_quiet(
      aes(size = JIMax, text = TooltipText2),
      show.legend = FALSE,
      shape = 1,
      color = "black"
    ) +
    facet_wrap(~Direction) +
    # Resize the dots, so that they are not too small when the Jaccard index is low
    scale_size(range = c(2, 12)) +
    labs(x = "", y = "")

  p_up_or_down <- ggplot(
    df %>% filter(Direction != "both"),
    aes(x = Seta, y = Setb)
  ) +
    geom_point_quiet(
      aes(
        size = JI,
        fill = JI,
        text = TooltipText1,
        customdata = CustomData
      ),
      shape = 21,
      color = "black",
      show.legend = FALSE
    ) +
    # Plot the maximum Jaccard index as reference
    geom_point_quiet(
      aes(size = JIMax, text = TooltipText2),
      show.legend = FALSE,
      shape = 1,
      color = "black"
    ) +
    facet_wrap(~Direction, labeller = as_labeller(facet_labels)) +
    # Resize the dots, so that they are not too small when the Jaccard index is low
    scale_size(range = c(2, 8)) +
    labs(x = "", y = "")

  # Define the components to color by because they cannot be inferred from this plot
  plot_components <- data.frame(
    aes = "fill",
    aes_name = "JI",
    aes_cont = TRUE,
    aes_n = NA_integer_
  )

  # Add the selected color scale
  p_up_and_down <- add_selected_colors(
    p = p_up_and_down,
    selected_palette = selected_palette,
    color_by = plot_components
  )
  p_up_or_down <- add_selected_colors(
    p = p_up_or_down,
    selected_palette = selected_palette,
    color_by = plot_components
  )

  p <- subplot(
    ggplotly(
      p_up_and_down,
      tooltip = "text",
      # height = plot_height(
      #   n_samples = nrow(df) / 3,
      #   min_size = 800,
      #   per_sample_size = 15
      # ),
      source = "dgea_jaccard"
    ),
    ggplotly(
      p_up_or_down,
      tooltip = "text",
      # height = plot_height(
      #   n_samples = nrow(df) / 3,
      #   min_size = 800,
      #   per_sample_size = 15
      # ),
      source = "dgea_jaccard"
    ),
    nrows = 2,
    margin = 0.05
  ) %>%
    # Reduce the modebar to only essential tools
    config(
      displaylogo = FALSE,
      modeBarButtons = list(
        list("toImage"),
        list("zoom2d"),
        list("pan2d"),
        list("resetScale2d")
      )
    ) %>%
    # Register click events for this plot
    event_register("plotly_click") %>%
    # Attach the custom tooltip from JS
    onRender(
      "
        function(el, x, tooltipType) {
          enableCustomTooltip(el, tooltipType);
        }
      ",
      data = list(tooltipType = "jaccard")
    )

  # Remove default tooltip
  for (i in seq_along(p$x$data)) {
    p$x$data[[i]]$hoverinfo <- "none"
  }

  return(p)
}
