#' @title Format Data for GSEA Contrast Intersection
#'
#' @description
#' Formats a data frame of enriched gene sets for Jaccard index calculation between pairs of contrasts. Returns a summary table with gene set lists and statistics for each pair.
#'
#' @param df A data frame containing GSEA information, including columns for contrasts, direction and patways.
#'
#' @param selected_directions Character vector specifying which directions ("up", "down", "both") to include.
#'
#' @return A data frame summarizing Jaccard index results for each pair of contrasts and direction, including gene set lists and statistics.
#'
#' @export
formatGSEAContrastIntersection <- function(
  df,
  selected_directions
) {
  # Define variables locally for R CMD check
  Direction <- Contrast <- Pathway <- Var1 <- Var2 <- NULL

  # Possible directions
  directions <- c("up", "down", "both")

  # For the calculation of the Jaccard index, at least two contrasts are required
  if (length(unique(df$Contrast)) < 2) {
    return(data.frame())
  }

  #  Prepare data frame for ballon plot
  df_jaccard_results <- bind_rows(
    lapply(directions, function(dir) {
      df_jaccard <- df %>%
        filter((dir == "both" | Direction == dir)) %>%
        group_by(Contrast) %>%
        # Extract the unique list of pathways for each contrast
        summarise(paths = list(unique(Pathway)), .groups = "drop")

      # Calculate the possible combinations of contrasts
      pairs <- base::expand.grid(
        df_jaccard$Contrast,
        df_jaccard$Contrast,
        stringsAsFactors = FALSE
      ) %>%
        # This removes duplicates and self-comparisons
        filter(Var1 < Var2)

      # Compute Jaccard index for each pair
      jaccard_results <- map_dfr(1:base::nrow(pairs), function(i) {
        seta <- df_jaccard %>% filter(Contrast == pairs[i, 1])
        setb <- df_jaccard %>% filter(Contrast == pairs[i, 2])

        inter <- length(base::intersect(seta$paths[[1]], setb$paths[[1]]))
        uni <- length(base::union(seta$paths[[1]], setb$paths[[1]]))

        tibble(
          Seta = seta$Contrast,
          Setb = setb$Contrast,
          Pathways_both_sets = inter,
          Pathways_total = uni,
          JI = inter / uni,
          Direction = dir
        )
      })
    })
  ) %>%
    as.data.frame()

  return(df_jaccard_results)
}

#' @title Create GSEA Contrast Intersection Plot
#'
#' @description
#' Generates an interactive `plotly` visualization of Jaccard indices for all pairs of contrasts in a gene set enrichment analysis (GSEA). The plot displays the overlap of enriched gene sets between contrasts, with dot size and color representing the Jaccard index. Tooltips provide detailed comparison information, and facets show results for different regulation directions.
#'
#' @param df A data frame as returned by \code{\link{formatGSEAContrastIntersection}()}, summarizing Jaccard index results for each pair of contrasts and direction.
#'
#' @param selected_palette Character. The name of the color palette to use for the plot.
#'
#' @return An interactive dotplot for Jaccard indicies for enriched gene sets by contrasts as a `plotly` object.
#'
#' @export
createGSEAContrastIntersectionPlot <- function(df, selected_palette) {
  # Define variables locally for R CMD check
  Seta <- Setb <- JI <- Pathways_both_sets <- Pathways_total <- Direction <- TooltipText1 <- TooltipText2 <- JIMax <- NULL

  # If data frame is empty, then return an empty plot with a message
  if (all(dim(df) == 0)) {
    return(empty_plot("Not enough contrasts provided."))
  }

  # Create new labels for the facets
  facet_labels <- c(
    up = "Upregulated gene sets",
    down = "Downregulated gene sets"
  )

  # Add tooltip just before plotting to avoid this in the downloaded data
  df <- df %>%
    mutate(
      TooltipText1 = paste0(
        "<b><div style='font-size:16px;'>Comparison: </b>",
        Seta,
        " and ",
        Setb,
        "</div><hr><b>Jaccard index: </b>",
        sprintf("%.3f", JI),
        "<br><b>Gene sets enriched in both: </b>",
        Pathways_both_sets,
        "<br><b>Total gene sets: </b>",
        Pathways_total
      ),
      TooltipText2 = paste0(
        "<b>Maximum Jaccard index:</b><br><i>All gene sets would be similarly enriched.</i>"
      ),
      JIMax = 1
    )

  p_up_and_down <- ggplot(
    df %>%
      filter(Direction == "both") %>%
      mutate(Direction = "Up- and down-regulated gene sets"),
    aes(x = Seta, y = Setb)
  ) +
    geom_point_quiet(
      aes(
        size = JI,
        fill = JI,
        text = TooltipText1
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
        text = TooltipText1
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

  # Add selected colors to the plots
  p_up_or_down <- add_selected_colors(
    p = p_up_or_down,
    selected_palette = selected_palette,
    color_by = plot_components
  )
  p_up_and_down <- add_selected_colors(
    p = p_up_and_down,
    selected_palette = selected_palette,
    color_by = plot_components
  )

  p <- subplot(
    ggplotly(
      p_up_and_down,
      tooltip = "text",
      # height = plot_height(
      #   n_samples = nrow(df) / 3,
      #   min_size = 650,
      #   per_sample_size = 15
      # )
    ),
    ggplotly(
      p_up_or_down,
      tooltip = "text",
      # height = plot_height(
      #   n_samples = nrow(df) / 3,
      #   min_size = 650,
      #   per_sample_size = 15
      # )
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
