#' @title Calculate Dynamic Plot Height
#'
#' @description
#' Dynamically calculates the height of a plot based on the number of samples, ensuring a minimum height for readability.
#'
#' @param n_samples Integer. The number of samples to display in the plot.
#'
#' @param min_size Numeric. The minimum height (in pixels) for the plot. Default is 500.
#'
#' @param per_sample_size Numeric. The height (in pixels) to allocate per sample. Default is 30.
#'
#' @return Numeric. The calculated plot height in pixels.
#'
calculatePlotHeight <- function(
  n_samples,
  min_size = 500,
  per_sample_size = 30
) {
  height <- per_sample_size * n_samples
  return(max(height, min_size))
}

#' @title Calculate Heatmap Component Heights and Domains
#'
#' @description
#' Computes the total height and vertical domains for the main components of a gene expression heatmap (tiles, group annotations, and dendrogram), based on the number of genes and user-specified component heights. Ensures a minimum total height and dynamically adjusts dendrogram height.
#'
#' @param n_genes Integer. Number of genes (rows) in the heatmap.
#'
#' @param tiles_height Numeric. Height (in pixels) allocated per gene for the heatmap tiles. Default is 40 (This proved to look nice.).
#'
#' @param groups_height Numeric. Height (in pixels) allocated for group annotation bars. Default is 60 (This proved to look nice.).
#'
#' @param dendro_height Numeric. Height (in pixels) allocated for the dendrogram. Default is 160 (This proved to look nice.).
#'
#' @param dendro_type Character. Dendrogram display option: "Samples", "Genes", "Samples and genes", or "None". If "Samples" is not present, dendrogram height is set to 0.
#'
#' @return A list with heights for the heatmap components:
#'
#' * `total_height`
#'
#' * `dendro_domain`
#'
#' * `group_domain`
#'
#' * `tiles_domain`
#'
#' @export
heatmapHeights <- function(
  n_genes,
  tiles_height = 40,
  groups_height = 60,
  dendro_height = 160,
  dendro_type = NULL
) {
  # Set `dendro_height` to 0 if no sample dendrogram is requested
  if (!base::grepl("Samples", dendro_type, fixed = TRUE)) {
    dendro_height <- 0
  }

  # Calculate total height
  total_height <- tiles_height * n_genes + groups_height + dendro_height

  # Set min height to 800 pixels
  if (total_height < 800) {
    total_height <- 800
  }

  # Calculate the domains for each component
  dendro_domain <- c(1 - (dendro_height / total_height), 1)
  group_domain <- c(
    dendro_domain[1] - (groups_height / total_height),
    dendro_domain[1]
  )
  tiles_domain <- c(0, group_domain[1])

  return(list(
    total_height = total_height,
    dendro_domain = dendro_domain,
    group_domain = group_domain,
    tiles_domain = tiles_domain
  ))
}

#' @title Calculate Facet Plot Domains
#'
#' @description
#' Calculates the vertical domains for each group/facet in a facetted plot, ensuring proportional heights based on the number of samples per group and consistent bar widths. Adds white space between facets and returns the original data frame with an added `domain` column for each group.
#'
#' @param df A data frame with columns `group` (group/facet identifier), `n_samples` (number of samples in each group) and `yaxis` (name of the y-axis form the plotly object).
#'
#' @param total_height The total height (in pixels) allocated for the plot.
#'
#' @param white_space The amount of white space (in pixels) to insert between facets. Default is 35 (this seems suitable based on experience).
#'
#' @return The input data frame with an added `domain` column (a list-column of length-2 numeric vectors giving the lower and upper bounds for each facet's domain).
#'
#' @export
calculateDomains <- function(
  df,
  total_height,
  white_space = 35
) {
  # Define variables locally for R CMD check
  n_samples <- NULL

  # Calculate number of groups/facets
  n_groups <- length(df$group)
  # Fraction of white space from total height
  white_space_height <- white_space / total_height
  # The actual plotting height
  plotting_height <- total_height - ((n_groups - 1) * white_space)
  # Total number of all samples across groups
  total_samples <- sum(df$n_samples)

  # Calculate domain height for each group
  df <- df %>%
    mutate(
      domain_height = n_samples / total_samples * plotting_height / total_height
    )

  # Calculate domain for each row
  get_domains <- function(domain_heights, white_space_height) {
    n <- length(domain_heights)
    uppers <- numeric(n)
    lowers <- numeric(n)
    uppers[1] <- 1
    lowers[1] <- 1 - domain_heights[1]
    for (i in 2:n) {
      uppers[i] <- lowers[i - 1] - white_space_height
      lowers[i] <- uppers[i] - domain_heights[i]
    }
    tibble(domain = map2(lowers, uppers, ~ c(.x, .y)))
  }

  domains_tbl <- get_domains(df$domain_height, white_space_height)

  df <- bind_cols(df, domains_tbl)

  return(df)
}
