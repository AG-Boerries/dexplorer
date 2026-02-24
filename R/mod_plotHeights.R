# Dynamically calculates the height of a plot based on the number of samples.
plot_height <- function(n_samples, min_size = 500, per_sample_size = 30) {
  height <- per_sample_size * n_samples
  return(max(height, min_size))
}

# Control the height of heatmaps
heatmap_height <- function(
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

# Control the height of the facetted plots
# This ensures the same width for bars and changes the height of each facet accordingly
# In contrast, by default, all facets have the same height, which looks messy, when the numnber of samples differs between groups
calculate_domains <- function(
  df,
  total_height,
  # This seems to be a suitable default value (based on experience)
  white_space = 35
) {
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
