# Color scale for theme colors
get_theme_colors <- function(color = NULL, n_values = NULL) {
  # Define theme colors
  theme_colors <- c(
    orange = "#f5a900ff",
    blue = "#1e4385ff",
    green = "#04f1a7ff",
    pink = "#c855fcff"
  )

  # Return all colors unnamed, when nothing specified
  if (is.null(color) && is.null(n_values)) {
    return(base::unname(theme_colors))
  }

  if (!is.null(n_values)) {
    if (n_values <= 4) {
      return(base::unname(theme_colors[1:n_values]))
    } else {
      return(colorRampPalette(theme_colors)(n_values))
    }
  }

  # Return color by index
  if (is.numeric(color)) {
    if (all(color %in% 1:4)) {
      return(base::unname(theme_colors[color]))
    } else {
      stop("Numeric index must be between 1 and 4.")
    }
  }

  # Return color by name
  if (is.character(color)) {
    color <- tolower(color)
    if (all(color %in% names(theme_colors))) {
      return(base::unname(theme_colors[color]))
    } else {
      stop(
        "Color name must be one of: orange, blue, green, yellow (not cap-sensitive)."
      )
    }
  }
}

# Define available colors
color_choices <- list(
  "App theme" = "App colors",
  "Viridis" = c(
    "viridis",
    "magma",
    "inferno",
    "plasma",
    "cividis",
    "turbo"
  ),
  "Wes Anderson" = names(wesanderson::wes_palettes),
  "RColorBrewer" = rownames(RColorBrewer::brewer.pal.info)
)

# Flatten color choices
color_choices_flat <- base::unname(unlist(color_choices))

# Extract the color family of the selected palette
# Although this information is contained in `color_choices`, the corresponding UI element returns an unnamed value
get_color_family <- function(selected_palette) {
  return(
    names(color_choices)[vapply(
      color_choices,
      function(x) selected_palette %in% x,
      logical(1)
    )]
  )
}

# When the mapped aesthetic is discrete, get the corresponding color palette
get_discrete_palette <- function(family, palette, n) {
  if (family == "App theme") {
    return(get_theme_colors(n_values = n))
  }

  if (family == "Viridis") {
    return(viridis(n, option = palette))
  }

  if (family == "Wes Anderson") {
    # Most of these palettes have 4 to 5 colors only
    # `wes_palette()` with `type = "continuous"` can inherently offer more values than the specified scale
    # `type = "discrete"`, which is required here, cannot, thus use workaround with `colorRampPalette()`
    return(colorRampPalette(wes_palette(palette, type = "discrete"))(n))
  }

  if (family == "RColorBrewer") {
    # Check if the chosen plalette can offer enough colors
    # If not use `colorRampPalette()` to extend it
    max_n <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
    base <- brewer.pal(min(n, max_n), palette)
    if (n > max_n) colorRampPalette(base)(n) else base
  }
}

# When the mapped aesthetic is continuous, get the corresponding color scale
get_continuous_scale <- function(family, palette, aes) {
  # Select the correct ggplot2 scale function based on the mapped aesthetic
  scale_fun <- if (aes == "fill") {
    scale_fill_continuous
  } else {
    scale_color_continuous
  }

  if (family == "App theme") {
    scale_fun(palette = get_theme_colors(c(4, 3, 1)))
  } else if (family == "Viridis") {
    # Viridis has its own continuous scale functions
    if (aes == "fill") {
      ggplot2::scale_fill_viridis_c(option = palette)
    } else {
      ggplot2::scale_color_viridis_c(option = palette)
    }
  } else if (family == "Wes Anderson") {
    scale_fun(palette = wesanderson::wes_palettes[[palette]])
  } else if (family == "RColorBrewer") {
    scale_fun(palette = palette)
  }
}

add_selected_colors <- function(p, selected_palette, color_by = NULL) {
  # When there is no mapping, return the plot as is
  # Important for app start up
  if (length(p$mapping) == 0) {
    return(p)
  }

  if (!is.null(color_by)) {
    # For some border cases, `plot_components` need to be provided manually,
    # Because it cannot be inferred from the plot object
    plot_components <- color_by
  } else {
    # Extract mapped aesthetics from the plot object
    aes <- intersect(names(p$mapping), c("fill", "colour"))
    # Extrat the name of the mapped variable for each aesthetic
    aes_name <- unlist(lapply(aes, function(x) rlang::as_name(p$mapping[[x]])))
    # Check if the mapped variable is continuous or discrete
    aes_cont <- unlist(lapply(aes_name, function(x) is.numeric(p$data[[x]])))
    # Count the number of unique discrete values if applicable
    aes_n <- mapply(
      function(aes_name, aes_cont) {
        if (aes_cont) {
          NA_integer_
        } else {
          length(unique(p$data[[aes_name]]))
        }
      },
      aes_name,
      aes_cont
    )

    # Construct the `plot_components`
    plot_components <- data.frame(
      aes = aes,
      aes_name = aes_name,
      aes_cont = aes_cont,
      aes_n = aes_n
    )
  }

  # Extract the color family of the selected palette
  selected_family <- get_color_family(selected_palette)

  # Add the colorscales for all mapped aesthetics
  p <- reduce(
    # Row-wise operation over the `plot_components`
    split(plot_components, seq_len(nrow(plot_components))),
    function(p, row) {
      if (row$aes_cont) {
        p +
          get_continuous_scale(
            family = selected_family,
            palette = selected_palette,
            aes = row$aes
          )
      } else {
        cols <- get_discrete_palette(
          family = selected_family,
          palette = selected_palette,
          n = row$aes_n
        )

        p +
          if (row$aes == "fill") {
            ggplot2::scale_fill_manual(values = cols)
          } else {
            ggplot2::scale_color_manual(values = cols)
          }
      }
    },
    .init = p
  )

  return(p)
}

# Create a color function for heatmaps from heatmaply
create_heatmap_color_function <- function(selected_colors) {
  ht_color_family <- get_color_family(selected_colors)

  if (ht_color_family == "App theme") {
    cols <- get_theme_colors(c(4, 3, 1))
  } else if (ht_color_family == "Viridis") {
    cols <- viridis(3, option = selected_colors)
  } else if (ht_color_family == "Wes Anderson") {
    cols <- wes_palette(selected_colors, 3, type = "continuous")
  } else if (ht_color_family == "RColorBrewer") {
    cols <- brewer.pal(3, selected_colors)
  }

  colorRampPalette(cols)
}

###################################################################################################
# This code needs to be run when new colors are added to create the color's PNGs and some JS ####
###################################################################################################

# TODO: now, having solved this differently, I'm almost sure that I don't have to create PNGs first ...

# # Extract four colors per palette
# colors_for_color_picker <- function(name) {
#   if (
#     name %in% c("viridis", "magma", "inferno", "plasma", "cividis", "turbo")
#   ) {
#     viridis::viridis(4, option = name)
#   } else if (name %in% names(wes_palettes)) {
#     unclass(wes_palette(name, 4))
#   } else if (name %in% rownames(brewer.pal.info)) {
#     RColorBrewer::brewer.pal(4, name)
#   } else if (name == "App colors") {
#     get_theme_colors()
#   }
# }

# TODO: adjusted the name of the "App colors.png" manually to "App_colors.png"
# TODO: this maintains portability and should be included here!
# # Named list with 4 colors values per palette
# palette_lookup <- setNames(
#   lapply(color_choices_flat, colors_for_color_picker),
#   color_choices_flat
# )

# # Create the PNGs for each color palette
# for (i in seq_along(palette_lookup)) {
#   color_plot <- ggplot(
#     data = data.frame(
#       x = seq(1, 4),
#       y = 1,
#       fill = palette_lookup[[i]]
#     ),
#     aes(x = x, y = y, fill = fill)
#   ) +
#     geom_tile() +
#     scale_fill_manual(values = palette_lookup[[i]]) +
#     theme_void() +
#     theme(legend.position = "none")

#   ggsave(
#     filename = paste0("www/color_palettes", names(palette_lookup[i]), ".png"),
#     plot = color_plot,
#     dpi = 100
#   )
# }

# # Define images size for selected and choice state
# image_sizes_choice_selected <- c("Selected" = "18", "Choice" = "30")

# for (name in names(image_sizes_choice_selected)) {
#   width <- image_sizes_choice_selected[[name]]

#   # Create the HTML strings for each palette
#   color_palettes_images <- setNames(
#     sapply(color_choices_flat, function(pal) {
#       html <- sprintf(
#         "<img src='color_palettes/%s.png' width=%spx style='vertical-align:middle'><div class='jhr'>%s</div>",
#         pal,
#         width,
#         pal
#       )
#       gsub("'", "\\'", html)
#     }),
#     color_choices_flat
#   )

#   # Convert to JS and write to file
#   sprintf(
#     "window.colorPaletteChoicesIcons%s = { %s };",
#     name,
#     paste0(
#       sprintf(
#         "'%s':\"%s\"",
#         names(color_palettes_images),
#         color_palettes_images
#       ),
#       collapse = ","
#     )
#   ) %>%
#     writeLines(con = paste0("www/colorPaletteChoicesIcons", name, ".js"))
# }
