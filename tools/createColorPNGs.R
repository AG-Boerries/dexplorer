#########################################################################################################
# This script creates the PNG images for the color palette selection and is only used during development.
#########################################################################################################

# Extract four colors per palette
colors_for_color_picker <- function(name) {
  if (
    name %in% c("viridis", "magma", "inferno", "plasma", "cividis", "turbo")
  ) {
    viridis::viridis(4, option = name)
  } else if (name %in% names(wes_palettes)) {
    unclass(wes_palette(name, 4))
  } else if (name %in% rownames(brewer.pal.info)) {
    RColorBrewer::brewer.pal(4, name)
  } else if (name == "App colors") {
    get_theme_colors()
  }
}

# Named list with 4 colors values per palette
palette_lookup <- setNames(
  lapply(color_choices_flat, colors_for_color_picker),
  # Remove the space in the color palette names
  sub(" ", "_", color_choices_flat)
)

# Create the PNGs for each color palette
for (i in seq_along(palette_lookup)) {
  color_plot <- ggplot(
    data = data.frame(
      x = seq(1, 4),
      y = 1,
      fill = palette_lookup[[i]]
    ),
    aes(x = x, y = y, fill = fill)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values = palette_lookup[[i]]) +
    theme_void() +
    theme(legend.position = "none")

  ggplot2::ggsave(
    filename = paste0(
      "inst/dexplorer_assets/color_palettes/",
      names(palette_lookup[i]),
      ".png"
    ),
    plot = color_plot,
    dpi = 100
  )
}

# Define images size for selected and choice state
image_sizes_choice_selected <- c("Selected" = "18", "Choice" = "30")

for (name in names(image_sizes_choice_selected)) {
  width <- image_sizes_choice_selected[[name]]

  # Create the HTML strings for each palette
  color_palettes_images <- setNames(
    sapply(names(palette_lookup), function(pal) {
      html <- sprintf(
        "<img src='color_palettes/%s.png' width=%spx style='vertical-align:middle'><div class='color-palette-choice'>%s</div>",
        pal,
        width,
        pal
      )
      gsub("'", "\\'", html)
    }),
    names(palette_lookup)
  )

  # Convert to JS and write to file
  sprintf(
    "window.colorPaletteChoicesIcons%s = { %s };",
    name,
    paste0(
      sprintf(
        "'%s':\"%s\"",
        names(color_palettes_images),
        color_palettes_images
      ),
      collapse = ","
    )
  ) %>%
    writeLines(
      con = paste0(
        "inst/dexplorer_assets/colorPaletteChoicesIcons",
        name,
        ".js"
      )
    )
}
