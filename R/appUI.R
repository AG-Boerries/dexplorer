#' @title DExploreR App UI
#'
#' @description
#' This function defines the user interface of the DExploreR Shiny app and it is called from \code{\link{runDExploreR}()}.
#'
#' @param config The list of configs that is created in \code{\link{runDExploreR}()}.
#'
app_ui <- function(config) {
  fluidPage(
    style = "margin-top: 15px;",
    ###################################################################################################
    # General Things #####
    ###################################################################################################

    # Necessary to use shinyjs functions
    useShinyjs(),

    # Necessary to use waiter functions
    useWaiter(),

    # This controls the overlay, when dropdown menus are opened
    div(id = "app-overlay"),

    # Load CSS styles
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "www/styles.css"
      )
    ),

    # Include HTML components
    includeHTML(
      system.file(
        "dexplorer_assets/customModal.html",
        package = "dexplorer"
      )
    ),

    # Add custom JS  for some tasks
    tags$script(src = "www/mainTabsHideShow.js"),
    tags$script(src = "www/dataTableAddOns.js"),
    tags$script(src = "www/customTooltip.js"),
    tags$script(src = "www/customModal.js"),
    tags$script(src = "www/colorPaletteChoicesIconsChoice.js"),
    tags$script(src = "www/colorPaletteChoicesIconsSelected.js"),
    tags$script(src = "www/colorPaletteRenderer.js"),

    # Add favicon
    tags$head(tags$link(rel = "shortcut icon", href = "www/favicon.ico")),

    ###################################################################################################
    # Start App #####
    ###################################################################################################

    # App navigation bar (tabs)
    navbarPage(
      title = div(
        class = "app-header",
        div(
          class = "header-left hover-group",
          img(
            src = "www/DExploreRLogo.png",
            class = "header-logo-rna"
          ),
          div(
            "DExploreR",
            class = "navpage-title"
          )
        ),
        div(
          a(
            href = "https://www.uniklinik-freiburg.de/de.html",
            target = "_blank",
            img(
              src = "www/UKFLogo_bw.png",
              class = "header-logo"
            )
          ),
          a(
            href = "https://uni-freiburg.de",
            target = "_blank",
            img(
              src = "www/UFRLogo_bw.png",
              class = "header-logo"
            )
          )
        ),
        div(
          "DExploreR v0.8.1.0",
          class = "app-version-fixed"
        )
      ),
      id = "navigation_bar",
      windowTitle = "DExploreR",

      ###################################################################################################
      # Chapter: Start Page #####
      ###################################################################################################

      if (config$mode == "standard") {
        tabPanel(
          "Home",
          div(
            img(src = "assets/logo.png", style = "margin-top: 200px;"),
            style = "text-align: center;"
          ),
          uiOutput("upload_rds_ui")
        )
      },

      ###################################################################################################
      # Chapter: Data Sets #####
      ###################################################################################################

      if (config$mode == "standard") {
        tabPanel(
          "Data sets",
          fluidPage(
            div(
              titlePanel(title = "Data sets", windowTitle = "Data sets"),
              h4("Choose and load a data set by clicking on it"),
              class = "tab-header"
            )
          ),
          DTOutput("data_sets_table"),
        )
      },

      ###################################################################################################
      # Chapter: Raw data ####
      ###################################################################################################

      tabPanel(
        "Raw data",
        tabHeaders(title = "Raw data", text_id = "raw_data_details"),
        tabsetPanel(
          id = "raw_data_tabs",
          ###################################################################################################
          # Subchapter: Quality control ####
          ###################################################################################################

          tabPanel(
            "Quality control",
            tabContentUI("raw_counts_content")
          ),

          ###################################################################################################
          # Subchapter: Dimensionality reduction ####
          ###################################################################################################

          tabPanel(
            "Dimensionality reduction (PCA)",
            makeSubTabContent(
              id = "pca",
              top_left_wide = div(
                plotlyOutput(
                  "scree_plot",
                  height = "180px",
                  width = "95%"
                ),
                class = "panel_plot_box"
              ),
              further_controls = div(
                # Select PC for x
                virtualSelectInput(
                  inputId = "select_PC_x",
                  label = "Select PC for x-axis:",
                  choices = c(),
                ),
                # Select PC for y
                virtualSelectInput(
                  inputId = "select_PC_y",
                  label = "Select PC for y-axis:",
                  choices = c(),
                ),
                virtualSelectInput(
                  inputId = "pca_grouping",
                  label = "Overlay group variable as",
                  choices = c("Ellipse", "Convex hull", "Don't overlay"),
                  selected = "Convex hull"
                )
              ),
              main_content = plotlyOutput(
                "pca_plot",
                height = "500px",
                width = "98%"
              ),
              # Samples cannot be excluded in the PCA
              # Sample exclusion requires recalculation, which is not performed in the app
              remove_sample_selection = TRUE
            ),
          ),

          ###################################################################################################
          # Subchapter: Gene expression ####
          #####################################################################################################

          tabPanel(
            "Gene expression heatmap",
            makeSubTabContent(
              id = "heatmap",
              # Allows user to upload gene list for heatmap
              top_left_wide = div(
                column(
                  width = 5,
                  div(
                    uiOutput("user_gene_list_ui"),
                    uiOutput("go_to_volcano"),
                    style = "align-items: flex-start; display: flex; flex-direction: column;"
                  )
                ),
                column(
                  width = 5,
                  uiOutput("tab_multi_genes")
                ),
                column(
                  width = 2,
                  uiOutput("genes_not_found")
                )
              ),
              additional_button_right = actionButton(
                "clear_user_genes_button_heatmap",
                label = "Clear uploaded gene list",
                class = "custom-button"
              ),
              # Further controls are required for the heatmap
              further_controls = div(
                virtualSelectInput(
                  inputId = "color_select_heatmap_groups",
                  label = "Select color palette for groups:",
                  # The color choices are defined in `controls_colors.R`
                  choices = color_choices,
                  selected = "inferno",
                  search = TRUE,
                  showSelectedOptionsFirst = TRUE
                ),
                virtualSelectInput(
                  inputId = "heatmap_dendrogram",
                  label = "Show dendrogram for:",
                  choices = c("Samples", "Genes", "Samples and genes", "None"),
                  selected = "Samples and genes"
                ),
                div(
                  HTML(
                    "<span style='display:block; margin-bottom:5px; font-size:14px; font-weight: 700;'>Select genes by:</span>"
                  ),
                  switchInput(
                    inputId = "median_or_variance_heatmap",
                    onLabel = "Variance",
                    offLabel = "Median",
                    value = TRUE
                  ),
                  sliderInput(
                    inputId = "subset_size_select_heatmap",
                    label = "Number of genes:",
                    min = 0,
                    max = 200,
                    step = 1,
                    value = 20,
                    ticks = FALSE
                  ),
                  class = "selection_box"
                ),
                # Switch between gene ID and symbols
                virtualSelectInput(
                  inputId = "switch_id_symbols_heatmap",
                  label = "Display IDs or symbols?",
                  choices = c(
                    "Ensembl ID",
                    "Entrez ID",
                    "Gene symbol",
                    "Gene name"
                  ),
                  selected = "Gene symbol"
                ),
                virtualSelectInput(
                  inputId = "gene_select_heatmap",
                  label = "Include genes of interest:",
                  choices = c(),
                  multiple = TRUE,
                  search = TRUE,
                  showSelectedOptionsFirst = TRUE,
                  disableSelectAll = TRUE
                )
              ),
              main_content = uiOutput("heatmap_ui")
            )
          )
        )
      ),

      ###############################################################################################
      # Chapter: DGEA #####
      ###############################################################################################

      tabPanel(
        "DGEA",
        tabHeaders(
          title = "Differential gene expression analysis",
          text_id = "dgea_details"
        ),
        tabsetPanel(
          id = "dgea_tabs",
          ###################################################################################################
          # Subchapter: Top-scoring genes ####
          ###################################################################################################

          tabPanel(
            "Top-scoring genes",
            makeSubTabContent(
              id = "top_genes",
              top_left_wide = div(
                h4("Top-scoring differentially expressed genes"),
                h5(
                  "Identify the most differentially expressed genes for each contrast"
                )
              ),
              remove_sample_selection = TRUE,
              further_controls = div(
                virtualSelectInput(
                  inputId = "top_genes_contrast_select",
                  label = "Select contrast(s):",
                  choices = c(),
                  multiple = TRUE,
                  search = TRUE
                ),
                # The labels of the `switchInput()`s are created manually to adjust the style of the other labels
                HTML(
                  "<span style='display:block; margin-bottom:5px; font-size:14px; font-weight: 700;'>Toggle x-axis (log2FC or p-value):</span>"
                ),
                switchInput(
                  inputId = "top_genes_fc_or_pvalue",
                  onLabel = "log2FC",
                  offLabel = "p\u2011value",
                  value = TRUE
                ),
                # The labels of the `switchInput()`s are created manually to adjust the style of the other labels
                HTML(
                  "<span style='display:block; margin-bottom:5px; font-size:14px; font-weight: 700;'>Toggle up- or downregulated genes:</span>"
                ),
                switchInput(
                  inputId = "top_genes_up_or_down",
                  onLabel = "Up",
                  offLabel = "Down",
                  value = TRUE
                ),
                sliderInput(
                  inputId = "top_genes_number_select",
                  label = "Select number of genes:",
                  min = 4,
                  max = 50,
                  value = 10,
                  step = 1,
                  ticks = FALSE
                ),
              ),
              main_content = plotlyOutput(
                "top_genes",
                height = "auto",
                width = "98%"
              )
            ),
          ),

          ###################################################################################################
          # Subchapter: Volcano plots ####
          ###################################################################################################

          tabPanel(
            "Volcano plot",
            makeSubTabContent(
              id = "volcano",
              top_left_wide = div(
                h4("Volcano plot"),
                uiOutput("gene_list_to_volcano"),
                uiOutput("gene_list_upload")
              ),
              additional_button_right = actionButton(
                "clear_user_genes_button_volcano",
                label = "Clear uploaded gene list",
                class = "custom-button"
              ),
              remove_sample_selection = TRUE,
              further_controls = div(
                virtualSelectInput(
                  inputId = "volcano_contrast_select",
                  label = "Select contrast:",
                  choices = c(),
                  search = TRUE,
                  multiple = TRUE
                ),
                sliderTextInput(
                  inputId = "volcano_p_threshold",
                  label = "Set p-value threshold:",
                  choices = c(
                    "0.001",
                    "0.0025",
                    "0.005",
                    "0.01",
                    "0.025",
                    "0.05"
                  ),
                  selected = "0.05",
                  hide_min_max = TRUE
                ),
                sliderInput(
                  inputId = "volcano_l2fc_threshold",
                  label = "Set log2FC threshold:",
                  min = 1,
                  max = 5,
                  value = 1,
                  step = 0.25,
                  ticks = FALSE
                ),
                virtualSelectInput(
                  inputId = "gene_select_volcano",
                  label = "Highlight genes of interest:",
                  choices = c(),
                  multiple = TRUE,
                  search = TRUE,
                  showSelectedOptionsFirst = TRUE,
                  disableSelectAll = TRUE
                ),
              ),
              main_content = plotlyOutput(
                "volcano_plot",
                height = "auto",
                width = "98%"
              )
            ),
          ),

          ###################################################################################################
          # Subchapter: Contrast intersection ####
          ###################################################################################################

          tabPanel(
            "Contrast intersection",
            makeSubTabContent(
              id = "contrast_intersection",
              top_left_wide = div(
                h4("Contrast intersection using Jaccard index"),
                h5(
                  "Identify genes that are commonly differentially expressed across multiple contrasts"
                ),
                div(
                  HTML(
                    "<b>Note:</b> Click on a colored dot to get information on which genes are commonly differentially expressed in this contrast comparison."
                  ),
                  style = "color: var(--theme-color-green);"
                )
              ),
              remove_sample_selection = TRUE,
              further_controls = div(
                sliderTextInput(
                  inputId = "contrast_intersection_p_threshold",
                  label = "Set p-value threshold:",
                  choices = c(
                    "0.001",
                    "0.0025",
                    "0.005",
                    "0.01",
                    "0.025",
                    "0.05"
                  ),
                  selected = "0.05",
                  hide_min_max = TRUE
                ),
                sliderInput(
                  inputId = "contrast_intersection_l2fc_threshold",
                  label = "Set log2FC threshold:",
                  min = 1,
                  max = 5,
                  value = 2,
                  step = 0.25,
                  ticks = FALSE
                )
              ),
              main_content = plotlyOutput(
                "jaccard_dgea",
                height = "1000px",
                width = "98%"
              )
            ),
          ),
        )
      ),

      ###############################################################################################
      # Chapter: GSEA #####
      ###############################################################################################

      tabPanel(
        "GSEA",
        tabHeaders(
          title = "Gene set enrichment analysis",
          text_id = "gsea_details"
        ),
        tabsetPanel(
          ###################################################################################################
          # Subchapter: Top-scoring gene sets ####
          ###################################################################################################

          tabPanel(
            "Top-scoring gene sets",
            makeSubTabContent(
              id = "top_gene_sets",
              top_left_wide = div(
                h4("Top-scoring enriched gene sets"),
                div(
                  HTML(
                    "<b>Note:</b> Click on a colored dot to get see a volcano plot or a heatmap of this gene set."
                  ),
                  style = "color: var(--theme-color-green);"
                )
              ),
              remove_sample_selection = TRUE,
              further_controls = div(
                virtualSelectInput(
                  inputId = "gene_sets_contrast_select",
                  label = "Select contrast:",
                  choices = c(),
                  search = TRUE,
                  multiple = TRUE
                ),
                virtualSelectInput(
                  inputId = "select_gene_sets",
                  label = "Select gene sets of interest:",
                  choices = c(),
                  search = TRUE,
                  multiple = TRUE,
                  disableSelectAll = TRUE,
                  showSelectedOptionsFirst = TRUE,
                  noOfDisplayValues = 0,
                  maxValues = 60,
                  popupDropboxBreakpoint = "3000px"
                )
              ),
              main_content = plotlyOutput(
                "top_gene_sets",
                height = "auto",
                width = "98%"
              )
            ),
          ),

          ###################################################################################################
          # Subchapter: Contrast intersection by gene sets ####
          ###################################################################################################

          tabPanel(
            "Contrast intersection",
            makeSubTabContent(
              id = "contrast_intersection_sets",
              top_left_wide = div(
                h4("Contrast intersection using Jaccard index"),
                h5(
                  "Identify gene sets that are commonly differentially expressed across multiple contrasts"
                )
              ),
              remove_sample_selection = TRUE,
              main_content = plotlyOutput(
                "jaccard_gsea",
                height = "1000px",
                width = "98%"
              )
            )
          )
        )
      )
    )
  )
}
