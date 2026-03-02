#' @title The Server for DExploreR
#'
#' @description
#' This function creates the server for DExplorerR. It relies on many of the functions with the prefix *mod_*.
#'
#' @param input Default parameter for shiny app servers.
#' @param output Default parameter for shiny app servers.
#' @param session Default parameter for shiny app servers.
#' @param config A list of configuration parameters, such as the data directory.
#'
app_server <- function(input, output, session, config) {
  # Define variables locally for R CMD check
  . <- Symbol <- Contrst <- Genes <- Seta <- Setb <- Direction <- Contrast <- GSCollectionName <- GSName <- GeneID <- NULL

  ###################################################################################################
  # General Things at Start up #####
  ###################################################################################################

  # Immediately show the waiter, when user provided a data set directly, e.g. no data set selection necessary
  if (config$mode != "standard") {
    waiter_show(
      html = tagList(
        h4("Loading data and plots ... "),
        img(
          src = "assets/logo.png",
          class = "pulse-logo",
          style = "margin-top: 100px;"
        )
      ),
      color = "white"
    )
  }

  # Reactive values to track plotting status
  plots_ready <- reactiveValues(
    bar_plot = FALSE,
    scree = FALSE,
    pca = FALSE,
    heatmap = FALSE,
    top_genes = FALSE,
    volcano = FALSE,
    jaccard_dgea = FALSE,
    top_gene_sets = FALSE,
    jaccard_gsea = FALSE
  )

  # Tabs to disable/enable during data set loading
  tabs <- c("Raw data", "DGEA", "GSEA")

  # Disable tabs at start up only when user has to select a data set
  observe({
    req(
      length(input$data_sets_table_rows_selected) == 0 &&
        config$mode == "standard"
    )
    runjs(sprintf(
      "disableTabs(%s)",
      toJSON(tabs, auto_unbox = TRUE)
    ))
  })

  # Show loading screen when a new data set is selected
  observeEvent(
    input$data_sets_table_rows_selected,
    {
      req(length(input$data_sets_table_rows_selected) == 1)

      # Invalidate dataset to avoid clash of sample names between new and old data set
      lapply(names(plots_ready), function(nm) {
        plots_ready[[nm]] <- FALSE
      })

      # Show waiter as long as data ist loading
      waiter_show(
        html = tagList(
          h4("Loading data and plots ... "),
          img(
            src = "assets/logo.png",
            class = "pulse-logo",
            style = "margin-top: 100px;"
          )
        ),
        color = "white"
      )
    },
    ignoreInit = FALSE
  )

  # Check if all plots are ready, after data set loading
  # Then, hide waiter and enable tabs
  observe(
    {
      allPlotsReady <- all(unlist(reactiveValuesToList(plots_ready)))

      if (allPlotsReady) {
        # Activate tabs
        runjs(sprintf(
          "enableTabs(%s)",
          toJSON(tabs, auto_unbox = TRUE)
        ))

        # Switch to quality control in raw data tab
        updateTabsetPanel(
          inputId = "navigation_bar",
          selected = "Raw data"
        )
        updateTabsetPanel(
          inputId = "raw_data_tabs",
          selected = "Quality control"
        )

        # Simulate longer loading time and finally hide waiter
        Sys.sleep(1)
        waiter_hide()
      }
    }
  )

  # Create text outputs for each tab with unique IDs
  iwalk(
    # The names are the IDs for the output
    # The values are the headings of `dataSetsTable`
    list(
      raw_data_details = "Sequencing Details",
      dgea_details = "DGEA Details",
      gsea_details = "GSEA Details"
    ),
    function(column, output_id) {
      output[[output_id]] <- renderText({
        if (config$mode == "internal") {
          dTypes$dataSetsTable[1, column]
        } else {
          dTypes$dataSetsTable[input$data_sets_table_rows_selected, column]
        }
        # When `config$mode == "interactive"`, there is no `dataSetsTable`, thus nothing is returned and rendered
      })
    }
  )

  # Get the author information
  authors <- reactive({
    if (config$mode != "interactive") {
      dTypes$dataSetsTable[
        input$data_sets_table_rows_selected,
        "Authors"
      ] %>%
        gsub(" ", "_", .)
    } else {
      ""
    }
  })

  # Available samples for selection in the plots
  available_samples_by_groups <- reactive({
    req(data_set_loaded())

    # Extract samples by groups
    df <- data_set_loaded()[["SamplesGroups"]]
    # Turn this into a list split by groups
    # This can be used directly in `virtualSelectInput()`
    # The first column contains the user-defined sample names from meta_data_lab
    # The second column contains the user-defined groups
    # The third column contains the `RunID` column from `RawCounts`
    # Further information can be obtained from `dexplorer::prepareDfs()`
    split(df$SampleNameUser, df$Group)
  })

  # Update all sample selection UI elements, when the samples are available or change because new data set is loaded
  lapply(
    list(
      "raw_counts_content-sample_select",
      "sample_select_heatmap"
    ),
    function(id) {
      observeEvent(available_samples_by_groups(), {
        updateVirtualSelect(
          id,
          choices = available_samples_by_groups(),
          # Selected samples have to be an unnamed vector
          selected = unname(unlist(available_samples_by_groups()))
        )
      })
    }
  )

  # Reactive function that can be called to render the file upload of a user-defined gene list
  render_gene_list_file_upload <- function() {
    output$user_gene_list_ui <- renderUI({
      div(
        fileInput(
          inputId = "user_gene_list_heatmap",
          label = "Upload list of genes of interest:",
          accept = c(".txt", ".csv"),
          width = "250px"
        ),
        style = "display: flex; align-items: center; justify-content: left;"
      )
    })

    # When rendering the file upload, hide the button to clear the uploaded gene list
    hide("clear_user_genes_button_heatmap")
    hide("clear_user_genes_button_volcano")
  }

  # Auto-render the file upload upon start up of the app
  render_gene_list_file_upload()

  ###################################################################################################
  # Chapter: Data Sets ####
  ###################################################################################################

  # Check the provided data and read files accordingly
  dTypes <- dataTypeSelector(config = config)

  # When `config$mode == "standard"`, a data set selection is necessary
  # All available data sets are shown to the user
  if (!is.null(dTypes$dataSetsTable) && is.null(dTypes$dataSet)) {
    # Render the table with the data sets
    output$data_sets_table <- renderDT(
      dTypes$dataSetsTable %>%
        select(
          "Cell line or tissue",
          "Study target",
          "Authors",
          "Date",
          "Details"
        ),
      rownames = FALSE,
      # Allow to select only a single row
      selection = "single",
      escape = FALSE,
      options = list(
        # Hide the `Details` column (index 4 in 0-based JS)
        columnDefs = list(list(visible = FALSE, targets = 4)),
        # Display `Details` column as tooltip on hovering over row
        rowCallback = JS("dataSetDetails"),
        drawCallback = JS("dtTooltip")
      )
    )

    # Load the selected data set
    data_set_loaded <- reactive({
      req(length(input$data_sets_table_rows_selected) == 1)
      # Prevents errors like `samples not found`, when switching between data sets
      freezeReactiveValue(input, "sample_select_heatmap")
      # Prevents volcano plot from crashing, when switching between data sets
      freezeReactiveValue(input, "volcano_contrast_select")
      freezeReactiveValue(input, "gene_sets_contrast_select")
      # Using the file path from the meta data table, load the corresponding `.rds`
      file_path <- paste0(
        config$data,
        "/",
        dTypes$dataSetsTable[
          input$data_sets_table_rows_selected,
          "data_path"
        ]
      )

      d <- readRDS(file_path)
      message("\n\n****** Loading data set from: ", file_path, " ******\n\n")
      d
    })
  } else if (!is.null(dTypes$dataSet)) {
    # This handles two cases, either the user provided data set as a list or the internal data set
    # Thus, create a corresponding message for loading
    loading_msg <- if (config$mode == "internal") {
      "\n\n****** Loading internal data set ... ******\n\n"
    } else {
      "\n\n****** Loading data set provided by you ... ******\n\n"
    }

    data_set_loaded <- reactive({
      # These values need to be frozen to prevent reactivity issues
      freezeReactiveValue(input, "sample_select_heatmap")
      freezeReactiveValue(input, "volcano_contrast_select")
      freezeReactiveValue(input, "gene_sets_contrast_select")

      d <- dTypes$dataSet
      message(loading_msg)
      d
    })
  }

  ###################################################################################################
  # Chapter: Raw data ####
  ###################################################################################################

  ###################################################################################################
  # Subchapter: Raw counts ####
  ###################################################################################################

  tabContentServer(
    "raw_counts_content",
    data = data_set_loaded,
    plot_status = plots_ready,
    authors = authors()
  )

  ###################################################################################################
  # Subchapter: Dimensionality reduction ####
  ###################################################################################################

  # Update PCA component selection inputs, when data set is loaded
  lapply(
    list(
      list(id = "select_PC_x", selected = "PC1"),
      list(id = "select_PC_y", selected = "PC2")
    ),
    function(pc) {
      observeEvent(data_set_loaded(), {
        updateVirtualSelect(
          pc$id,
          choices = data_set_loaded()[["VarianceExplained"]]$PC,
          selected = pc$selected
        )
      })
    }
  )

  # Reactive for scree plot
  scree_plot <- reactive({
    req(data_set_loaded())
    # The plot requries these inputs and they are updated when when data is loaded
    req(input$select_PC_x, input$select_PC_y)

    createScreePlot(
      explained_var = data_set_loaded()[["VarianceExplained"]],
      pc_x = input$select_PC_x,
      pc_y = input$select_PC_y,
      selected_palette = input$color_select_pca
    )
  })

  # Reactive for PCA scatter plot
  pca_plot <- reactive({
    req(data_set_loaded())
    # The plot requries these inputs and they are updated when when data is loaded
    req(input$select_PC_x, input$select_PC_y)

    createPCAPlot(
      df_pca = data_set_loaded()[["PCA"]],
      explained_var = data_set_loaded()[["VarianceExplained"]],
      pc_x = input$select_PC_x,
      pc_y = input$select_PC_y,
      selected_palette = input$color_select_pca,
      group_overlay = input$pca_grouping
    )
  })

  # Check if the plots are create and set their reactive values accordingly
  observe({
    req(scree_plot())
    req(pca_plot())

    plots_ready$scree <- TRUE
    plots_ready$pca <- TRUE
    message(
      "[Raw data][Dimensionality reduction] PCA plots are ready."
    )
  })

  # Send the created plots to the UI
  output$scree_plot <- renderPlotly({
    scree_plot()
  })
  output$pca_plot <- renderPlotly({
    pca_plot()
  })

  # Download handler for PCA data
  output$download_data_pca <- dataDownload(
    name = "PCA_data",
    data = data_set_loaded()[["PCA"]],
    authors = authors()
  )

  ###################################################################################################
  # Subchapter: Gene expression ####
  ###################################################################################################

  # Update the selectable genes
  observeEvent(data_set_loaded(), {
    updateVirtualSelect(
      "gene_select_heatmap",
      choices = unique(data_set_loaded()[["NormalizedCounts"]]$Symbol)
    )
  })

  # Create the data frame for the heatmap as well as the download
  df_heatmap <- reactive({
    req(data_set_loaded())
    formatForHeatmap(
      df = data_set_loaded()[["NormalizedCounts"]],
      selected_samples = input$sample_select_heatmap,
      selected_subset_size = input$subset_size_select_heatmap,
      selected_genes = input$gene_select_heatmap,
      gene_selection_by = input$median_or_variance_heatmap
    )
  })

  # Calculate heatmap and domain heights based on selected inputs
  heatmap_heights <- reactive({
    heatmapHeights(
      n_genes = nrow(df_heatmap()),
      dendro_type = input$heatmap_dendrogram
    )
  })

  # Create the heatmap plot
  heatmap_plot <- reactive({
    createGeneExpressionHeatmap(
      df = df_heatmap(),
      id_or_sym = input$switch_id_symbols_heatmap,
      samples_groups = data_set_loaded()[["SamplesGroups"]],
      heatmap_colors = input$color_select_heatmap,
      group_colors = input$color_select_heatmap_groups,
      dendrogram_type = input$heatmap_dendrogram,
      heatmap_heights = heatmap_heights()
    )
  })

  # Print message when heatmap plot is ready
  observe({
    req(heatmap_plot())
    plots_ready$heatmap <- TRUE
    message(
      "[Raw data][Gene expression heatmap] Plot is ready."
    )
  })

  # Send the heatmap to the UI
  output$heatmap_gene_expression <- renderPlotly({
    heatmap_plot()
  })

  # Calculate total heatmap height based on the number of samples
  output$heatmap_ui <- renderUI({
    plotlyOutput(
      "heatmap_gene_expression",
      height = heatmap_heights()$total_height
    )
  })

  # Get the data frame for the .csv download
  output$download_data_heatmap <- dataDownload(
    name = "Heatmap_data",
    data = df_heatmap(),
    authors = authors()
  )

  # The default state for the button in the volcano plots
  observeEvent(data_set_loaded(), {
    show("gene_list_upload")
    hide("gene_list_to_volcano")
  })

  # Set userGenes to NULL at the start
  userGenes <- reactiveVal(NULL)

  # Logic for uploading a user-defined gene list
  observeEvent(input$user_gene_list_heatmap, {
    req(!is.null(input$user_gene_list_heatmap))

    # Upon upload of a gene list, try to extract the genes
    genes <- tryCatch(
      {
        processUserGenes(
          available_genes = data_set_loaded()[["GeneInfoAliases"]],
          user_genes = input$user_gene_list_heatmap
        )
      },
      error = function(e) {
        # Prepare an example list of genes
        example_genes_list <- data.frame(
          "genes" = c("Gene1", "Gene2", "Gene3", "...", "")
        )
        output$example_genes <- renderTable(
          example_genes_list,
          colnames = FALSE
        )

        # Show a modal, when the uploaded format is wrong
        showModal(
          modalDialog(
            title = "Wrong format of uploaded genes of interest.",
            easyClose = TRUE,
            footer = NULL,
            div(
              "Please provide a simple text file (.csv or .txt format) with one gene name per line, like so:",
              div(
                tableOutput("example_genes"),
                style = "display: flex; justify-content: center; margin-top: 20px"
              )
            )
          )
        )
        # Re-render the file upload to reset it after wrong file format
        render_gene_list_file_upload()
      }
    )

    if (!is.null(genes)) {
      # If uploaded file has the right format, then set this as the reactive variable
      userGenes(genes)

      # Update the selected genes in the heatmap gene selection input, while keeping what a user might have selected already
      updateVirtualSelect(
        inputId = "gene_select_heatmap",
        selected = c(userGenes()$genesFound, input$gene_select_heatmap)
      )

      # Remove the top 20 genes by variance, which are displayed by default
      updateSliderInput(
        inputId = "subset_size_select_heatmap",
        value = 0
      )

      # Create a text output for the genes that were not found
      output$genes_not_found <- renderUI({
        if (length(userGenes()$genesNotFound) == 0) {
          tags$b("All genes found.")
        } else {
          tagList(
            tags$b("Genes not found:"),
            tags$ul(
              lapply(
                userGenes()$genesNotFound,
                function(g) tags$li(g)
              )
            )
          )
        }
      })

      # Create a table for the genes with multiple mappings
      output$genes_multiple <- renderTable(
        userGenes()$genesFoundMulti,
        width = "100%"
      )

      output$tab_multi_genes <- renderUI({
        if (nrow(userGenes()$genesFoundMulti) == 0) {
          return()
        } else {
          div(
            tags$b("Genes with multiple symbols:"),
            div(
              tableOutput("genes_multiple"),
              style = "border-color: var(--alt-border-color); border-style: solid; border-width: 2px; border-radius: var(--box-border-radius); margin: 0px; padding: 0px; overflow-x: auto; overflow-y: auto; height: 120px;"
            ),
            tags$i(
              "You can unselect unwanted genes in the plot settings on the right.",
              style = "font-size: 10px;"
            )
          )
        }
      })

      output$go_to_volcano <- renderUI({
        actionButton(
          inputId = "check_genes_in_volcano",
          label = "Check your uploaded genes in the volcano plot!",
          class = "custom-button"
        )
      })

      # Change the buttons in the volcano tab
      hide("gene_list_upload")
      show("gene_list_to_volcano")

      # When list is uploaded, show button to clear the uploaded gene list
      show("clear_user_genes_button_heatmap")

      # Show some information about the uploaded genes
      show("tab_multi_genes")
      show("genes_not_found")

      # Display a button to inspect the genes in the volcano plot
      show("go_to_volcano")
    }
  })

  # Create a button to use the uploaded gene list in the volcano plot, if desired
  output$gene_list_to_volcano <- renderUI({
    div(
      h5(
        "You uploaded a list of genes for the gene expression heatmap, do you want to highlight these genes in the volcano plot(s), too?"
      ),
      actionButton(
        inputId = "use_gene_list_in_volcano",
        label = "Use uploaded list of genes to highlight",
        class = "custom-button"
      ),
      class = "highlight-genes-volcano"
    )
  })

  observeEvent(input$use_gene_list_in_volcano, {
    # When gene list is used in volcano, change the buttons in the volcano tab
    show("clear_user_genes_button_volcano")
    hide("gene_list_to_volcano")

    # Update the selected genes in the volcano plot, if there are genes uploaded
    updateVirtualSelect(
      "gene_select_volcano",
      selected = c(
        userGenes()$genesFound,
        input$gene_select_volcano
      )
    )
  })

  # Create a button leading to the upload of the gene list, which is in the heatmap tab
  output$gene_list_upload <- renderUI({
    div(
      h5(
        "No gene list uploaded. Upload a gene list to enable volcano plot highlighting."
      ),
      actionButton(
        inputId = "go_to_gene_upload",
        label = "Go to gene list upload",
        class = "custom-button"
      ),
      class = "highlight-genes-volcano"
    )
  })

  # Logic to go to the gene list upload in the heatmap tab
  observeEvent(input$go_to_gene_upload, {
    updateTabsetPanel(
      inputId = "navigation_bar",
      selected = "Raw data"
    )
    updateTabsetPanel(
      inputId = "raw_data_tabs",
      selected = "Gene expression heatmap"
    )
  })

  # Logic to go to the volcano plot, when gene list is uploaded
  observeEvent(input$check_genes_in_volcano, {
    updateTabsetPanel(
      inputId = "navigation_bar",
      selected = "DGEA"
    )
    updateTabsetPanel(
      inputId = "dgea_tabs",
      selected = "Volcano plot"
    )
    # When gene list is used in volcano, change the buttons in the volcano tab
    show("clear_user_genes_button_volcano")
    hide("gene_list_to_volcano")

    # Update the selected genes in the volcano plot, if there are genes uploaded
    updateVirtualSelect(
      "gene_select_volcano",
      selected = c(
        userGenes()$genesFound,
        input$gene_select_volcano
      )
    )
  })

  # Clear genes only from volcano plot
  observeEvent(input$clear_user_genes_button_volcano, {
    # Clear the selected genes in the volcano plot
    updateVirtualSelect(
      "gene_select_volcano",
      selected = character(0)
    )

    # Hide the button to transfer gene list to the volcano plot
    show("gene_list_to_volcano")
    # Hide the clear genes button
    hide("clear_user_genes_button_volcano")
  })

  # Clear genes from both, heatmap and volcano plot
  observeEvent(input$clear_user_genes_button_heatmap, {
    # Re-render the file upload for user-defined genes
    render_gene_list_file_upload()

    # Upon removing user gene list, show top 20 genes by variance again
    updateSliderInput(
      inputId = "subset_size_select_heatmap",
      value = 20
    )

    # Clear the selected genes in the heatmap gene selection input
    updateVirtualSelect(
      "gene_select_heatmap",
      selected = character(0)
    )

    # Clear the selected genes in the volcano plot
    updateVirtualSelect(
      "gene_select_volcano",
      selected = character(0)
    )

    # Remove information about the uploaded genes
    hide("tab_multi_genes")
    hide("genes_not_found")
    hide("go_to_volcano")

    # Hide the button to transfer gene list to the volcano plot
    hide("gene_list_to_volcano")
    # Show the button to the file upload
    show("gene_list_upload")

    # Set the reactive value back to initial state: NULL
    userGenes(NULL)
  })

  ###################################################################################################
  # Chapter: DGEA ####
  ###################################################################################################

  ###################################################################################################
  # Subchapter: Top-scoring genes ####
  ###################################################################################################

  observeEvent(data_set_loaded(), {
    updateVirtualSelect(
      "top_genes_contrast_select",
      choices = unique(data_set_loaded()[["DGEAnalysis"]]$Contrast),
      selected = unique(data_set_loaded()[["DGEAnalysis"]]$Contrast)
    )
  })

  # Prepare data frame for top-scoring genes plot and download
  df_top_genes_dgea <- reactive({
    req(data_set_loaded())
    formatTopDEGs(
      df = data_set_loaded()[["DGEAnalysis"]],
      selected_contrast = input$top_genes_contrast_select,
      selected_number_of_genes = input$top_genes_number_select,
      selected_direction = input$top_genes_up_or_down,
      fc_or_pvalue = input$top_genes_fc_or_pvalue
    )
  })

  # Create the top-scoring genes plot
  top_genes_plot <- reactive({
    req(df_top_genes_dgea())
    createTopDEGsPlot(
      df = df_top_genes_dgea(),
      selected_palette = input$color_select_top_genes,
      fc_or_pvalue = input$top_genes_fc_or_pvalue
    )
  })

  # Notify when the plot is ready
  observe({
    req(top_genes_plot())
    plots_ready$top_genes <- TRUE
    message(
      "[DGEA][Top-scoring genes] Plot is ready."
    )
  })

  # Send plot to the UI
  output$top_genes <- renderPlotly({
    top_genes_plot()
  })

  # Prepare data frame for download
  output$download_data_top_genes <- dataDownload(
    name = "Top_scoring_genes",
    data = df_top_genes_dgea() %>%
      # Remove the groups added by `tidytext::reorder_within()`
      mutate(Symbol = sub("__.*$", "", Symbol)),
    authors = authors()
  )

  ###################################################################################################
  # Subchapter: Volcano plots ####
  ###################################################################################################

  # Update selectable contrasts and genes for volcano plot after data upload
  observeEvent(data_set_loaded(), {
    updateVirtualSelect(
      "volcano_contrast_select",
      choices = unique(data_set_loaded()[["DGEAnalysis"]]$Contrast),
      selected = unique(data_set_loaded()[["DGEAnalysis"]]$Contrast)
    )

    updateVirtualSelect(
      "gene_select_volcano",
      choices = unique(data_set_loaded()[["DGEAnalysis"]]$Symbol)
    )
  })

  # Craate the volcano plot
  volcano_plot <- reactive({
    req(data_set_loaded())
    req(input$volcano_contrast_select)
    createVolcanoPlot(
      df = data_set_loaded()[["DGEAnalysis"]],
      selected_palette = input$color_select_volcano,
      p_threshold = input$volcano_p_threshold,
      l2fc_threshold = input$volcano_l2fc_threshold,
      selected_genes = input$gene_select_volcano,
      selected_contrast = input$volcano_contrast_select
    )
  })

  # Notify when the plot is ready
  observe({
    req(volcano_plot())
    plots_ready$volcano <- TRUE
    message(
      "[DGEA][Volcano plot] Plot is ready."
    )
  })

  # Send the plot to the UI
  output$volcano_plot <- renderPlotly({
    volcano_plot()
  })

  # Get the data for the .csv download
  output$download_data_volcano <- dataDownload(
    name = "Volcano_plot_data",
    data = data_set_loaded()[["DGEAnalysis"]] %>%
      filter(Contrast %in% input$volcano_contrast_select),
    authors = authors()
  )

  ###################################################################################################
  # Subchapter: Contrast intersection ####
  ###################################################################################################

  df_dgea_ci <- reactive({
    req(data_set_loaded())
    formatDGEAContrastIntersection(
      df = data_set_loaded()[["DGEAnalysis"]],
      p_threshold = input$contrast_intersection_p_threshold,
      l2fc_threshold = input$contrast_intersection_l2fc_threshold
    )
  })

  contrast_intersection_plot <- reactive({
    createDGEAContrastIntersectionPlot(
      df = df_dgea_ci(),
      selected_palette = input$color_select_contrast_intersection
    )
  })

  observe({
    req(contrast_intersection_plot())
    plots_ready$jaccard_dgea <- TRUE
    message(
      "[DGEA][Contrast intersection] Plot is ready."
    )
  })

  output$jaccard_dgea <- renderPlotly({
    contrast_intersection_plot()
  })

  output$download_data_contrast_intersection <- dataDownload(
    name = "Contrast_intersection_DGEA",
    # Remove the list column, which cannot be saved as .csv
    data = df_dgea_ci() %>% select(-Genes),
    authors = authors()
  )

  observeEvent(
    event_data(event = "plotly_click", source = "dgea_jaccard"),
    {
      # Extract the custom data from the clicked data point
      clicked_ji <- event_data(
        event = "plotly_click",
        source = "dgea_jaccard"
      )$customdata

      # The circle of the maxium JI is clickable but has no `customdata`
      # In this case, do nothing
      if (is.null(clicked_ji)) {
        return()
      }

      # Separate the the string by the defined delimiter in `R/plot_dgea_jaccard.R`
      clicked_ji <- str_split_1(clicked_ji, "\\|")

      # Filter the data frame for the clicked set and direction and pull the data frame containing the genes
      # Use isolate the prevent data table from requesting columns from previous modal
      df <- isolate({
        df_dgea_ci() %>%
          filter(
            Seta == clicked_ji[1],
            Setb == clicked_ji[2],
            Direction == clicked_ji[3]
          ) %>%
          pull(Genes) %>%
          as.data.frame()
      })

      # Repair the column names, remove the dots
      colnames(df) <- gsub("\\.", " ", colnames(df))

      # Get the data for the .csv download
      output$download_dgea_ji <- dataDownload(
        name = "Intersecting_genes",
        data = df,
        authors = authors()
      )

      venn_plot <- reactive({
        createVennDiagram(
          df = df,
          selected_palette = input$color_select_venn_modal
        )
      })

      # Create the Venn diagram
      output$venn_plot <- renderPlotly({
        venn_plot()
      })

      # Create the list of genes as a data table
      output$gene_table <- renderDT(
        df,
        rownames = FALSE,
        # Avoid server site processing, to prevent the warning in the UI (missing columns)
        # However, this can create warnings in the console when data is too big
        # Maybe this can also crash
        server = FALSE,
        selection = "single",
        escape = FALSE,
        options = list(
          # Hide the NCBIURL column (index 7 in 0-based JS)
          columnDefs = list(list(visible = FALSE, targets = 7)),
          # Go to NCBI's gene page, when row clicked
          rowCallback = JS("visitNCBI"),
          # Increase default number of rows
          pageLength = 100
        )
      )

      # After clicking and processing, show the modal with the corresponding data
      observe({
        req(df)
        showModal(modalDialog(
          title = HTML(paste0(
            "DEGs at the intersection of <i>",
            colnames(df)[1],
            "</i> and <i>",
            colnames(df)[2],
            "</i>"
          )),
          easyClose = TRUE,
          footer = NULL,
          class = "enlarged-modal",
          tagList(
            tags$i(paste0(
              "You are viewing ",
              ifelse(
                clicked_ji[3] == "both",
                "up and down regulated genes.",
                paste0(clicked_ji[3], "-regulated genes.")
              )
            )),
            tags$hr()
          ),
          tabsetPanel(
            tabPanel(
              "Venn diagram",
              div(
                virtualSelectInput(
                  inputId = "color_select_venn_modal",
                  label = "Select color palette:",
                  # The color choices are defined in `controls_colors.R`
                  choices = color_choices,
                  selected = "App colors",
                  search = TRUE,
                  showSelectedOptionsFirst = TRUE,
                  # Add custom renderers for the colors, which include images of the color scales
                  labelRenderer = "colorsWithIconChoice",
                  selectedLabelRenderer = "colorsWithIconSelected"
                ),
                actionButton(
                  inputId = "open_custom_download_modal_venn",
                  label = "Download plot",
                  class = "custom-button",
                  icon = icon("download"),
                  width = "155px"
                ),
                class = "tab-header-modal"
              ),
              hr(),
              plotlyOutput("venn_plot", height = "450px")
            ),
            tabPanel(
              "Gene list",
              div(
                downloadButton(
                  outputId = "download_dgea_ji",
                  label = "Download data",
                  class = "custom-button"
                ),
                class = "tab-header-modal"
              ),
              hr(),
              DTOutput("gene_table")
            )
          )
        ))
      })

      # Open the custom modal for downloads from modals
      # This needs special treatment as usually shiny only allows one modal to be open at a time
      observeEvent(input$open_custom_download_modal_venn, {
        output$custom_modal_ui_venn <- renderUI({
          downloadSettingsModal(id = "venn")
        })
        session$sendCustomMessage("show-custom-modal-venn", list())
      })

      # Download handler for plot downloads from the modal
      output$download_plot_venn_modal <- downloadHandler(
        filename = function() {
          # Create the name of the downloaded file similar to the data downloads
          paste0(
            "Venn_diagram_",
            authors(),
            "_",
            Sys.Date(),
            ".",
            input$plot_format_venn
          )
        },
        content = function(file) {
          # Use `save_image()` from plotly to save the plot in the desired format
          # This depends on python, thus, usage of `reticulate` and `kaleido`
          save_image(
            p = eval(parse(text = "venn_plot()")),
            file = file,
            width = input$plot_width_venn,
            height = input$plot_height_venn
          )
        }
      )
    }
  )

  ###################################################################################################
  # Chapter: GSEA ####
  ###################################################################################################

  ###################################################################################################
  # Subchapter: Top-scoring gene sets ####
  ###################################################################################################

  # Update control elements after data upload
  observeEvent(data_set_loaded(), {
    updateVirtualSelect(
      inputId = "gene_sets_contrast_select",
      choices = unique(data_set_loaded()[["GeneSets"]]$Contrast),
      selected = unique(data_set_loaded()[["GeneSets"]]$Contrast)
    )

    updateVirtualSelect(
      inputId = "select_gene_sets",
      # Create list of named vectors for grouped choices
      choices = data_set_loaded()[["GeneSetsGenes"]] %>%
        distinct(GSCollectionName, GSName) %>%
        group_by(GSCollectionName) %>%
        summarise(values = list(GSName), .groups = "drop") %>%
        deframe(),
      # By default select the hallmark gene sets
      selected = data_set_loaded()[["GeneSetsGenes"]] %>%
        filter(GSCollectionName == "Hallmark") %>%
        pull(GSName) %>%
        unique()
    )
  })

  # Create the top-scoring gene sets plot
  top_gene_sets_plot <- reactive({
    req(data_set_loaded())
    if (
      is.null(input$gene_sets_contrast_select) ||
        is.null(input$select_gene_sets) ||
        length(input$select_gene_sets) == 0
    ) {
      p <- empty_plot(
        "Select a contrast and at least one gene set.\nCave: Not all gene sets are enriched in all contrasts."
      )
    } else {
      p <- createTopGeneSetsPlot(
        df = data_set_loaded()[["GeneSets"]],
        df_genes = data_set_loaded()[["GeneSetsGenes"]],
        selected_palette = input$color_select_top_gene_sets,
        selected_contrast = input$gene_sets_contrast_select,
        selected_gene_sets = input$select_gene_sets
      )
    }
    p
  })

  # Notify when the plot is ready
  observe({
    req(top_gene_sets_plot())
    plots_ready$top_gene_sets <- TRUE
    message(
      "[GSEA][Top-scoring gene sets] Plot is ready."
    )
  })

  # Send the plot to the UI
  output$top_gene_sets <- renderPlotly({
    top_gene_sets_plot()
  })

  # Prepare data for download
  output$download_data_top_gene_sets <- dataDownload(
    name = "Top-scoring_gene_sets_data",
    data = data_set_loaded()[["GeneSets"]] %>%
      filter(Contrast == input$gene_sets_contrast_select),
    authors = authors()
  )

  observeEvent(
    event_data(event = "plotly_click", source = "gene_sets_plot"),
    {
      req(top_gene_sets_plot())

      # Get the information from the clicked point
      click_info_gene_sets_plot <- event_data(
        event = "plotly_click",
        source = "gene_sets_plot"
      )

      # Extract set size for heatmap height
      initial_set_size <- as.numeric(
        sub(
          ".*<b>Set size: </b>([0-9]+).*",
          "\\1",
          click_info_gene_sets_plot$customdata[1]
        )
      )

      # Extract the contrast form the event data and adjust format to DGEA constast format
      contrast <- click_info_gene_sets_plot$customdata[[1]][2]

      # Extract the gene set name from the customdata
      # This is the tooltip text defined in `plot_gsea_top_gene_sets()`
      pathway <- sub(
        ".*16px;'>(.*)</div>.*",
        "\\1",
        click_info_gene_sets_plot$customdata[1]
      )

      # Get the genes in the gene set
      pathway_genes <- data_set_loaded()[["GeneSetsGenes"]] %>%
        filter(GSName == pathway) %>%
        select(Symbol, GeneID)

      # Get the pathway description
      pathway_info <- data_set_loaded()[["GeneSetsGenes"]] %>%
        filter(GSName == pathway)
      pathway_description <- pathway_info$GSDescription[1]
      pathway_url <- pathway_info$GSURL[1]

      volcano_modal_plot <- reactive({
        createVolcanoPlot(
          df = data_set_loaded()[["DGEAnalysis"]] %>%
            filter(GeneID %in% pathway_genes$GeneID),
          # Allow to change the colors and the thresholds
          selected_palette = input$color_select_volcano_modal,
          p_threshold = input$p_threshold_volcano_modal,
          l2fc_threshold = input$l2fc_threshold_volcano_modal,
          # No need to further select any genes
          selected_genes = c(),
          selected_contrast = contrast,
          dot_size = 3
        )
      })

      # Create the volcano plot for the selected gene set form the selected contrast
      output$dgea_plot <- renderPlotly({
        volcano_modal_plot()
      })

      # The genes from the clicked pathway are send to the UI, i.e. `input$gene_select_heatmap_modal`
      # This is used here to get the genes from the data frame for the pathway heatmap
      # Furthermore, this allows the user to unselect unwanted genes form this pathway
      df_gsea_genes_heatmap <- reactive({
        formatForHeatmap(
          df = data_set_loaded()[["NormalizedCounts"]],
          selected_samples = input$sample_select_heatmap_modal,
          selected_genes = input$gene_select_heatmap_modal,
          # Avoid subsetting by median or variance and just use the genes from the selected pathway
          selected_subset_size = 0,
          gene_selection_by = FALSE
        )
      })

      # Calculate the heights using all selected genes
      heatmap_heights_modal <- reactive({
        heatmapHeights(
          n_genes = nrow(df_gsea_genes_heatmap()),
          dendro_type = input$heatmap_dendrogram_modal
        )
      })

      heatmap_modal_plot <- reactive({
        createGeneExpressionHeatmap(
          df = df_gsea_genes_heatmap(),
          id_or_sym = "Gene symbol",
          samples_groups = data_set_loaded()[["SamplesGroups"]],
          heatmap_colors = input$color_select_heatmap_tiles_modal,
          group_colors = input$color_select_heatmap_groups_modal,
          dendrogram_type = input$heatmap_dendrogram_modal,
          heatmap_heights = heatmap_heights_modal()
        )
      })

      # Create the heatmap with the genes form the selected pathway
      output$heatmap_plot_modal <- renderPlotly({
        heatmap_modal_plot()
      })

      # Send the heatmap to the modal
      output$heatmap_ui_modal <- renderUI({
        plotlyOutput(
          "heatmap_plot_modal",
          height = heatmap_heights_modal()$total_height
        )
      })

      # The pathway modal, with some information on the pathway and the volcano plot and the heatmap
      showModal(modalDialog(
        title = paste0("Enriched genes in ", pathway),
        easyClose = TRUE,
        footer = NULL,
        class = "enlarged-modal",
        tagList(
          tags$i(paste0(pathway_description, ".")),
          tags$a(
            href = pathway_url,
            target = "_blank",
            "For more information on this gene set, click here to enter MSigDB."
          ),
          tags$hr()
        ),
        tabsetPanel(
          tabPanel(
            "Volcano plot",
            div(
              virtualSelectInput(
                inputId = "color_select_volcano_modal",
                label = "Select color palette:",
                # The color choices are defined in `controls_colors.R`
                choices = color_choices,
                selected = "App colors",
                search = TRUE,
                showSelectedOptionsFirst = TRUE,
                # Add custom renderers for the colors, which include images of the color scales
                labelRenderer = "colorsWithIconChoice",
                selectedLabelRenderer = "colorsWithIconSelected"
              ),
              sliderTextInput(
                inputId = "p_threshold_volcano_modal",
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
                inputId = "l2fc_threshold_volcano_modal",
                label = "Set log2FC threshold:",
                min = 1,
                max = 5,
                value = 1,
                step = 0.25,
                ticks = FALSE
              ),
              actionButton(
                inputId = "open_custom_download_modal_volcano",
                label = "Download plot",
                class = "custom-button",
                icon = icon("download"),
                width = "155px"
              ),
              class = "tab-header-modal"
            ),
            hr(),
            plotlyOutput("dgea_plot", height = "auto")
          ),
          tabPanel(
            "Gene expression heatmap",
            div(
              virtualSelectInput(
                inputId = "gene_select_heatmap_modal",
                label = "Include genes of interest from this gene set:",
                choices = pathway_genes$Symbol,
                selected = pathway_genes$Symbol,
                multiple = TRUE,
                search = TRUE,
                showSelectedOptionsFirst = TRUE,
                disableSelectAll = TRUE
              ),
              virtualSelectInput(
                inputId = "heatmap_dendrogram_modal",
                label = "Show dendrogram for:",
                choices = c(
                  "Samples",
                  "Genes",
                  "Samples and genes",
                  "None"
                ),
                selected = "Samples and genes"
              ),
              virtualSelectInput(
                inputId = "sample_select_heatmap_modal",
                label = "Select samples:",
                choices = available_samples_by_groups(),
                selected = unname(unlist(available_samples_by_groups())),
                multiple = TRUE,
                search = TRUE,
                showSelectedOptionsFirst = TRUE
              ),
              virtualSelectInput(
                inputId = "color_select_heatmap_tiles_modal",
                label = "Select color palette:",
                # The color choices are defined in `controls_colors.R`
                choices = color_choices,
                selected = "App colors",
                search = TRUE,
                showSelectedOptionsFirst = TRUE,
                # Add custom renderers for the colors, which include images of the color scales
                labelRenderer = "colorsWithIconChoice",
                selectedLabelRenderer = "colorsWithIconSelected"
              ),
              virtualSelectInput(
                inputId = "color_select_heatmap_groups_modal",
                label = "Select color palette for groups:",
                # The color choices are defined in `controls_colors.R`
                choices = color_choices,
                selected = "inferno",
                search = TRUE,
                showSelectedOptionsFirst = TRUE,
                # Add custom renderers for the colors, which include images of the color scales
                labelRenderer = "colorsWithIconChoice",
                selectedLabelRenderer = "colorsWithIconSelected"
              ),
              actionButton(
                inputId = "open_custom_download_modal_heatmap",
                label = "Download plot",
                class = "custom-button",
                icon = icon("download"),
                width = "155px"
              ),
              class = "tab-header-modal"
            ),
            hr(),
            uiOutput("heatmap_ui_modal")
          )
        )
      ))

      # Open the custom modal for downloads from modals
      # This needs special treatment as usually shiny only allows one modal to be open at a time
      observeEvent(input$open_custom_download_modal_volcano, {
        output$custom_modal_ui_volcano <- renderUI({
          downloadSettingsModal(id = "volcano_modal")
        })
        session$sendCustomMessage("show-custom-modal-volcano", list())
      })
      # Open the custom modal for downloads from modals
      # This needs special treatment as usually shiny only allows one modal to be open at a time
      observeEvent(input$open_custom_download_modal_heatmap, {
        output$custom_modal_ui_heatmap <- renderUI({
          downloadSettingsModal(id = "heatmap_modal")
        })
        session$sendCustomMessage("show-custom-modal-heatmap", list())
      })

      modal_plot_ids <- c("volcano_modal", "heatmap_modal")

      lapply(modal_plot_ids, function(id) {
        # Download handler for plot downloads from the modal
        output[[paste0(
          "download_plot_",
          id
        )]] <- downloadHandler(
          filename = function() {
            # Create the name of the downloaded file similar to the data downloads
            paste0(
              id,
              "_plot_",
              authors(),
              "_",
              Sys.Date(),
              ".",
              input[[paste0("plot_format_", id)]]
            )
          },
          content = function(file) {
            # Use `save_image()` from plotly to save the plot in the desired format
            # This depends on python, thus, usage of `reticulate` and `kaleido`
            save_image(
              p = eval(parse(text = paste0(id, "_plot()"))),
              file = file,
              width = input[[paste0(
                "plot_width_",
                id
              )]],
              height = input[[paste0(
                "plot_height_",
                id
              )]]
            )
          }
        )
      })
    }
  )

  ###################################################################################################
  # Subchapter: Contrast intersection by gene sets ####
  ###################################################################################################

  df_gsea_ci <- reactive({
    req(data_set_loaded())
    formatGSEAContrastIntersection(
      df = data_set_loaded()[["GeneSets"]]
    )
  })

  contrast_intersection_sets_plot <- reactive({
    createGSEAContrastIntersectionPlot(
      df = df_gsea_ci(),
      selected_palette = input$color_select_contrast_intersection_sets
    )
  })

  observe({
    req(contrast_intersection_sets_plot())
    plots_ready$jaccard_gsea <- TRUE
    message(
      "[GSEA][Contrast intersection] Plot is ready."
    )
  })

  output$jaccard_gsea <- renderPlotly({
    contrast_intersection_sets_plot()
  })

  output$download_data_contrast_intersection_sets <- dataDownload(
    name = "Contrast_intersection_GSEA",
    data = df_gsea_ci(),
    authors = authors()
  )

  ###################################################################################################
  # General Things at Run Time #####
  ###################################################################################################

  # Observe if a drop down menu is selected, then fade background
  observe({
    if (
      isTRUE(input$`raw_counts_content-plot_settings_state`) |
        isTRUE(input$plot_settings_pca_state) |
        isTRUE(input$plot_settings_heatmap_state) |
        isTRUE(input$plot_settings_top_genes_state) |
        isTRUE(input$plot_settings_volcano_state) |
        isTRUE(input$plot_settings_contrast_intersection_state) |
        isTRUE(input$plot_settings_top_gene_sets_state) |
        isTRUE(input$plot_settings_contrast_intersection_sets_state)
    ) {
      show(
        "app-overlay",
        anim = TRUE,
        animType = "fade",
        time = 0.2
      )
    } else {
      hide(
        "app-overlay",
        anim = TRUE,
        animType = "fade",
        time = 0.2
      )
    }
  })

  # Show modals upon clicking the `actionButton()` with 'Further information'
  lapply(names(further_info_contents), function(id) {
    observeEvent(input[[id]], {
      showModal(modalDialog(
        title = "Further information",
        easyClose = TRUE,
        footer = NULL,
        further_info_contents[[id]]
      ))
    })
  })

  # IDs of plots for download
  download_plot_ids <- c(
    "pca",
    "heatmap",
    "top_genes",
    "volcano",
    "contrast_intersection",
    "top_gene_sets",
    "contrast_intersection_sets"
  )

  lapply(download_plot_ids, function(id) {
    # Show modal with download option upon clicking the corresponding `actionButton()`
    observeEvent(input[[paste0("download_plot_", id)]], {
      showModal(modalDialog(
        title = "Download plot",
        easyClose = TRUE,
        footer = NULL,
        virtualSelectInput(
          inputId = paste0("plot_format_", id),
          label = "Select file format:",
          choices = c("png", "jpeg", "svg", "webp", "pdf"),
          # eps may also be included, if desired but requires additional python packages
          selected = "png"
        ),
        # Adjust downloaded plot size and format
        numericInput(
          inputId = paste0("plot_height_", id),
          label = "Height (in px):",
          value = 720,
          min = 1,
          max = 100000
        ),
        numericInput(
          inputId = paste0("plot_width_", id),
          label = "Width (in px):",
          value = 1280,
          min = 1,
          max = 100000
        ),
        downloadButton(
          outputId = paste0("download_plot_", id, "_modal"),
          label = "Download plot",
          class = "custom-button"
        )
      ))
    })

    # Download handler for plot downloads from the modal
    output[[paste0("download_plot_", id, "_modal")]] <- downloadHandler(
      filename = function() {
        # Create the name of the downloaded file similar to the data downloads
        paste0(
          id,
          "_plot_",
          authors(),
          "_",
          Sys.Date(),
          ".",
          input[[paste0("plot_format_", id)]]
        )
      },
      content = function(file) {
        # Use `save_image()` from plotly to save the plot in the desired format
        # This depends on python, thus, usage of `reticulate` and `kaleido`
        save_image(
          p = eval(parse(text = paste0(id, "_plot()"))),
          file = file,
          width = input[[paste0("plot_width_", id)]],
          height = input[[paste0("plot_height_", id)]]
        )
      }
    )
  })
}
