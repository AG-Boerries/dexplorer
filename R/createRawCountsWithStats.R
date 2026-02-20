#' @title Read STAR Output, Create Raw Counts Data Frame And Statistics Data Frame
#'
#' @description
#' This function reads all `ReadsPerGene.out.tab` files from a STAR output directory and combines them into a single data frame. It also calculates some basic statistics for each sample, such as the number of detected genes. The function allows the user to specify the strandedness of the library preparation protocol to select the correct counting method from ht-seq. On demand, it can fix `RunID`s using a user-provided function, which is useful when joining functions return multiple matches.
#'
#' @param path_to_STAR_directory The path to the STAR output containing the ReadsPerGene.out.tab files.
#'
#' @param strandedness Specify strandedness of the library prep protocol to select the correct counting method from ht-seq. Defaults to `CountsUnstranded`.
#'
#' @param fix_runid A function to fix the `RunID`s. Useful when \code{\link[fuzzyjoin]{fuzzy_left_join}()} returns multiple matches. The default is `NULL` and it is suggested only to provide a function when the first run resulted in multiple matches.
#'
#' @return A list containing:
#'
#' * `RawCounts`: A data frame with raw counts (> 0 counts per gene) for all samples. This data frame is required for the ridgeline distribution plots in |Raw data| -> |Quality control| -> |Read count distribution|.
#'
#' * `Stats`: A data frame with statistics for each sample. This data frame is required for the bar plots in |Raw data| -> |Quality control| -> |Number of reads| and |Raw data| -> |Quality control| -> |Number of genes|.
#'
#' @export
createRawCountsWithStats <- function(
  path_to_STAR_directory,
  strandedness = c("CountsUnstranded", "CountsForward", "CountsReverse"),
  fix_runid = NULL
) {
  # Define variables locally for R CMD check
  GeneID <- Counts <- RunID <- NULL

  # Check if the user specified a correct value for `strandedness`
  strandedness <- match.arg(strandedness)

  # List all `ReadsPerGene.out.tab` files in the STAR output directory
  ReadsPerGeneTab <- list.files(
    path = path_to_STAR_directory,
    pattern = "ReadsPerGene.out.tab",
    full.names = TRUE,
    recursive = TRUE
  )

  # Read all tabs into name list of data frames
  ReadTabs <- lapply(
    ReadsPerGeneTab,
    function(x) {
      read_tsv(
        x,
        col_names = c(
          "GeneID",
          "CountsUnstranded",
          "CountsForward",
          "CountsReverse"
        ),
        # The second column defines the strandedness of the RNA-prep protocol
        col_select = all_of(c("GeneID", strandedness)),
        # Then the first column is always a character, it contains the gene IDs
        # The second column is always numeric, it contains the counts
        col_types = list(col_character(), col_double())
      ) %>%
        # Rename counts column to a common name
        dplyr::rename(Counts = strandedness)
    }
  )

  # Extract the ID and use it as name for the data frame
  # This can eventually be used to translate to sample names
  names(ReadTabs) <- lapply(basename(ReadsPerGeneTab), function(x) {
    sub(".?ReadsPerGene.out.tab$", "", x)
  })

  # Calculate statistics for all samples and add the `RunID` (name of the file)
  DfStats <- imap_dfr(
    ReadTabs,
    ~ calculateStats(.x) %>%
      mutate(RunID = .y)
  )

  # Combine all data frames into one long data frame
  # This is required for the ridgeline distribution plots
  DfRawCounts <- bind_rows(ReadTabs, .id = "RunID") %>%
    as.data.frame() %>%
    # Remove the meta information on the sequencing quality, which is typically stored in the first 4 rows
    filter(str_detect(GeneID, "^ENS"))

  # Filter to keep only genes with `counts > 0`
  DfRawCountsAboveZero <- DfRawCounts %>%
    filter(Counts > 0)

  # Calculate the total number of detected genes across all samples
  TotalGenes <- length(unique(DfRawCountsAboveZero$GeneID))

  # Add the TotalGenes to `DfStats`
  DfStats <- DfStats %>%
    add_row(
      RunID = "All samples",
      GenesRecorded = TotalGenes
    )

  # Create list of dataframes
  Dfs <- list(
    RawCounts = DfRawCountsAboveZero,
    Stats = DfStats
  )

  # If fixing the `RunID` is requested, apply the function to all data frames
  if (!is.null(fix_runid)) {
    # Make sure the provided value is a function
    assert_that(is.function(fix_runid))
    Dfs <- lapply(Dfs, function(df) {
      df %>% mutate(RunID = fix_runid(RunID))
    })
    return(Dfs)
  } else {
    return(Dfs)
  }
}
