# Create list for tab details
# The names are the IDs for the output
# The values are the headings of `dataSetsTable` or the text itself
tab_details <- list(
  # These information are withdrawn from `dataSetsTable`
  raw_data_details = "Sequencing Details",
  dgea_details = "DGEA Details",
  gsea_details = "GSEA Details"
)

# Generate the headers for the tabs
tabHeaders <- function(title, text_id) {
  fluidPage(
    div(
      titlePanel(title = title, windowTitle = title),
      textOutput(text_id),
      class = "tab-header"
    )
  )
}
