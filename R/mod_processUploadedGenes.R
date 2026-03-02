#' @title Process Uploaded User Genes
#'
#' @description
#' Matches user-uploaded gene names to available gene aliases in the RNA-seq dataset. Handles case-insensitive matching, identifies genes not found, and detects user genes mapping to multiple symbols.
#'
#' @param available_genes A data frame of available genes with at least columns `Alias` (gene aliases, lowercased) and `Symbol` (official gene symbol).
#'
#' @param user_genes A file input object from \code{\link[shiny]{fileInput}()}, containing a `datapath` to the uploaded gene list.
#'
#' @return A list with elements:
#'
#' * `genesFound`: character vector of matched gene symbols.
#'
#' * `genesNotFound`: character vector of user genes not found.
#'
#' * `genesFoundMulti`: data frame of user genes mapping to multiple symbols.
#'
#' @export
processUserGenes <- function(available_genes, user_genes) {
  # Define variables locally for R CMD check
  UserGenes <- Alias <- Symbol <- NULL

  # Try most generic read function to capture user's mistakes
  user_genes <- read.table(
    file = user_genes$datapath,
    header = FALSE,
    fill = TRUE,
    col.names = "UserGenes"
  ) %>%
    # Avoid case-sensitive filtering
    mutate(UserGenes = base::tolower(UserGenes)) %>%
    as.data.frame()

  df_genes <- available_genes %>%
    # Avoid case-sensitive filtering
    mutate(Alias = base::tolower(Alias)) %>%
    # Check which of the user selected genes are present in the RNAseq data
    # To do so, compare user-defined gene names with all available aliases
    filter(Alias %in% user_genes$UserGenes) %>%
    # Combine both data frames
    full_join(user_genes, by = c("Alias" = "UserGenes"))

  # Identify NAs
  # Because of the full_join, these are the user-defined genes that did not match any alias
  genes_not_found <- df_genes %>% filter(is.na(Symbol)) %>% pull(Alias)

  # Identify multiple mappings
  genes_multiple <- df_genes %>%
    group_by(Alias) %>%
    filter(n() > 1) %>%
    dplyr::select(Alias, Symbol) %>%
    dplyr::rename("User's gene" = "Alias", "Found symbol" = "Symbol")

  return(
    list(
      genesFound = df_genes$Symbol[!is.na(df_genes$Symbol)],
      genesNotFound = genes_not_found,
      genesFoundMulti = genes_multiple
    )
  )
}
