process_user_genes <- function(available_genes, user_genes) {
  # Try most generic read function to capture user's mistakes
  user_genes <- read.table(
    file = user_genes$datapath,
    header = FALSE,
    fill = TRUE,
    col.names = "UserGenes"
  ) %>%
    # Avoid case-sensitive filtering
    mutate(UserGenes = tolower(UserGenes)) %>%
    as.data.frame()

  df_genes <- available_genes %>%
    # Avoid case-sensitive filtering
    mutate(Alias = tolower(Alias)) %>%
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
    select(Alias, Symbol) %>%
    rename("User's Gene" = "Alias", "Found symbol" = "Symbol")

  return(
    list(
      genesFound = df_genes$Symbol[!is.na(df_genes$Symbol)],
      genesNotFound = genes_not_found,
      genesFoundMulti = genes_multiple
    )
  )
}
