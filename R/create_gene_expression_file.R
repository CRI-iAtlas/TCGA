create_gene_expression_file <- function(){
  
  tcga_aliquots <- "syn21435422" %>%
    synapse_delimited_id_to_tbl(.) %>%
    dplyr::rename("patient" = 1, "aliqout" = 2) %>%
    tidyr::drop_na() %>%
    dplyr::pull("aliqout")
  
  tcga_samples <- "syn22139885" %>%
    synapse_feather_id_to_tbl(.) %>%
    dplyr::pull("name")
  
  expression_tbl <- "syn22890627" %>%
    synapse_delimited_id_to_tbl(.) %>%
    tidyr::separate("gene_id", sep = "\\|", into = c("hgnc", "entrez")) %>%
    dplyr::mutate("entrez" = as.integer(.data$entrez)) %>%
    dplyr::select(dplyr::any_of(c("hgnc", "entrez", tcga_aliquots))) %>%
    dplyr::rename_with(~stringr::str_sub(.x, 1, 12)) %>%
    dplyr::select(dplyr::any_of(c("hgnc", "entrez", tcga_samples))) %>%
    tidyr::pivot_longer(
      cols      = -c("hgnc", "entrez"), 
      names_to  = "sample", 
      values_to = "rna_seq_expr"
    ) %>% 
    dplyr::filter(!is.na(.data$rna_seq_expr))

  synapse_store_feather_file(
    expression_tbl,
    "expression.feather",
    "syn22889307"
  )
}
