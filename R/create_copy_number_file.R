create_copy_number_file <- function(){
  
  tcga_hgnc_to_entrez <- "syn22133677" %>% 
    synapse_feather_id_to_tbl() %>%
    dplyr::mutate("entrez" = as.integer(.data$entrez))
  
  tcga_samples <- "syn22139885" %>%
    synapse_feather_id_to_tbl(.) %>%
    dplyr::pull("name")
  
  copy_number_tbl <- "syn22889333" %>%
    synapse_delimited_id_to_tbl() %>%
    dplyr::select(-'Locus ID', -'Cytoband') %>%
    dplyr::rename_with(~stringr::str_sub(.x, 1, 12)) %>%
    dplyr::rename("hgnc" = "Gene Symbol") %>%
    dplyr::left_join(tcga_hgnc_to_entrez, by = "hgnc") %>%
    dplyr::select(dplyr::any_of(c("hgnc", "entrez", tcga_samples))) %>% 
    tidyr::pivot_longer(
      cols = -c("hgnc", "entrez"), 
      names_to = "sample", 
      values_to = "copy_number"
    ) %>%
    dplyr::filter(!is.na(.data$copy_number))
  
  synapse_store_feather_file(
    copy_number_tbl,
    "copy_number.feather",
    "syn22889307"
  )
}
