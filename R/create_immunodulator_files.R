create_immunomodulator_files <- function(){
  
  im_tbl <- "syn22151531" %>%
    synapse_feather_id_to_tbl(.) %>% 
    dplyr::select(
      "entrez" = "Entrez ID",
      "friendly_name" = "Friendly Name",
      "gene_family" = "Gene Family",
      "super_category" = "Super Category",
      "immune_checkpoint" = "Immune Checkpoint",
      "gene_function" = "Function",
      "link" = "Reference(s) [PMID]"
    ) %>%
    dplyr::filter(!is.na(entrez))
  
  im_tbl %>% 
    dplyr::select(-link) %>% 
    synapse_store_feather_file(
      "immunomodulators.feather",
      "syn22889307"
    )
  
  publication_tbl1 <- im_tbl %>% 
    dplyr::select("entrez", "link") %>%
    tidyr::drop_na() %>%
    tidyr::separate_rows("link", sep = " \\| ") %>%
    dplyr::mutate(
      "pubmed_id" = stringr::str_match(.data$link, "([:digit:]+)")[,2]
    ) %>%
    dplyr::select("entrez", "pubmed_id") %>% 
    dplyr::distinct()
  
  synapse_store_feather_file(
    publication_tbl1, 
    "immunomodulator_entrez_pubmed_id.feather",
    "syn22889307"
  )
  
  publication_tbl2 <- publication_tbl1 %>% 
    dplyr::pull("pubmed_id") %>% 
    unique %>% 
    purrr::map(easyPubMed::get_pubmed_ids) %>%
    purrr::map(easyPubMed::fetch_pubmed_data, encoding = "ASCII") %>%
    purrr::map(easyPubMed::article_to_df) %>%
    purrr::map(dplyr::slice, 1) %>%
    dplyr::bind_rows() %>%
    dplyr::as_tibble() %>%
    dplyr::select(
      "do_id" = "doi",
      "pubmed_id" = "pmid",
      "journal" = "jabbrv",
      "first_author_last_name" = "lastname",
      "year",
      "title"
    ) %>%
    dplyr::mutate("do_id" = dplyr::if_else(
      .data$do_id == "",
      NA_character_,
      .data$do_id
    )) %>%
    tidyr::unite("name", "do_id", "pubmed_id", remove = F)
  
  synapse_store_feather_file(
    publication_tbl2, 
    "immunomodulator_publications.feather",
    "syn22889307"
  )
}




