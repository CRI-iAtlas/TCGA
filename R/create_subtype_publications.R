create_subtype_publications_files <- function(){
  
  doi_pattern <- "^https://doi.org/([:print:]+$)"
  nature_pattern <- "(https://doi.org/[:print:]+?/nature[:digit:]+)"
  cell_pattern <- "(https://doi.org/[:digit:]+.[:digit:]+/j.cell.[:digit:]+.[:digit:]+.[:digit:]+)"
  cancer_cell_pattern <- "(https://doi.org/[:digit:]+.[:digit:]+/j.cc[:print:]+?.[:digit:]+.[:digit:]+.[:digit:]+)"
  nejm_pattern <- "(https://doi.org/[:digit:]+.[:digit:]+/NEJMoa[:digit:]+)"
  
  
  get_doid_from_doi_link <- function(link){
    link %>%
      stringr::str_match(., doi_pattern) %>%
      purrr::pluck(2)
  }
  
  get_journal_do_pattern <- function(journal){
    if (journal == "Nature") return(nature_pattern)
    else if (journal == "Cell") return(cell_pattern)
    else if (journal == "Cancer Cell") return(cancer_cell_pattern)
    else if (journal == "Nejm") return(nejm_pattern)
  }
  
  get_doid_with_pattern <- function(link, pattern){
    link %>%
      curl::curl_fetch_memory(.) %>%
      purrr::pluck("content") %>%
      rawToChar() %>%
      stringr::str_match_all(., pattern) %>%
      unlist() %>%
      unique() %>%
      get_doid_from_doi_link(.)
  }
  
  get_doid <- function(link, journal){
    if (stringr::str_detect(link, doi_pattern)) {
      return(stringr::str_match(link, doi_pattern)[,2])
    } else if (journal %in% c("Nature", "Cell", "Cancer Cell", "Nejm")) {
      pattern <- get_journal_do_pattern(journal)
      return(get_doid_with_pattern(link, pattern))
    } else {
      return(NA_character_)
    }
  }
  
  new_subtype_tbl <- "syn23545011" %>%
    synapse_feather_id_to_tbl(.) %>%
    dplyr::select("name", "old_name")
  
  subtype_tbl <- "syn22140514" %>%
    synapse_feather_id_to_tbl(.) %>%
    dplyr::filter(
      .data$sample_group == "Subtype_Curated_Malta_Noushmehr_et_al"
    ) %>%
    dplyr::select("old_name" = "FeatureValue", "temp" = "Characteristics") %>%
    tidyr::separate(
      "temp", " ; ",
      into = c("temp", "link"),
      extra = "merge"
    ) %>%
    dplyr::inner_join(new_subtype_tbl) %>%
    dplyr::select("name", "link", "temp")
  
  do_tbl <- subtype_tbl %>%
    dplyr::select("link", "temp") %>%
    dplyr::distinct() %>%
    tidyr::separate(
      "temp",
      " ",
      into = c("journal1", "journal2", "year"),
      fill = "left"
    ) %>%
    tidyr::unite("journal", "journal1", "journal2", sep = " ", na.rm = T) %>%
    dplyr::mutate("journal" = stringr::str_to_title(.data$journal)) %>%
    dplyr::arrange(.data$journal) %>%
    dplyr::mutate("link" = dplyr::if_else(
      .data$link == "http://linkinghub.elsevier.com/retrieve/pii/S0092-8674(17)30639-6",
      "https://www.cell.com/cell/fulltext/S0092-8674(17)30639-6",
      .data$link
    )) %>%
    dplyr::mutate("do_id" = purrr::map2_chr(.data$link, .data$journal, get_doid)) %>%
    dplyr::mutate("journal" = dplyr::if_else(
      .data$journal == "Nejm",
      "N. Engl. J. Med.",
      .data$journal
    ))
  
  pubmed_tbl <- do_tbl %>%
    dplyr::pull("do_id") %>%
    purrr::map(easyPubMed::get_pubmed_ids) %>%
    purrr::map(easyPubMed::fetch_pubmed_data, encoding = "ASCII") %>%
    purrr::map(easyPubMed::article_to_df) %>%
    purrr::map(dplyr::as_tibble) %>%
    purrr::map(dplyr::slice, 1) %>%
    dplyr::bind_rows() %>%
    dplyr::select(
      "do_id" = "doi",
      "pubmed_id" = "pmid",
      "first_author_last_name" = "lastname",
      "title"
    )
  
  publication_tbl <-
    dplyr::left_join(do_tbl, pubmed_tbl, by = "do_id") %>%
    tidyr::unite("publication_name", "do_id", "pubmed_id", remove = F) %>%
    dplyr::inner_join(
      subtype_tbl %>%
        dplyr::select("link", "tag_name" = "name"),
      by = "link"
    )
  
  synapse_store_feather_file(
    publication_tbl,
    "tcga_subtype_publications.feather",
    "syn22889307"
  )
}





