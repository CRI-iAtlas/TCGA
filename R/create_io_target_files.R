create_io_targets_file <- function(){
  
  io_targets <- "syn22151533" %>%
    synapse_feather_id_to_tbl(.) %>%
    dplyr::select(
      "entrez" = "Entrez ID",
      "io_landscape_name" = "Friendly Name",
      "pathway" = "Pathway",
      "therapy_type" = "Therapy Type",
      "description" = "Description"
    ) %>%
    dplyr::mutate("entrez" = as.integer(entrez)) %>%
    dplyr::group_by(entrez) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  synapse_store_feather_file(
    io_targets, 
    "io_targets.feather",
    "syn22889307"
  )
}
