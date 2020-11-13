synapse_feather_id_to_tbl <- function(id) {
  synapser::synLogin()
  id %>%
    synapser::synGet() %>%
    purrr::pluck("path") %>%
    arrow::read_feather(.) %>%
    dplyr::as_tibble()
}

synapse_store_file <- function(file, parent_id) {
  synapser::synLogin()
  file_entity <- synapser::File(file, parent_id)
  synapser::synStore(file_entity)
}

synapse_store_feather_file <- function(df, file_name, parent_id){
  arrow::write_feather(df, file_name)
  synapse_store_file(file_name, parent_id)
  file.remove(file_name)
}