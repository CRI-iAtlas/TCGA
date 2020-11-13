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
  file.remove(file)
}

synapse_store_feather_file <- function(df, file_name, parent_id){
  arrow::write_feather(df, file_name, compression = "uncompressed")
  synapse_store_file(file_name, parent_id)
}

synapse_delimited_id_to_tbl <- function(id, delim = "\t") {
  synapser::synLogin()
  id %>%
    synapser::synGet() %>%
    purrr::pluck("path") %>%
    readr::read_delim(., delim = delim)
}