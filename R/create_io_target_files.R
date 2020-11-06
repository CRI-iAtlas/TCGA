library(magrittr)

synapse_feather_id_to_tbl <- function(id) {
    synapser::synLogin()
    id %>%
        synapser::synGet() %>%
        purrr::pluck("path") %>%
        feather::read_feather(.) %>%
        dplyr::as_tibble()
}

synapse_store_file <- function(file, parent_id) {
    synapser::synLogin()
    file_entity <- synapser::File(file, parent_id)
    synapser::synStore(file_entity)
}

synapse_store_feather_file <- function(df, file_name, parent_id){
    feather::write_feather(df, file_name)
    synapse_store_file(file_name, parent_id)
    file.remove(file_name)
}

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