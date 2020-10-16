library(magrittr)
library(rlang)

tcga_expression_id <- "syn4976369"
output_file_name   <- "gene_map.tsv"

synapser::synLogin()

tcga_expression_id %>% 
    synapser::synGet() %>% 
    purrr::pluck("path") %>% 
    data.table::fread(select = "gene_id") %>% 
    tidyr::separate("gene_id", into = c("hugo", "entrez"), sep = "\\|") %>% 
    dplyr::mutate(
        hugo = stringr::str_replace_all(.data$hugo, "\\?", NA_character_)
    ) %>% 
    readr::write_tsv(output_file_name)

activity_obj <- synapser::Activity(
    name = "pull out gene map from tcga expression",
    used = list(tcga_expression_id),
    executed = paste0(
        "https://github.com/CRI-iAtlas/TCGA/create_iatlas_sample_file.R"
    )
)

output_file_name %>% 
    synapser::File(parent = "syn21786285") %>% 
    synapser::synStore(activity = activity_obj)

rm(output_file_name)
