
require(rlang)
require(magrittr)

feature_tbl <- "syn22276118" %>% 
    synapser::synGet() %>% 
    purrr::pluck("path") %>% 
    feather::read_feather(.) %>% 
    dplyr::filter(.data$feature %in% c("leukocyte_fraction", "B_cells_memory"))

feather::write_feather(feature_tbl, "features.feather")

"features.feather" %>% 
    synapser::File(parent = "syn22334042") %>% 
    synapser::synStore()

rm("features.feather")


tag_tbl <- "syn22141045" %>% 
    synapser::synGet() %>% 
    purrr::pluck("path") %>% 
    feather::read_feather(.) %>% 
    dplyr::filter(.data$related_tag == "Immune_Subtype")

group_tbl <- "syn22140429" %>% 
    synapser::synGet() %>% 
    purrr::pluck("path") %>% 
    feather::read_feather(.) %>% 
    dplyr::inner_join(tag_tbl, by = "tag") %>% 
    dplyr::select("sample", "group" = "tag")

feather::write_feather(group_tbl, "groups.feather")

"groups.feather" %>% 
    synapser::File(parent = "syn22334042") %>% 
    synapser::synStore()

rm("groups.feather")

mutation_tbl <- "syn22140158" %>% 
    synapser::synGet() %>% 
    purrr::pluck("path") %>% 
    feather::read_feather(.) %>% 
    tidyr::unite("mutation", "entrez", "mutation_code", sep = ":") %>% 
    dplyr::select("sample", "mutation", "status") %>% 
    dplyr::filter(.data$mutation %in% c("9125:P131L", "7157:Q136*"))

feather::write_feather(mutation_tbl, "mutations.feather")

"mutations.feather" %>% 
    synapser::File(parent = "syn22334042") %>% 
    synapser::synStore()

rm("mutations.feather")
