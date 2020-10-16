require(rlang)
require(magrittr)

synapser::synLogin()

cell_features <- c(
    "Dendritic_cells_Aggregate2",
    "T_cells_CD8_Aggregate2", 
    "Macrophage_Aggregate2",     
    "Tumor_cell"   
)

genes <- c(8995, 8784, 7292, 7293, 8744, 3604, 29126, 5133, 80380, 959, 958)

"syn22276118" %>% 
    synapser::synGet() %>% 
    purrr::pluck("path") %>% 
    feather::read_feather(.) %>%
    dplyr::filter(.data$feature %in% cell_features) %>% 
    feather::write_feather(., "cells.feather")

"cells.feather" %>% 
    synapser::File(parent = "syn22336355") %>% 
    synapser::synStore()

rm("cells.feather")

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
    dplyr::select("sample", "group" = "tag") %>% 
    feather::write_feather(., "groups.feather")

"groups.feather" %>% 
    synapser::File(parent = "syn22336355") %>% 
    synapser::synStore()

rm("groups.feather")

expression_tbl <- "syn22130980" %>% 
    synapser::synGet() %>% 
    purrr::pluck("path") %>% 
    feather::read_feather(.) %>% 
    dplyr::filter(entrez %in% genes) %>% 
    feather::write_feather(., "expression.feather")

"expression.feather" %>% 
    synapser::File(parent = "syn22336355") %>% 
    synapser::synStore()

rm("expression.feather")
