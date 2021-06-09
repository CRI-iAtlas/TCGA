create_tags <- function(){
  
  all_tags <- "syn22140514" %>%
    synapse_feather_id_to_tbl(.)
  
  tcga_subtypes_all <- all_tags %>%
    dplyr::filter(.data$sample_group == "Subtype_Curated_Malta_Noushmehr_et_al") %>% 
    dplyr::select(
      "old_name" = "FeatureValue",
      "name" = "FeatureValue",
      "old_parent_name" = "TCGA Studies",
      "parent_name" = "TCGA Studies",
      "parent_short_display" = "TCGA Studies",
      "parent_long_display" = "FeatureDisplayName",
    ) %>% 
    dplyr::mutate(
      "name" = stringr::str_replace_all(.data$name, "[:punct:]", "_"),
      "name" = stringr::str_remove_all(.data$name, "([:space:])"),
      "short_display" = .data$name,
      "short_display" = stringr::str_replace_all(.data$short_display, "_", " "),
      "long_display" = .data$short_display,
      "parent_name" = stringr::str_replace_all(.data$parent_name, "[:punct:]", "_"),
      "parent_name" = stringr::str_c(.data$parent_name, "_subtypes")
    ) 
  
  tcga_subtypes <- 
    dplyr::bind_rows(
      tcga_subtypes_all %>% 
        dplyr::select("name", "old_name", "short_display", "long_display"),
      
      tcga_subtypes_all %>%
        dplyr::select(
          "name" = "parent_name",
          "short_display" = "parent_short_display",
          "long_display" = "parent_long_display"
        ) %>% 
        dplyr::distinct()
    )
  
  tcga_subtype_relationships <- 
    dplyr::bind_rows(
      tcga_subtypes_all %>%
        dplyr::select("tag" = "name") %>% 
        dplyr::mutate("related_tag" = "TCGA_Subtype"),
      
      tcga_subtypes_all %>%
        dplyr::select("tag" = "name")%>% 
        dplyr::mutate("related_tag" = "group"),
      
      tcga_subtypes_all %>%
        dplyr::select("tag" = "name", "related_tag" = "parent_name"),
      
      tcga_subtypes_all %>%
        dplyr::select("tag" = "parent_name") %>% 
        dplyr::mutate("related_tag" = "TCGA_Subtype") %>% 
        dplyr::distinct(),
      
      tcga_subtypes_all %>%
        dplyr::select("tag" = "parent_name") %>% 
        dplyr::mutate("related_tag" = "metagroup") %>% 
        dplyr::distinct()
    )
  
  tcga_studies <- all_tags %>%
    dplyr::filter(.data$sample_group == "Study") %>% 
    dplyr::select(
      "old_name" = "FeatureValue",
      "name" = "FeatureValue",
      "characteristics" = "Characteristics",
      "short_display" = "FeatureValue",
      "long_display" = "FeatureName",
      "color" = "FeatureHex"
    )
  
  tcga_study_relationships <- 
    dplyr::bind_rows(
      tcga_studies %>%
        dplyr::select("tag" = "name") %>% 
        dplyr::mutate("related_tag" = "TCGA_Study"),
      
      tcga_studies %>%
        dplyr::select("tag" = "name") %>% 
        dplyr::mutate("related_tag" = "group")
    )
  
  immune_subtypes <- all_tags %>%
    dplyr::filter(.data$sample_group == "Subtype_Immune_Model_Based") %>% 
    dplyr::select(
      "old_name" = "FeatureValue",
      "name" = "FeatureValue",
      "characteristics" = "Characteristics",
      "short_display" = "FeatureValue",
      "long_display" = "FeatureName",
      "color" = "FeatureHex"
    )
  
  immune_subtype_relationships <- 
    dplyr::bind_rows(
      immune_subtypes %>%
        dplyr::select("tag" = "name") %>% 
        dplyr::mutate("related_tag" = "Immune_Subtype"),
      
      immune_subtypes %>%
        dplyr::select("tag" = "name") %>% 
        dplyr::mutate("related_tag" = "group")
    )
  
  tcga_meta_tags <- 
    dplyr::tibble(
      "name" = c("Immune_Subtype", "TCGA_Subtype", "TCGA_Study"),
      "short_display" = c("Immune Subtype", "TCGA Subtype", "TCGA Study"),
      "long_display" = c("Immune Subtype", "TCGA Subtype", "TCGA Study"),
    ) 
  
  tags <- 
    dplyr::bind_rows(
      immune_subtypes, tcga_studies, tcga_subtypes, tcga_meta_tags
    ) %>% 
    dplyr::select("name", "old_name", "short_display", "long_display", "color", "characteristics")
  
  parent_groups <- dplyr::tibble(
    "tag" = c("Immune_Subtype", "TCGA_Subtype", "TCGA_Study"),
    "related_tag" = "parent_group"
  )
  
  tag_relationships <- dplyr::bind_rows(
    immune_subtype_relationships, 
    tcga_study_relationships, 
    tcga_subtype_relationships,
    parent_groups
  )
  
  synapse_store_feather_file(
    tags,
    "tcga_tags.feather",
    "syn22889307"
  )
  
  synapse_store_feather_file(
    tag_relationships,
    "tcga_tag_relationships.feather",
    "syn22889307"
  )
}


