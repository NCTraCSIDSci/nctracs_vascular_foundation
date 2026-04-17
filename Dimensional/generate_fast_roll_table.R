generate_fast_roll_table <- function(extant_condition_network,specificity_naughty_list,generality_naughty_list,minimum_count,maximum_roll)
{
    # Identify "naughty list" specificity by pulling all descendants of the "naughty" ancestors.
    descendant_naughty_list <- concept_ancestor %>%
        filter(ancestor_concept_id %in% specificity_naughty_list) %>%
        pull(descendant_concept_id)
    
    # This creates a basic rollup chart usable for conditions.
    narrow_extant_condition_network <- extant_condition_network %>%
        filter(!ancestor_concept_id %in% descendant_naughty_list &
                   !ancestor_concept_id %in% specificity_naughty_list &
                   !ancestor_concept_id %in% generality_naughty_list)
    
    preserved_conditions <- extant_condition_network %>%
        filter(descendant_concept_id == ancestor_concept_id &
                   ancestor_count >= minimum_count)
    
    proposed_rollups <- narrow_extant_condition_network %>%
        filter(ancestor_count >= minimum_count &
                   descendant_count < minimum_count) %>%
        group_by(descendant_concept_id) %>%
        filter(min_levels_of_separation == min(min_levels_of_separation) &
                   min_levels_of_separation <= max_roll_distance) %>%
        ungroup()
    
    
    roll_table_fast <- proposed_rollups %>%
        union(preserved_conditions) %>%
        select(ancestor_concept_id, descendant_concept_id) %>%
        rename(concept_id = descendant_concept_id, rolled_concept_id = ancestor_concept_id)
    
    # Check orphan count.
    orphans <- narrow_extant_condition_network %>%
        filter(!descendant_concept_id %in% preserved_conditions$descendant_concept_id &
                   !descendant_concept_id %in% proposed_rollups$descendant_concept_id) %>%
        select(descendant_concept_id) %>%
        distinct()
    
    print(paste0("Network conditions: ",nrow(narrow_extant_condition_network)))
    print(paste0("Rollups: ",nrow(proposed_rollups)))
    print(paste0("Ancestors: ",length(unique(proposed_rollups$ancestor_concept_id))))
    print(paste0("Targets: ",length(unique(proposed_rollups$descendant_concept_id))))
    print(paste0("Preserved: ",nrow(preserved_conditions)))
    print(paste0("Outputs: ",length(unique(roll_table_fast$rolled_concept_id))))
    print(paste0("Table size: ", nrow(roll_table_fast)))
    print(paste0("Orphaned: ",nrow(orphans)))
    return(roll_table_fast)
}
