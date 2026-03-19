generate_rollup_candidates <- function(priority_table, ancestor_table, ancestor_naughty_list, max_level = 1, min_level = 1)
{
    condensed_feature_list <- priority_table %>%
        distinct() %>%
        collect() %>%
        rename(descendant_concept_id = concept_id) %>%
        inner_join(ancestor_table) %>%
        filter(min_levels_of_separation > 0) %>%
        filter(!ancestor_concept_id %in% ancestor_naughty_list) %>%
        inner_join(concept %>%
                       select(concept_id, concept_name) %>%
                       rename(descendant_concept_id = concept_id, descendant_concept_name = concept_name)) %>%
        inner_join(concept %>%
                       select(concept_id, concept_name) %>%
                       rename(ancestor_concept_id = concept_id, ancestor_concept_name = concept_name))
    
    possible_rollups <- condensed_feature_list %>%
        group_by(ancestor_concept_id,ancestor_concept_name) %>%
        summarize(levels = min(min_levels_of_separation),
                  mean_ST = mean(Salient_T),
                  mean_SD = mean(Salient_A),
                  mean_SA = mean(Salient_D),
                  mean_STA = mean(Salient_TA),
                  mean_STD = mean(Salient_TD),
                  descendants = n()) %>%
        filter(levels <= max_level & levels >= min_level)
    return(possible_rollups)
}
