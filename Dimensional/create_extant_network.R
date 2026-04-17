# Create extant_conditions, extant_condition_network, limb-specific condition lists

create_extant_network <- function(concept_ancestor,features_table)
{
    # Create extant_conditions, extant_condition_network, limb-specific condition lists
    # Filter for conditions that actually show up.
    extant_conditions_uncorrected <- features_table %>%
        select(concept_id, person_id) %>%
        distinct() %>%
        group_by(concept_id) %>%
        summarize(concept_count = n()) %>%
        collect()
    
    # Identify the sub-network of concept_ancestor that is potentially useful, with raw (uncombined) concept counts.
    extant_condition_network_uncorrected <- concept_ancestor %>%
        inner_join(extant_conditions_uncorrected %>%
                       rename(descendant_concept_id = concept_id, descendant_count = concept_count)) %>%
        left_join(extant_conditions_uncorrected %>%
                      rename(ancestor_concept_id = concept_id, ancestor_count = concept_count)) %>%
        distinct()
    
    # Recalculate counts based on combined count.
    extant_conditions <- extant_condition_network_uncorrected %>%
        group_by(ancestor_concept_id) %>%
        summarize(descendant_sum = sum(descendant_count)) %>% # Note that concepts are their own descendants, so this includes the base count
        rename(concept_id = ancestor_concept_id, concept_count = descendant_sum)
    
    # Regenerate the corrected extant_condition_network
    extant_condition_network <- concept_ancestor %>%
        inner_join(extant_conditions %>%
                       rename(descendant_concept_id = concept_id, descendant_count = concept_count)) %>%
        left_join(extant_conditions %>%
                      rename(ancestor_concept_id = concept_id, ancestor_count = concept_count))
    # Not working for now, just return uncorrected version, fix later.
    return(extant_condition_network_uncorrected)
}
