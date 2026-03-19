#' A function to identify leaf nodes of the directed concept ancestor graph (i.e., descendants that are not ancestors).
#' @param ancestor_table A dataframe with the format of OMOP concept_ancestor:
#' Mandatory columns: "ancestor_concept_id","descendant_concept_id","min_levels_of_separation"
#' @returns A subset of the ancestor_table that only contains descendant nodes and their immediate ancestors.
#' @export
find_leaves <- function(ancestor_table)
{
    ancestor_table <- ancestor_table %>%
        filter(min_levels_of_separation > 0)
    descendants <- unique(ancestor_table$descendant_concept_id)
    ancestors <- unique(ancestor_table$ancestor_concept_id)
    leaves <- descendants[!descendants %in% ancestors]
    filter_leaf <- ancestor_table$descendant_concept_id %in% leaves
    filter_immediate <- ancestor_table$min_levels_of_separation == 1
    combined_filter <- filter_leaf & filter_immediate
    leaf_table <- ancestor_table[combined_filter,]
    return(leaf_table)
}
