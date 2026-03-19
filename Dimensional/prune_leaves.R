require(dplyr)
#' A function to prune descendants
#' @param leaf_table A dataframe with a collection of target leaves
#' @param test_function A function with a signature (compressed_patient_df,target_variable,ancestor_variables) and a return value including an $indicator named vector in the form used in rolled_covariances, rolled_glm_coeff - i.e., a named vector with 0 / 1 values and names that are the ancestor variables tested.
#' @param outcome_vars all preserved variables used to test outcomes (this may include treatments and outcomes)
#' @param tall_df a data frame containing two important columns: person_id and *_concept_id
#' @returns A rollup table
#' @export
prune_leaves <- function(leaf_table,
                         tall_df,
                         test_function,
                         outcome_vars,
                         indicator_threshold = 1,
                         rollup_bias = 0.001,
                         concept_str = "concept_id",
                         person_str = "person_id")
{
    concept_col <- grep(concept_str,names(tall_df))[[1]]
    person_col <- grep(person_str,names(tall_df))[[1]]
    tiny_tall <- tall_df[c(concept_col,person_col)]
    names(tiny_tall) <- c("concept_id","person_id")
    tiny_tall$value <- 1
    tiny_tall <- tiny_tall %>%
        distinct()
    #Trim outcome_vars from leaf_table:
    leaf_table <- leaf_table %>%
        filter(!(descendant_concept_id %in% outcome_vars)) %>%
        filter(!(ancestor_concept_id %in% outcome_vars))
    #Trim any extraneous variables from tall_df:
    leaf_vars <- as.character(unique(leaf_table$descendant_concept_id))
    stem_vars <- as.character(unique(leaf_table$ancestor_concept_id))
    #Trim any extraneous variables from leaf_table:
    data_vars <- unique(as.character(tiny_tall$concept_id))
    actual_leaves_present <- intersect(leaf_vars,data_vars)
    actual_stems_present <- intersect(stem_vars,data_vars)
    relevant_vars <- c(actual_leaves_present,stem_vars)
    #Pivot to wide table with only relevant vars
    tiny_tall <- tiny_tall %>%
        filter(concept_id %in% relevant_vars)
    leaf_table <- leaf_table %>%
        filter(descendant_concept_id %in% actual_leaves_present) %>%
        filter(ancestor_concept_id %in% actual_stems_present)
    actual_leaves_present_2 <- as.character(unique(leaf_table$descendant_concept_id))
    rollup_table <- data.frame(rolled_concept = character(0),to_concept = character(0))
    #Loop for now - probably priv + apply for more speed.
    for (leaf_var in actual_leaves_present_2)
    {
        ancestors <- leaf_table %>%
            filter(descendant_concept_id == leaf_var) %>%
            select(ancestor_concept_id) %>%
            unlist() %>%
            as.character()
        selected_variables <- c(leaf_var,outcome_vars,ancestors)
        #Pivot - we should have a small enough number of variables
        wide_df <- tiny_tall %>%
            filter(concept_id %in% selected_variables) %>%
            mutate(concept_id = factor(concept_id,levels = selected_variables)) %>%
            pivot_wider(names_from = concept_id,
                        values_from = value, 
                        values_fill = 0,
                        names_expand = TRUE)
        #Make tiny table:
        condensed_data <- select_targets(wide_df,selected_variables)
        #Add back zero line:
        missing_pts <- length(unique(tiny_tall$person_id)) - sum(condensed_data$patient_count)
        condensed_data <- condensed_data %>%
            ungroup() %>%
            add_case(patient_count = missing_pts)
        condensed_data[is.na(condensed_data)] <- 0
        #Now regress:
        test_values <- test_function(compressed_patient_df = condensed_data, target_variable = leaf_var, ancestor_variables = ancestors, rollup_bias = rollup_bias)
        if (max(test_values$indicator) >= indicator_threshold)
        {
            target_ancestor <- names(test_values$indicator)[[which.max(test_values$indicator)[[1]]]]
            rollup_table <- add_case(rollup_table, rolled_concept = leaf_var,to_concept = target_ancestor)
        }
    }
    
    return(rollup_table)
}
