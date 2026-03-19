require(dplyr)
#' A convenience function that selects and compresses a patient frame based on a set of target variables.
#' @param compressed_patient_df A dataframe that includes the target variables and optionally a patient_count column that indicates how many patients have a matching record. Patient counts of NA are interpreted as 0.
#' @param target_variables Column names (must be strings)
#' @param begin_tall If TRUE, expects a "tall" data frame with concept_id and person_id columns. If FALSE, expects a wide dataframe with concepts as columns.
#' @param If TRUE (default), converts values of NA / NaN to 0 before aggregating the frame.
#' @returns A compressed binary patient frame that only includes the target variables and a patient_count column. It will have at most one row per unique combination of target variables.
#' @export

select_targets <- function(compressed_patient_df,
                           target_variables,
                           begin_tall,
                           na.rm = TRUE)
{
    if(begin_tall)
    {
        tall_filtered <- compressed_patient_df %>%
            filter(concept_id %in% target_variables) %>%
            select(concept_id,person_id) %>%
            distinct()
        if (inherits(compressed_patient_df, "tbl_spark"))
        {
            # Pivot the data
            features_pivoted <- tall_filtered %>%
                sdf_pivot(person_id ~ concept_id)
            
            # Mutate the data
            features_mutated <- features_pivoted %>%
                mutate(across(-person_id, ~ ifelse(is.na(.), 0, 1)))
            
            # Collect the data
            features_wide_full <- features_mutated %>%
                collect()
        }
        else
        {
            features_wide_full <- tall_filtered %>%
                mutate(presence = 1) %>%
                pivot_wider(id_cols = person_id,
                            names_from = concept_id,
                            values_from = presence,
                            values_fill = 0)
        }
        
        
        return_df <- features_wide_full %>%
            group_by(across(all_of(target_variables))) %>%
            summarize(patient_count = n(), .groups = "drop")
    }
    else
    {
        return_df <- compressed_patient_df %>%
            select(any_of(target_variables),patient_count)
        if (!("patient_count" %in% tbl_vars(compressed_patient_df)))
        {
            return_df <- compressed_patient_df %>%
                mutate(patient_count = 1)
        }
        return_df <- compressed_patient_df %>%
            group_by(across(any_of(target_variables))) %>%
            summarize(patient_count = sum(patient_count), .groups = "drop")
    }
    if (na.rm)
    {
        return_df <- return_df %>% replace_na(list(patient_count = 0))
    }
    return(return_df)
}
