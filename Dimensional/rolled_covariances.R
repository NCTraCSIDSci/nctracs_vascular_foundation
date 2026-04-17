require(dplyr)
#' A function to compute covariances between outcome variables, a target variable, and a set of ancestor variables linked to that target variable.
#' @param compressed_patient_df A dataframe that includes the target variables with values of 0, 1, or NA (interpreted as 0), or alternately Boolean values of TRUE, FALSE, NA (which R will parse as 0,1,NA while creating sums), and optionally a patient_count column that indicates how many patients have a matching record.
#' @param target_variable the variable to be rolled up
#' @param ancestor_variables potential ancestor variables. all variables other than the target and ancestors are assumed to be outcomes of potential interest.
#' @param rollup_bias A parameter that increases or decreases the threshold for "roughly equal". Positive values increase rollup indicator, negative values decrease rollup indicator
#' @returns A list with two objects in it. The first, $indicator, is a vector that shows whether the rolled variable shows improved total covariance (2), identical (or close enough to identical) total covariance (1), or reduced total covariance with outcome variables (0). The second, $coeff, is a matrix or vector with the relevant covariances with the outcome variables. If there is only one outcome variable, the return is a vector.
#' @export
rolled_covariances <- function(compressed_patient_df,target_variable,ancestor_variables, rollup_bias = 0)
{
    if (!("patient_count" %in% names(compressed_patient_df)))
    {
        compressed_patient_df$patient_count <- 1
    }
    combined_vars <- ancestor_variables
    for(i in 1:length(ancestor_variables))
    {
        combined_var <- paste0(ancestor_variables[[i]],"_w_",target_variable)
        combined_vars[[i]] <- combined_var
        combined_vec <- pmax(compressed_patient_df[[ancestor_variables[[i]]]],compressed_patient_df[[target_variable]])
        compressed_patient_df[[combined_var]] <- combined_vec
    }
    independent_vars <- c(target_variable,ancestor_variables, combined_vars)
    dependent_vars <- names(compressed_patient_df) %>%
        setdiff(independent_vars) %>%
        setdiff("patient_count")
    covariance_matrix <- cov.wt(compressed_patient_df[names(compressed_patient_df)!="patient_count"],compressed_patient_df$patient_count)$cov
    covariance_matrix[is.na(covariance_matrix)] <- 0
    covariance_matrix[is.nan(covariance_matrix)] <- 0
    #Build
    target_sum_abs_cov <- sum(abs(covariance_matrix[target_variable,dependent_vars]))
    roll_chart <- numeric(length(ancestor_variables))
    for(i in 1:length(ancestor_variables))
    {
        ancestor_sum_abs_cov <- sum(abs(covariance_matrix[ancestor_variables[[i]],dependent_vars]))
        combined_sum_abs_cov <- sum(abs(covariance_matrix[combined_vars[[i]],dependent_vars]))
        roll_chart[[i]] <- (combined_sum_abs_cov > ancestor_sum_abs_cov & combined_sum_abs_cov > target_sum_abs_cov)+
            (combined_sum_abs_cov + rollup_bias >= ancestor_sum_abs_cov & combined_sum_abs_cov + rollup_bias >= target_sum_abs_cov)
    }
    names(roll_chart) <- ancestor_variables
    covariance_targets <- covariance_matrix[dependent_vars,independent_vars]
    #Note that the above object transforms into a vector if one of those is of dimension 1.
    return_list <- list("indicator" = roll_chart,"coeffs" = covariance_targets)
    return(return_list)
}
