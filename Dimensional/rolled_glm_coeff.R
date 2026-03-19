require(dplyr)
#' A function to compute linear coefficients between outcome variables, a target variable, and a set of ancestor variables linked to that target variable.
#' @param compressed_patient_df A dataframe that includes the target variables with values of 0, 1, or NA (interpreted as 0), or alternately Boolean values of TRUE,
#' FALSE, NA (which R will parse as 0,1,NA while creating sums), and optionally a patient_count column that indicates how many patients have a matching record.
#' @param target_variable the variable to be rolled up
#' @param ancestor_variables potential ancestor variables. all variables other than the target and ancestors are assumed to be outcomes of potential interest.
#' @param rollup_bias A parameter that increases or decreases the threshold for "roughly equal". Positive values increase rollup indicator, negative values decrease rollup indicator
#' @returns A list with two objects in it. The first, $indicator, is a vector that shows whether the rolled variable shows higher coefficients (2), identical total coefficients (1), or reduced total coefficients with outcome variables (0). The second, $coeffs, is a matrix or vector with the relevant coefficients with the outcome variables. If there is only one outcome variable, the return is a vector.
#' @export
rolled_glms <- function(compressed_patient_df,target_variable,ancestor_variables,rollup_bias = 0)
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
    coeffs <- compressed_patient_df[0,] #inherits column names and structure
    #Run GLM on each outcome variable:
    for (i in 1:length(dependent_vars))
    {
        outcome_var <- dependent_vars[[i]]
        var_list <- c(outcome_var,independent_vars,"patient_count")
        compressed_again <- compressed_patient_df[var_list]
        formula_string <- paste0(outcome_var," ~ ")
        for(j in 1:length(independent_vars))
        {
            if(j != 1)
            {
                formula_string <- paste0(formula_string," + ")
            }
            formula_string <- paste0(formula_string,independent_vars[[j]])
        }
        linear_model <- glm(data = compressed_again, weights = compressed_again$patient_count,formula = as.formula(formula_string))
        new_coeffs <- data.frame(t(linear_model$coefficients))
        coeffs <- coeffs %>%
            rbind(new_coeffs)
        row.names(coeffs)[[i]] <- outcome_var
    }
    #Generate the indicator list
    target_sum_abs_cov <- sum(abs(coeffs[dependent_vars,target_variable]))
    roll_chart <- numeric(length(ancestor_variables))
    for(i in 1:length(ancestor_variables))
    {
        ancestor_sum_abs_cov <- sum(abs(coeffs[dependent_vars,ancestor_variables[[i]]]))
        combined_sum_abs_cov <- sum(abs(coeffs[dependent_vars,combined_vars[[i]]]))
        roll_chart[[i]] <- (combined_sum_abs_cov > ancestor_sum_abs_cov & combined_sum_abs_cov > target_sum_abs_cov) +
            (combined_sum_abs_cov + rollup_bias >= ancestor_sum_abs_cov & combined_sum_abs_cov + rollup_bias >= target_sum_abs_cov)
    }
    names(roll_chart) <- ancestor_variables
    #Note that the above object transforms into a vector if one of those is of dimension 1.
    return_list <- list("indicator" = roll_chart,"coeffs" = coeffs)
    return_list$coeffs[is.na(return_list$coeffs)] <- 0
    return_list$indicator[is.na(return_list$indicator)] <- 0
    return(return_list)
}
