#' A function to compute covariances between outcome variables, a target variable, and a set of ancestor variables linked to that target variable.
#' @param compressed_patient_df A dataframe that includes the target variables with values of 0, 1, or NA (interpreted as 0), or alternately Boolean values of TRUE, FALSE, NA (which R will parse as 0,1,NA while creating sums), and optionally a patient_count column that indicates how many patients have a matching record.
#' @param target_variable the variable to be rolled up
#' @param ancestor_variables potential ancestor variables. all variables other than the target and ancestors are assumed to be outcomes of potential interest.
#' @param rollup_bias A parameter that increases or decreases the threshold for "roughly equal". Positive values increase rollup indicator, negative values decrease rollup indicator
#' @returns A list with two objects in it. The first, $indicator, is a vector that shows whether the rolled variable shows improved total covariance (2), identical (or close enough to identical) total covariance (1), or reduced total covariance with outcome variables (0). The second, $coeff, is a matrix or vector with the relevant covariances with the outcome variables. If there is only one outcome variable, the return is a vector.
#' @export
roll_cov_glm <- function(compressed_patient_df,target_variable,ancestor_variables, rollup_bias = 0)
{
    roll_cov <- rolled_covariances(compressed_patient_df,target_variable,ancestor_variables,rollup_bias)
    roll_glm <- rolled_glm_coeff(compressed_patient_df,target_variable,ancestor_variables,rollup_bias)
    indicator <- roll_cov$indicator + roll_glm$indicator
    coeffs <- roll_cov$coeffs + roll_glm$coeffs
    results <- list("indicator" = indicator, "coeffs" = coeffs)
}
