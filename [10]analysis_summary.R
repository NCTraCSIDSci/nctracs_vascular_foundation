# Some post analysis
# Author(s): Tomas McIntee
# Last updated: 2025-04-17
#
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.

# Examination of variables

require(duckdb)
require(tidyverse)

# Source database opened read-only. No writes to vascular_model in this script.
read_con <- dbConnect(duckdb::duckdb(), dbdir = "Z:\\vascular.duckdb", read_only = TRUE)

# ---------------------------------------------------------------------------

load("amp_model_final.RData")
# Find the index of the lambda closest to lambda.min
# Get the lambda vector and find the closest index
lambda_vec <- amp_model_final$glmnet.fit$lambda
lambda_index <- which.min(abs(lambda_vec - amp_model_final$lambda.min))

# Extract sparse matrix of coefficients for that lambda
sparse_beta <- amp_model_final$glmnet.fit$beta[, lambda_index]

# Convert to dense matrix and get nonzero entries
coefs_matrix <- as.matrix(sparse_beta)
saved_coefs <- as.data.frame(coefs_matrix)
coefficients_amp <- data.frame(concept_id = rownames(saved_coefs), weight = saved_coefs[[1]])

# Filter nonzero coefficients
nonzero_coeffs_amp <- coefficients_amp %>%
    filter(weight != 0) %>%
    nrow()
print(nonzero_coeffs_amp)

load("death_model_final.RData")
# Find the index of the lambda closest to lambda.min
# Get the lambda vector and find the closest index
lambda_vec <- death_model_final$glmnet.fit$lambda
lambda_index <- which.min(abs(lambda_vec - death_model_final$lambda.min))

# Extract sparse matrix of coefficients for that lambda
sparse_beta <- death_model_final$glmnet.fit$beta[, lambda_index]

# Convert to dense matrix and get nonzero entries
coefs_matrix <- as.matrix(sparse_beta)
saved_coefs <- as.data.frame(coefs_matrix)
coefficients_death <- data.frame(concept_id = rownames(saved_coefs), weight = saved_coefs[[1]])

# Filter nonzero coefficients
nonzero_coeffs_death <- coefficients_death %>%
    filter(weight != 0) %>%
    nrow()
print(nonzero_coeffs)

# ---------------------------------------------------------------------------

load("treat_model_final.RData")
# Find the index of the lambda closest to lambda.min
# Get the lambda vector and find the closest index
lambda_vec <- treat_model_final$glmnet.fit$lambda
lambda_index <- which.min(abs(lambda_vec - treat_model_final$lambda.min))

# Extract sparse matrix of coefficients for that lambda
sparse_beta <- treat_model_final$glmnet.fit$beta[, lambda_index]

# Convert to dense matrix and get nonzero entries
coefs_matrix <- as.matrix(sparse_beta)
saved_coefs <- as.data.frame(coefs_matrix)
coefficients_treat <- data.frame(concept_id = rownames(saved_coefs), weight = saved_coefs[[1]])

# Filter nonzero coefficients
nonzero_coeffs_treat <- coefficients_treat %>%
    filter(weight != 0) %>%
    nrow()
print(nonzero_coeffs)

# ---------------------------------------------------------------------------

str(amp_model_final)

# ---------------------------------------------------------------------------

#dbGetQuery(read_con, "SELECT * FROM vascular_feature_supplement LIMIT 10")

# ---------------------------------------------------------------------------

require(ggpubr)
require(tidyverse)
file_prefix <- ""
weights_T <- read.csv(paste0(file_prefix, "weights_T.csv"))
weights_A <- read.csv(paste0(file_prefix, "weights_A.csv"))
weights_D <- read.csv(paste0(file_prefix, "weights_D.csv"))
g1 <- ggplot(weights_T, aes(x = weight))+
    geom_histogram()+
    ggtitle("Treatment weights")
g2 <- ggplot(weights_A, aes(x = weight))+
    geom_histogram()+
    ggtitle("Amputation weights")
g3 <- ggplot(weights_D, aes(x = weight))+
    geom_histogram()+
    ggtitle("Death weights")
print(g1)

# ---------------------------------------------------------------------------

print(g2)

# ---------------------------------------------------------------------------

print(g3)

# ---------------------------------------------------------------------------

getwd()

# ---------------------------------------------------------------------------

require(ggpubr)
require(tidyverse)
file_prefix <- ""
coeff_T <- read.csv(paste0(file_prefix, "coeff_log_treat.csv"))
coeff_A <- read.csv(paste0(file_prefix, "coeff_log_amp.csv"))
coeff_D <- read.csv(paste0(file_prefix, "coeff_log_death.csv"))
g1 <- ggplot(coeff_T, aes(x = weight))+
    geom_density()+
    ggtitle("Treatment coeffs")
g2 <- ggplot(coeff_A, aes(x = weight))+
    geom_density()+
    ggtitle("Amputation coeffs")
g3 <- ggplot(coeff_D, aes(x = weight))+
    geom_density()+
    ggtitle("Death coeffs")

# ---------------------------------------------------------------------------

g1

# ---------------------------------------------------------------------------

g2

# ---------------------------------------------------------------------------

g3

# ---------------------------------------------------------------------------

source("Dimensional/source_this_file.R")
require(tidyverse)

# Name and handle interaction variables
concepts_modified <- concept %>%
    mutate(root_concept_id = concept_id,
        concept_id = paste0("treated_x_",concept_id),
        concept_name = paste0("Interaction of treatment and ",concept_name)) %>%
    bind_rows(concept %>% mutate(root_concept_id = concept_id)) %>%
    select(concept_id,concept_name,root_concept_id)

# Combine with coefficient data:
coeff_T_named <- coeff_T %>%
    left_join(concepts_modified) %>%
    mutate(root_concept_id = coalesce(root_concept_id,concept_id)) %>%
    mutate(root_concept_id = str_replace(root_concept_id,"treated_x_","")) %>%
    group_by(concept_id,concept_name,root_concept_id) %>%
    summarize(median_coefficient = median(weight),
        mean_coefficient = mean(weight))
coeff_A_named <- coeff_A %>%
    left_join(concepts_modified) %>%
    mutate(root_concept_id = coalesce(root_concept_id,concept_id)) %>%
    mutate(root_concept_id = str_replace(root_concept_id,"treated_x_","")) %>%
    group_by(concept_id,concept_name,root_concept_id) %>%
    summarize(median_coefficient = median(weight),
        mean_coefficient = mean(weight))
coeff_D_named <- coeff_D %>%
    left_join(concepts_modified) %>%
    mutate(root_concept_id = coalesce(root_concept_id,concept_id)) %>%
    mutate(root_concept_id = str_replace(root_concept_id,"treated_x_","")) %>%
    group_by(concept_id,concept_name,root_concept_id) %>%
    summarize(median_coefficient = median(weight),
        mean_coefficient = mean(weight))

# Bind together
coeff_T_interactions <- coeff_T_named %>%
    filter(root_concept_id != concept_id) %>%
    rename(median_interaction = median_coefficient,
        mean_interaction = mean_coefficient,
        interaction_variable = concept_name,
        interaction_id = concept_id)
coeff_T_named_corrected <- coeff_T_named %>%
    filter(root_concept_id == concept_id) %>%
    full_join(coeff_T_interactions) %>%
    mutate(concept_name = coalesce(concept_name,concept_id)) %>%
    select(concept_name,
        median_coefficient,
        mean_coefficient,
        interaction_variable,
        median_interaction,
        mean_interaction)
coeff_A_interactions <- coeff_A_named %>%
    filter(root_concept_id != concept_id) %>%
    rename(median_interaction = median_coefficient,
        mean_interaction = mean_coefficient,
        interaction_variable = concept_name,
        interaction_id = concept_id)
coeff_A_named_corrected <- coeff_A_named %>%
    filter(root_concept_id == concept_id) %>%
    full_join(coeff_A_interactions) %>%
    mutate(concept_name = coalesce(concept_name,concept_id)) %>%
    select(concept_name,
        median_coefficient,
        mean_coefficient,
        interaction_variable,
        median_interaction,
        mean_interaction)
coeff_D_interactions <- coeff_D_named %>%
    filter(root_concept_id != concept_id) %>%
    rename(median_interaction = median_coefficient,
        mean_interaction = mean_coefficient,
        interaction_variable = concept_name,
        interaction_id = concept_id)
coeff_D_named_corrected <- coeff_D_named %>%
    filter(root_concept_id == concept_id) %>%
    full_join(coeff_D_interactions) %>%
    mutate(concept_name = coalesce(concept_name,concept_id)) %>%
    select(concept_name,
        median_coefficient,
        mean_coefficient,
        interaction_variable,
        median_interaction,
        mean_interaction)


write_csv(coeff_T_named_corrected,"coeff_T_named.csv")
write_csv(coeff_A_named_corrected,"coeff_A_named.csv")
write_csv(coeff_D_named_corrected,"coeff_D_named.csv")

write_csv(coefficients_amp,"coefficients_final_model_amp.csv")
write_csv(coefficients_death,"coefficients_final_model_death.csv")
write_csv(coefficients_treat,"coefficients_final_model_treat.csv")

# ---------------------------------------------------------------------------
dbExecute(read_con, "CHECKPOINT")
dbExecute(read_con, "VACUUM")
dbDisconnect(read_con,  shutdown="TRUE")
