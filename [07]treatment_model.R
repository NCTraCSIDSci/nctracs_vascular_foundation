# First feature rollup stage.
# Author(s): Tomas McIntee
# Last updated: 2025-04-17
#
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.

# Look at the feature pool. Assign weights.
# n_salient <- 200 # Each variable pool is built from three overlapping collections of this size.
set.seed(37) # Replication for training / testing split.
require(duckdb)
require(tidyverse)
if (!requireNamespace("glmnet", quietly = TRUE))
{
    install.packages("glmnet")
}
require(glmnet)
require(lubridate)
source("Dimensional/source_this_file.R")

# Source database is opened read-only. All writes go to vascular_model.
read_con  <- dbConnect(duckdb::duckdb(), dbdir = "Z:\\vascular.duckdb", read_only = TRUE)
write_con <- dbConnect(duckdb::duckdb(), dbdir = "Z:\\vascular_model.duckdb")

# Grab (unfortunately) the whole table:
wide_table_column_names <- dbListFields(read_con, "vascular_wide_training")
feature_names <- wide_table_column_names[startsWith(wide_table_column_names, "F_")]

# Adjust salient concepts to only include the short list:
arranged_priority <- read_csv("arranged_priority_reroll.csv")

# Build weights frame
weights_frame <- data.frame(concept_id = feature_names) %>%
    left_join(arranged_priority) %>%
    mutate(Salient_T = coalesce(Salient_T, 1),
        Salient_A = coalesce(Salient_A, 1),
        Salient_D = coalesce(Salient_D, 1),
        Salient_TA = coalesce(Salient_TA, 1),
        Salient_TD = coalesce(Salient_TD, 1))
weights_T <- weights_frame %>%
    select(concept_id, Salient_T) %>%
    rename(weight = Salient_T) %>%
    mutate(weight_of_weight = 1)
weights_A <- weights_frame %>%
    select(concept_id, Salient_TA) %>%
    rename(weight = Salient_TA) %>%
    mutate(weight_of_weight = 1)
weights_D <- weights_frame %>%
    select(concept_id, Salient_TD) %>%
    rename(weight = Salient_TD) %>%
    mutate(weight_of_weight = 1)
write_csv(weights_T,"weights_T.csv")
write_csv(weights_A,"weights_A.csv")
write_csv(weights_D, "weights_D.csv")

# ---------------------------------------------------------------------------

weights_A

# ---------------------------------------------------------------------------
# Treatment model MCMC loop
# ---------------------------------------------------------------------------

loop_count <- 100 # Reduced for short testing.
feature_count <- 100
if (!requireNamespace("glmnet", quietly = TRUE))
{
    install.packages("glmnet")
}
require(tidyverse)
require(glmnet)
source("Dimensional/source_this_file.R")

# Grab full wide data.
features_wide_training   <- dbReadTable(read_con, "vascular_wide_training")
features_wide_validation <- dbReadTable(read_con, "vascular_wide_validation")

coeff_log <- data.frame()

for(i in 1:loop_count)
{

    start_time = Sys.time()
    weights <- read_csv("weights_T.csv")
    subset_data <- sample_features(training_data = features_wide_training,
        validation_data = features_wide_validation,
        weights = weights,
        fixed_vars = c("Treated","Amputated","Untreated","Dead","person_id"),
        num_vars = feature_count,
        seed = i)
    training_data <- subset_data$subset_train %>%
        collect()
    try(
        {
            treat_model <- build_model(training_data,
                "Treated",
                "Dead",
                c("Treated","Amputated","Untreated","Dead"))

            # Convert the matrix to a data frame
            saved_coefs <- coef(treat_model, s = treat_model$lambda.min) %>%
                as.matrix() %>%
                as.data.frame()
            coefficients <- data.frame(concept_id = rownames(saved_coefs),
                coefficient = saved_coefs[[1]],
                weight = 1*as.numeric(saved_coefs[[1]]!= 0)) %>%
                mutate(weight_of_weight = 1, iteration = i)
            new_weights <- update_weights(weights, coefficients)
            write_csv(new_weights, "weights_T.csv")
            new_weights <- new_weights %>% mutate(iteration = i)
            coeff_log <- bind_rows(coeff_log, coefficients)
            nonzero_coeffs <- coefficients %>%
                filter(weight != 0) %>%
                nrow()
            gc()
            clock = Sys.time()-start_time
            print(paste0("\nLoop ",i," of ",loop_count," Time was ",round(clock,1), " and ",nonzero_coeffs," nonzero coefficients."))
            save(treat_model, file = "treat_model.RData")
            write_csv(coeff_log, "coeff_log_treat.csv")
        })
}

# ---------------------------------------------------------------------------
# Build one final treatment model from the basis of the weights.
# ---------------------------------------------------------------------------

feature_count <- 200
if (!requireNamespace("glmnet", quietly = TRUE))
{
    install.packages("glmnet")
}
require(tidyverse)
require(glmnet)
require(lubridate)
source("Dimensional/source_this_file.R")

# Grab full wide data.
features_wide_training   <- dbReadTable(read_con, "vascular_wide_training")
features_wide_validation <- dbReadTable(read_con, "vascular_wide_validation")

start_time = Sys.time()
weights <- read.csv("weights_T.csv") %>%
    arrange(-weight) %>%
    head(feature_count)
final_vars <- weights %>%
    pull(concept_id)

subset_data <- features_wide_training %>%
    select(any_of(c("Treated","Amputated","Untreated","Dead","person_id")), any_of(final_vars)) %>%
    collect() %>%
    arrange(person_id)

val_data <- features_wide_validation %>%
    select(any_of(c("Treated","Amputated","Untreated","Dead","person_id")), any_of(final_vars)) %>%
    collect() %>%
    arrange(person_id)

treat_model_final <- build_model(subset_data,
    "Treated",
    "Dead",
    c("Treated","Amputated","Untreated","Dead","person_id"))
coefs_matrix <- as.matrix(coef(treat_model_final, s = treat_model_final$lambda.min))
saved_coefs <- as.data.frame(coefs_matrix)
coefficients <- data.frame(concept_id = rownames(saved_coefs),
    coefficient = saved_coefs[[1]],
    weight = 1*as.numeric(saved_coefs[[1]]!= 0)) %>%
    mutate(weight_of_weight = 1, iteration = i)
write_csv(coefficients, "treatment_model_coefficients.csv")
dbWriteTable(write_con, "treatment_model_coefficients", coefficients, overwrite = TRUE)
save(treat_model_final, file = "treat_model_final.RData")
outcome_probs_trainset <- quick_apply_model(subset_data,
    treat_model_final,
    "Treated",
    nonmodel_vars = c("Amputated","Untreated","Dead","person_id"))
outcome_probs_valset <- quick_apply_model(val_data,
    treat_model_final,
    "Treated",
    nonmodel_vars = c("Amputated","Untreated","Dead","person_id"))
write_csv(outcome_probs_trainset %>% select(person_id, outcome_prob), "treat_probs_trainset.csv")
write_csv(outcome_probs_valset %>% select(person_id, outcome_prob), "treat_probs_valset.csv")
dbWriteTable(write_con, "treat_probs_trainset", outcome_probs_trainset %>% select(person_id, outcome_prob), overwrite = TRUE)
dbWriteTable(write_con, "treat_probs_valset",   outcome_probs_valset  %>% select(person_id, outcome_prob), overwrite = TRUE)

# AUC
auc_val <- basic_AUC(outcome_probs_valset$Treated, outcome_probs_valset$outcome_prob)
auc_train <- basic_AUC(outcome_probs_trainset$Treated, outcome_probs_trainset$outcome_prob)

summary_performance <- outcome_probs_valset %>%
    summarize(mean_prob = mean(outcome_prob))
summary_performance_T <- outcome_probs_trainset %>%
    summarize(mean_prob = mean(outcome_prob))

print(summary_performance)
print(summary_performance_T)
print(paste0("AUC: ",auc_val," (trainset ",auc_train,")"))

# ---------------------------------------------------------------------------

dbExecute(read_con, "CHECKPOINT")
dbExecute(read_con, "VACUUM")
dbDisconnect(read_con,  shutdown="TRUE")
dbExecute(write_con, "CHECKPOINT")
dbExecute(write_con, "VACUUM")
dbDisconnect(write_con,  shutdown="TRUE")
