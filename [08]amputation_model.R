# Loop A with IPTW vector

loop_count <- 100
feature_count <- 100
if (!requireNamespace("glmnet", quietly = TRUE))
{
    install.packages("glmnet")
}
require(tidyverse)
require(duckdb)
require(glmnet)
require(lubridate)
source("Dimensional/source_this_file.R")

# Source database is opened read-only. All writes go to vascular_model.
read_con  <- dbConnect(duckdb::duckdb(), dbdir = "Z:\\vascular.duckdb", read_only = TRUE)
write_con <- dbConnect(duckdb::duckdb(), dbdir = "Z:\\vascular_model.duckdb")

# Run treatment model.
treat_probs_trainset <- read_csv("treat_probs_trainset.csv")
treat_probs_valset   <- read_csv("treat_probs_valset.csv")

train_data_T <- dbReadTable(read_con, "vascular_wide_training") %>%
    inner_join(treat_probs_trainset, by = "person_id")
load("treat_model_final.RData")
outcomes <- quick_apply_model(input_data = train_data_T,
    input_model = treat_model_final)
val_data_T <- dbReadTable(read_con, "vascular_wide_validation") %>%
    inner_join(treat_probs_valset, by = "person_id")

# Grab full wide data.
features_wide_training   <- train_data_T
features_wide_validation <- val_data_T
iptw_weights <- outcomes %>%
    inner_join(features_wide_training %>% select(person_id)) %>%
    mutate(iptw = 1/outcome_prob) %>%
    mutate(iptw = iptw/sum(iptw)) %>%
    pull(iptw)
coeff_log <- data.frame()

for(i in 1:loop_count)
{
    start_time = Sys.time()
    weights <- read.csv("weights_A.csv")
    # print('check1')
    subset_data <- sample_features(features_wide_training,
        features_wide_validation,
        weights,
        c("Treated","Amputated","Untreated","Dead","person_id"),
        feature_count,
        i)
    training_data <- subset_data$subset_train %>%
        collect() %>%
        arrange(person_id) # Match IPTW vector order

    # print(str(training_data))

    # Call refactored model builder
    amp_model <- build_advanced_model(
        training_data = training_data,
        treatment_var = "Treated",
        outcome_var = "Amputated",
        ignored_vars = c("Dead", "Untreated","person_id"),
        weight_vector = iptw_weights,
        alpha_val = 1)

    # Convert the matrix to a data frame
    if (is.null(amp_model$lambda.min))
    {
        print("error, null lambda")
        next
    }

    coefs_matrix <- as.matrix(coef(amp_model, s = amp_model$lambda.min))
    saved_coefs <- as.data.frame(coefs_matrix)
    coefficients <- data.frame(concept_id = rownames(saved_coefs),
        coefficient = saved_coefs[[1]],
        weight = 1*as.numeric(saved_coefs[[1]]!= 0)) %>%
        mutate(weight_of_weight = 1, iteration = i)
    new_weights <- update_weights(weights, coefficients, stripped_prefix = "treated_x_")
    write.csv(new_weights, "weights_A.csv")
    coeff_log <- bind_rows(coeff_log, coefficients)
    nonzero_coeffs <- coefficients %>%
        filter(weight != 0) %>%
        nrow()
    gc()
    clock = Sys.time()-start_time
    print(paste0("\nLoop ",i," of ",loop_count," Time was ",round(clock,1), " and ",nonzero_coeffs," nonzero coefficients."))
}
save(amp_model, file = "amp_model.RData")
write.csv(coeff_log, "coeff_log_amp.csv")

# ---------------------------------------------------------------------------

str(features_wide_training)

# ---------------------------------------------------------------------------

features_wide_training %>% select(person_id) %>% summary()

# ---------------------------------------------------------------------------

iptw_weights <- outcomes %>%
    inner_join(features_wide_training %>% select(person_id)) %>%
    mutate(iptw = 1/outcome_prob) %>%
    mutate(iptw = iptw/sum(iptw)) %>%
    pull(iptw)
length(unique(outcomes$person_id)) - length(outcomes$person_id)
length(unique(treat_probs_trainset$person_id)) - length(treat_probs_trainset$person_id)

# ---------------------------------------------------------------------------
# Non-loop
# ---------------------------------------------------------------------------

feature_count <- 100
top_alpha <- 1
if (!requireNamespace("glmnet", quietly = TRUE))
{
    install.packages("glmnet")
}
require(tidyverse)
require(glmnet)
require(lubridate)
source("Dimensional/source_this_file.R")

# Run treatment model.
treat_probs_trainset <- read_csv("treat_probs_trainset.csv")
treat_probs_valset   <- read_csv("treat_probs_valset.csv")

train_data_T <- dbReadTable(read_con, "vascular_wide_training") %>%
    inner_join(treat_probs_trainset, by = "person_id")
load("treat_model_final.RData")
outcomes <- quick_apply_model(input_data = train_data_T,
    input_model = treat_model_final)
val_data_T <- dbReadTable(read_con, "vascular_wide_validation") %>%
    inner_join(treat_probs_valset, by = "person_id")

# Grab full wide data.
features_wide_training   <- train_data_T
features_wide_validation <- val_data_T
iptw_weights <- outcomes %>%
    inner_join(features_wide_training %>% select(person_id)) %>%
    mutate(iptw = 1/outcome_prob) %>%
    mutate(iptw = iptw/sum(iptw)) %>%
    pull(iptw)

weights <- read.csv("weights_A.csv") %>%
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

gc()
print("Setup complete, calling model build.")
# Call refactored model builder
amp_model_final <- build_advanced_model(
    training_data = subset_data,
    treatment_var = "Treated",
    outcome_var = "Amputated",
    ignored_vars = c("Dead", "Untreated","person_id","Amputated"),
    weight_vector = iptw_weights,
    alpha_val = top_alpha)

gc()
print("Model build complete")

# Get results
outcome_probs_trainset <- quick_apply_model(subset_data,
    amp_model_final,
    prefix = "F_",
    nonmodel_vars = c("Amputated","Untreated","Dead","person_id"))
outcome_probs_valset <- quick_apply_model(val_data,
    amp_model_final,
    prefix = "F_",
    nonmodel_vars = c("Amputated","Untreated","Dead","person_id"))

write_csv(outcome_probs_trainset, "amp_probs_trainset.csv")
write_csv(outcome_probs_valset,   "amp_probs_valset.csv")
dbWriteTable(write_con, "amp_probs_trainset", outcome_probs_trainset %>% select(person_id, outcome_prob), overwrite = TRUE)
dbWriteTable(write_con, "amp_probs_valset",   outcome_probs_valset   %>% select(person_id, outcome_prob), overwrite = TRUE)
save(amp_model_final, file = "amp_model_final.RData")

# ---------------------------------------------------------------------------
# Performance
# ---------------------------------------------------------------------------

require(tidyverse)
source("Dimensional/source_this_file.R")

amp_probs   <- read_csv("amp_probs_valset.csv")
amp_probs_T <- read_csv("amp_probs_trainset.csv")

auc_val   <- basic_AUC(amp_probs$Amputated,   amp_probs$outcome_prob)
auc_train <- basic_AUC(amp_probs_T$Amputated, amp_probs_T$outcome_prob)
amp_probs <- read_csv("amp_probs_valset.csv")
summary_performance <- amp_probs %>%
    group_by(Treated, Amputated) %>%
    summarize(mean_prob = mean(outcome_prob))

amp_probs_T <- read_csv("amp_probs_trainset.csv")
summary_performance_T <- amp_probs_T %>%
    group_by(Treated, Amputated) %>%
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