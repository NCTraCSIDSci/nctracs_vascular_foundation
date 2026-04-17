build_model <- function(training_data,
                        treatment_var,
                        outcome_var,
                        all_extra_vars,
                        id_col = "person_id",
                        intermediate_outcome_models = FALSE,
                        model_family = "binomial")
{
    # Create training subset
    training_data_snip <- training_data %>%
        select(-any_of(all_extra_vars)) %>%
        mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
    
    
    #Sparse data
    df_sparse <- as(as.matrix(training_data_snip[ , !names(training_data_snip) %in% c(id_col)]), "dgCMatrix")
    
    # Fit model
    treatment_model <- cv.glmnet(df_sparse, training_data[[treatment_var]], family = model_family)
    
    # Prediction matrices
    newx <- as.matrix(training_data_snip[ , !names(training_data_snip) %in% id_col])
    if(intermediate_outcome_models)
    {
        # Training subsets
        training_data_treated <- training_data %>%
            filter(.data[[treatment_var]] == 1)
        training_data_untreated <- training_data %>%
            filter(.data[[treatment_var]] == 0)
        
        #Sparsify
        treated_sparse <- as(as.matrix(training_data_treated[ , !names(training_data_treated) %in% c(id_col, all_extra_vars)]), "dgCMatrix")
        untreated_sparse <- as(as.matrix(training_data_untreated[ , !names(training_data_untreated) %in% c(id_col, all_extra_vars)]), "dgCMatrix")
        
        # Predict outcome for treated/untreated groups
        
        outcome_untreated_model <- cv.glmnet(untreated_sparse, training_data_untreated[[outcome_var]], family = model_family)
        outcome_treated_model <- cv.glmnet(treated_sparse, training_data_treated[[outcome_var]], family = model_family)
        
        newx2 <- as.matrix(training_data_untreated[ , !names(training_data_untreated) %in% c(id_col, all_extra_vars)])
        newx3 <- as.matrix(training_data_treated[ , !names(training_data_treated) %in% c(id_col, all_extra_vars)])
        
        #outcome_probs_untreated <- training_data_untreated %>%
        #	mutate(outcome_prob = predict(outcome_untreated_model, newx = newx2, type = "response", s = outcome_untreated_model$lambda.min)) %>%
        #	select(person_id, outcome_prob)
        
        #outcome_probs_treated <- training_data_treated %>%
        #	mutate(outcome_prob = predict(outcome_treated_model, newx = newx3, type = "response", s = outcome_treated_model$lambda.min)) %>%
        #	select(person_id, outcome_prob)
        return_obj = list(treatment_model = treatment_model,
                          treated_outcome = outcome_treated_model,
                          untreated_outcome = outcome_untreated_model,
                          #	probs = features_w_treatment_probs,
                          #	probs_ot = outcome_probs_treated,
                          #	prob_ou = outcome_probs_untreated
        )
    }
    else
    {
        return_obj = treatment_model
    }
    return(return_obj)
}

build_advanced_model <- function(training_data,
                                 outcome_var,
                                 ignored_vars,
                                 weight_vector = NULL,
                                 treatment_var = "Treated",
                                 id_col = "person_id",
                                 prefix = "F_",
                                 fold_count = 5,
                                 alpha_val = 0.025,
                                 seed = 37)
{
    # 1. Interaction terms for predictors
    training_double <- training_data %>%
        mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
        mutate(across(starts_with(prefix), ~ .data[[treatment_var]] * ., .names = "treated_x_{.col}"))
    
    training_double_snip <- training_double %>%
        mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
        select(-any_of(c(outcome_var,ignored_vars)))
    
    # 2. Prepare sparse matrices
    sparse_train <- as(as.matrix(training_double_snip[, !names(training_double_snip) %in% id_col]), "dgCMatrix")
    
    # 3. Reproducible folds
    set.seed(seed)
    foldId <- sample(rep(seq(fold_count), length.out = nrow(sparse_train)))
    
    # 4. Train model
    advance_model <- cv.glmnet(
        sparse_train,
        training_double[[outcome_var]],
        family = "binomial",
        weights = weight_vector,
        nfolds = fold_count,
        foldid = foldId,
        alpha = alpha_val
    )
    return(advance_model)
}
# Helpers for MCMC. Refactor into separate file later.
update_weights <- function(new_weights, old_weights, stripped_prefix = NULL)
{
    if(!is.null(stripped_prefix))
    {
        new_weights <- new_weights %>%
            mutate(concept_id = str_replace(concept_id,stripped_prefix,""))
    }
    adjusted_weights <- old_weights %>%
        bind_rows(new_weights) %>%
        group_by(concept_id) %>%
        summarize(weight = sum(abs(weight) * weight_of_weight)/sum(weight_of_weight),
                  weight_of_weight = sum(weight_of_weight)) %>%
        arrange(concept_id)
    return(adjusted_weights)
}
sample_features <- function(training_data, validation_data, weights, fixed_vars, num_vars, seed)
{
    set.seed(seed)
    # Check that weights and list of variables are identical
    training_vars <- data.frame(concept_id = as.character(tbl_vars(training_data)))
    weight_vars <- weights %>%
        inner_join(training_vars, by = "concept_id") %>%
        arrange(concept_id)
    base_vars <- weight_vars %>%
        pull(concept_id)
    weights_fixed <- weight_vars %>%
        pull(weight) %>%
        as.numeric()
    
    # Normalize weights to sum to 1
    weights_fixed <- weights_fixed / sum(weights_fixed)
    
    # Weighted sampling
    sampled_vars <- sample(base_vars, size = num_vars, prob = weights_fixed, replace = FALSE)
    training_data_modified <- training_data %>%
        select(any_of(c(sampled_vars,fixed_vars)))
    validation_data_modified <- validation_data %>%
        select(any_of(c(sampled_vars,fixed_vars)))
    return_list <- list(subset_train = training_data_modified, subset_validation = validation_data_modified)
    return(return_list)
}
quick_apply_model <- function(input_data,
                              input_model,
                              prefix = NULL,
                              treatment_var = "Treated",
                              nonmodel_vars = c("Amputated","Dead","Untreated","person_id"))
{
    # Generate interaction terms if needed
    if (!is.null(prefix)) {
        treat_val <- input_data[[treatment_var]]
        input_data <- input_data %>%
            mutate(across(starts_with(prefix), ~ treat_val * ., .names = "treated_x_{.col}"))
    }
    
    # Extract model variables AFTER generating terms
    coefs <- coef(input_model, s = input_model$lambda.min) %>% as.matrix() %>% as.data.frame()
    model_vars <- row.names(coefs)[-1]
    
    df_sparse <- input_data %>%
        select(any_of(nonmodel_vars),all_of(model_vars),treatment_var)
    
    if(inherits(df_sparse, "tbl_spark"))
    {
        df_sparse <- df_sparse %>%
            collect()
    }
    mat_sparse <- df_sparse %>%
        select(model_vars) %>%
        mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
        as.matrix() %>%
        as("dgCMatrix")
    
    # Predict
    outcome_probs <- df_sparse %>%
        select(any_of(nonmodel_vars),treatment_var) %>%
        mutate(outcome_prob = as.numeric(predict(input_model, newx = mat_sparse, type = "response", s = input_model$lambda.min)))
    
    return(outcome_probs)
}

basic_AUC <- function(labels, scores)
{
    df <- data.frame(label = labels, score = scores) %>% arrange(desc(score))
    df <- df %>%
        mutate(tp = cumsum(label == 1), fp = cumsum(label == 0),
               fn = sum(label == 1) - tp, tn = sum(label == 0) - fp)
    tpr <- df$tp / (df$tp + df$fn)
    fpr <- df$fp / (df$fp + df$tn)
    auc <- sum(diff(c(0, fpr)) * (tpr[-1] + tpr[-length(tpr)]) / 2)
    return(auc)
}
qof_model <- function(input_data,
                      input_model,
                      outcome_var,
                      treatment_var = "Treated",
                      prefix = "F_",
                      nonmodel_vars = c("Amputated","Untreated","Dead","person_id"))
{
    outcomes <- quick_apply_model(input_data = input_data,
                                  input_model = input_model,
                                  treatment_var = treatment_var,
                                  prefix = prefix,
                                  nonmodel_vars = nonmodel_vars)
    labels <- outcomes %>% pull(outcome_var)
    scores <- outcomes %>% pull(outcome_prob)
    AUC <- basic_AUC(labels,scores)
    return(AUC-0.5)
}
