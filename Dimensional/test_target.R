test_target <- function(proposed_target,
                        features_min_trimmed,
                        ancestor_table,
                        addl_vars = c("Dead","Treated","Untreated"),
                        treatment_var = "Treated",
                        outcome_var = "Dead",
                        tech = "glm",
                        max_desc = 20)
{
    gc()
    start_time <- Sys.time()
    # Feature manipulation section
    descendant_pool <- ancestor_table %>%
        filter(ancestor_concept_id == proposed_target) %>%
        select(ancestor_concept_id,descendant_concept_id)
    
    #Check for short return:
    if(nrow(descendant_pool) <= 1 | nrow(descendant_pool) > max_desc)
    {
        qofl_df <- data.frame(target = proposed_target, qofl_T = NA, qofl_U = NA)
        # Print results to verify
        print(qofl_df)
        return(qofl_df)
    }
    
    # print(paste0("Break 1: ",Sys.time() - start_time))
    start_time <- Sys.time()
    
    # Create variable lists
    base_target_variables <- descendant_pool %>%
        pull(descendant_concept_id)
    target_variables <- c(base_target_variables,addl_vars)
    target_variables_alt <- c(proposed_target,addl_vars)
    regress_variables_base <- descendant_pool %>%
        rename(concept_id = descendant_concept_id)
    regress_variables <- regress_variables_base %>%
        pull(concept_id)
    regress_variables_w_T <- c(regress_variables, treatment_var)
    
    # print(paste0("Break 2: ",Sys.time() - start_time))
    start_time <- Sys.time()
    
    # Apply feature selection to data:
    features_target <- features_min_trimmed %>%
        select_targets(target_variables, begin_tall = TRUE)
    print(paste0("Break 3: ",Sys.time() - start_time))
    start_time <- Sys.time()
    
    #Features target is already in columns, need to consolidate columns instead of doing this.
    rerolled_features <- features_target %>%
        mutate(!!sym(proposed_target) := pmax(!!!syms(base_target_variables))) %>%
        group_by(!!sym(proposed_target),!!!syms(addl_vars)) %>%
        summarize(patient_count = sum(patient_count), .groups = "drop")
    
    treat_formula <- as.formula(paste(treatment_var," ~", paste(base_target_variables, collapse = " + ")))
    treat_formula_alt <- as.formula(paste(treatment_var," ~",proposed_target))
    dead_formula <- as.formula(paste(outcome_var, " ~", paste(c(base_target_variables,treatment_var), collapse = " + ")))
    dead_formula_alt <- as.formula(paste(outcome_var, " ~", paste(c(proposed_target,treatment_var), collapse = " + ")))
    # print(paste0("Break 4: ",Sys.time() - start_time))
    start_time <- Sys.time()
    
    # Extract weights
    weights <- features_target$patient_count
    weights1 <- rerolled_features$patient_count
    # Train models:
    if(tech == "glmnet")
    {
        # Design matrices for original features
        x_T <- sparse.model.matrix(treat_formula, data = features_target)
        y_T <- features_target[[treatment_var]]
        
        x_U <- sparse.model.matrix(dead_formula, data = features_target)
        y_U <- features_target[[outcome_var]]
        
        # Design matrices for rerolled (simplified) features
        x_T1 <- sparse.model.matrix(treat_formula_alt, data = rerolled_features)
        y_T1 <- rerolled_features[[treatment_var]]
        
        x_U1 <- sparse.model.matrix(dead_formula_alt, data = rerolled_features)
        y_U1 <- rerolled_features[[outcome_var]]
        
        
        # Train models with glmnet (logistic regression)
        target_model_T  <- glmnet(x_T, y_T, family = "binomial", weights = weights)
        target_model_U  <- glmnet(x_U, y_U, family = "binomial", weights = weights)
        target_model_T1 <- glmnet(x_T1, y_T1, family = "binomial", weights = weights1)
        target_model_U1 <- glmnet(x_U1, y_U1, family = "binomial", weights = weights1)
        
        # Get enriched
        enriched_model_T  <- list(model = target_model_T,  data = list(x = x_T,  y = y_T),  weight = weights, label = treatment_var, formula = treat_formula, lambda = target_model_T$lambda[1])
        enriched_model_U  <- list(model = target_model_U,  data = list(x = x_U,  y = y_U),  weight = weights, label = outcome_var,  formula = dead_formula, lambda = target_model_U$lambda[1])
        enriched_model_T1 <- list(model = target_model_T1, data = list(x = x_T1, y = y_T1), weight = weights1, label = treatment_var, formula = treat_formula_alt, lambda = target_model_T1$lambda[1])
        enriched_model_U1 <- list(model = target_model_U1, data = list(x = x_U1, y = y_U1), weight = weights1, label = outcome_var,  formula = dead_formula_alt, lambda = target_model_U1$lambda[1])
        
    }
    else if(tech == "cv.glmnet")
    {
        # Design matrices for original features
        x_T <- sparse.model.matrix(treat_formula, data = features_target)
        y_T <- features_target[[treatment_var]]
        
        x_U <- sparse.model.matrix(dead_formula, data = features_target)
        y_U <- features_target[[outcome_var]]
        
        # Design matrices for rerolled (simplified) features
        x_T1 <- sparse.model.matrix(treat_formula_alt, data = rerolled_features)
        y_T1 <- rerolled_features[[treatment_var]]
        
        x_U1 <- sparse.model.matrix(dead_formula_alt, data = rerolled_features)
        y_U1 <- rerolled_features[[outcome_var]]
        
        # Extract weights
        weights <- features_target$patient_count
        weights1 <- rerolled_features$patient_count
        
        # Train models with cv.glmnet (logistic regression)
        target_model_T  <- cv.glmnet(x_T, y_T, family = "binomial", weights = weights)
        target_model_U  <- cv.glmnet(x_U, y_U, family = "binomial", weights = weights)
        target_model_T1 <- cv.glmnet(x_T1, y_T1, family = "binomial", weights = weights1)
        target_model_U1 <- cv.glmnet(x_U1, y_U1, family = "binomial", weights = weights1)
        
        # Get enriched
        enriched_model_T  <- list(model = target_model_T,  data = list(x = x_T,  y = y_T),  weight = weights, label = treatment_var, formula = treat_formula, lambda = target_model_T$lambda.min)
        enriched_model_U  <- list(model = target_model_U,  data = list(x = x_U,  y = y_U),  weight = weights, label = outcome_var,  formula = dead_formula, lambda = target_model_U$lambda.min)
        enriched_model_T1 <- list(model = target_model_T1, data = list(x = x_T1, y = y_T1), weight = weights1, label = treatment_var, formula = treat_formula_alt, lambda = target_model_T1$lambda.min)
        enriched_model_U1 <- list(model = target_model_U1, data = list(x = x_U1, y = y_U1), weight = weights1, label = outcome_var,  formula = dead_formula_alt, lambda = target_model_U1$lambda.min)
    }
    else # Default to "glm"
    {
        target_model_T <- glm(treat_formula,
                              data = features_target,
                              weights = features_target$patient_count,
                              family = binomial())
        
        target_model_U <- glm(dead_formula,
                              data = features_target,
                              weights = features_target$patient_count,
                              family = binomial())
        
        target_model_T1 <- glm(treat_formula_alt,
                               data = rerolled_features,
                               weights = rerolled_features$patient_count,
                               family = binomial())
        
        target_model_U1 <- glm(dead_formula_alt,
                               data = rerolled_features,
                               weights = rerolled_features$patient_count,
                               family = binomial())
        
        # Get enriched
        enriched_model_T  <- list(model = target_model_T,  data = features_target,  weight = weights, label = treatment_var, formula = treat_formula)
        enriched_model_U  <- list(model = target_model_U,  data = features_target,  weight = weights, label = outcome_var,  formula = dead_formula)
        enriched_model_T1 <- list(model = target_model_T1, data = rerolled_features, weight = weights1, label = treatment_var, formula = treat_formula_alt)
        enriched_model_U1 <- list(model = target_model_U1, data = rerolled_features, weight = weights1, label = outcome_var,  formula = dead_formula_alt)
    }
    # print(paste0("Break 5: ",Sys.time() - start_time))
    start_time <- Sys.time()
    # Call quality_of_fit_loss function
    qofl_T <- quality_of_fit_loss(enriched_model_T, enriched_model_T1)
    qofl_U <- quality_of_fit_loss(enriched_model_U, enriched_model_U1)
    
    # Create final dataframe with results
    qofl_df <- data.frame(target = proposed_target, qofl_T = qofl_T, qofl_U = qofl_U)
    # print(paste0("Break 6: ",Sys.time() - start_time))
    # Print results to verify
    print(qofl_df)
    return(qofl_df)
}
quality_of_fit <- function(enriched_model)
{
    model_A <- enriched_model$model  # Extract glm model
    data_A <- enriched_model$data
    label_col <- enriched_model$label
    weights <- enriched_model$weight
    formula_A <- enriched_model$formula
    if ("glmnet" %in% class(model_A))
    {
        x <- enriched_model$data$x
        labels <- as.numeric(enriched_model$data$y)
        lambda <- enriched_model$lambda
        if (is.null(lambda) || length(na.omit(lambda)) < 2) lambda <- model_A$lambda[1]
        if (is.na(lambda)) lambda <- 0
        predictions <- as.numeric(predict(model_A, newx = x, type = "response", s = lambda))
    }
    else
    {
        labels <- data_A[[label_col]]  # Extract correct column
        predictions <- predict(model_A, data_A, type = "response")  # Get probabilities
    }
    if (!is.numeric(weights)) weights <- as.numeric(weights)
    if (!is.numeric(labels)) labels <- as.numeric(labels)
    class_1_mean_prediction <- sum(labels * predictions * weights) / sum(labels * weights)
    class_0_mean_prediction <- sum((1 - labels) * predictions * weights) / sum((1 - labels) * weights)
    quality <- class_1_mean_prediction - class_0_mean_prediction
    # print(paste0("qofl ",quality,"c0 ", class_0_mean_prediction,"c1 ",class_1_mean_prediction,"\n"))
    return(quality)
}
quality_of_fit_loss <- function(enriched_model_1,enriched_model_2)
{
    quality_1 <- quality_of_fit(enriched_model_1)
    quality_2 <- quality_of_fit(enriched_model_2)
    quality_delta <- quality_2 - quality_1
    # print(paste0(quality_2," - ",quality_1," = ",quality_delta))
    return(quality_delta)
}
test_target_function <- function(proposed_target,
                                 features,
                                 ancestor,
                                 fileName,
                                 addl_vars = c("Dead","Treated","Untreated"),
                                 treatment_var = "Treated",
                                 outcome_var = "Dead",
                                 tech = "glm")
{
    print(paste0("Proposed target is ",proposed_target))
    qofl_row <- test_target(proposed_target,
                            features,
                            ancestor,
                            addl_vars,
                            treatment_var,
                            outcome_var,
                            tech
    )
    print(paste0("Completed iteration on ", proposed_target))
    if(!is.null(fileName)) write.table(qofl_row, file = fileName, append = TRUE, sep = ",", col.names = !file.exists(fileName), row.names = FALSE)
    return(qofl_row[1,])
}
