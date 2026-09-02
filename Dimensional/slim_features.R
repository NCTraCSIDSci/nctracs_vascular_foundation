# Presumes the existence of person_id, concept_id, first_amp, last_amp, first_revasc, death_date columns
slim_features <- function(wide_features, ancestor_table, sparky, max_level)
{
    is_spark  <- inherits(sparky, "spark_connection")
    is_duckdb <- inherits(sparky, "duckdb_connection")
    if (!is_spark && !is_duckdb && !identical(sparky, FALSE)) {
        stop(paste0(
            "slim_features: the 'sparky' argument must be a spark_connection, ",
            "a duckdb_connection, or FALSE (for plain R data frames).\n",
            "  Received object of class: ", paste(class(sparky), collapse = ", "), "\n",
            "  If you intended to run without any database backend, pass sparky = FALSE."
        ))
    }

    tall_outcomes <- wide_features %>%
        mutate(Treated = !is.na(first_revasc),
               Dead = !is.na(death_date),
               Amputated = !is.na(first_amp) & (is.na(first_revasc) | last_amp > first_revasc), #Looking for amputation outcomes.
               Untreated = is.na(first_revasc))
    gc()
    tall_outcomes <- tall_outcomes %>%
        select(person_id,
               Treated,
               Dead,
               Amputated,
               Untreated)
    gc()
    tall_outcomes <- tall_outcomes %>%
        pivot_longer(cols = !person_id,
                     names_to = "concept_id",
                     values_to = "present",
            values_drop_na = TRUE)
    gc()
    tall_outcomes <- tall_outcomes %>%
        filter(present == TRUE) %>%
        select(person_id, concept_id)
    gc()

    features_min <- wide_features %>%
        filter(is.na(first_revasc) | (concept_date < first_revasc)) %>%
        select(person_id, concept_id) %>%
        bind_rows(tall_outcomes)

    features_that_exist <- features_min %>%
        select(concept_id) %>%
        distinct() %>%
        rename(descendant_concept_id = concept_id) %>%
        collect()

    concept_ancestor_trimmed <- ancestor_table %>%
        filter(min_levels_of_separation <= max_level) %>%
        select(ancestor_concept_id, descendant_concept_id, max_level) %>%
        inner_join(features_that_exist) %>%
        distinct() %>%
        add_row(descendant_concept_id = "Dead") %>%
        add_row(descendant_concept_id = "Treated") %>%
        add_row(descendant_concept_id = "Amputated") %>%
        add_row(descendant_concept_id = "Untreated")

    if (is_spark) {
        concept_ancestor_trimmed <- sdf_copy_to(sparky, concept_ancestor_trimmed, "concept_ancestor_sparky", overwrite = TRUE, copy = TRUE)
    } else if (is_duckdb) {
        dbWriteTable(sparky, "concept_ancestor_sparky", concept_ancestor_trimmed, overwrite = TRUE)
        # concept_ancestor_trimmed remains a local R data frame; the table in DuckDB
        # is available for any subsequent SQL that needs it directly.
    }
    # else (sparky == FALSE): concept_ancestor_trimmed stays as a local R data frame; nothing to do.

    features_min_trimmed <- features_min %>%
        inner_join(concept_ancestor_trimmed %>%
                       select(descendant_concept_id) %>%
                       rename(concept_id = descendant_concept_id) %>%
                       distinct())
    results <- list(data = features_min_trimmed, ontology = concept_ancestor_trimmed)
    return(results)
}

skinny_combined_features <- function(features, spark_connection, random_seed = 37)
{
    is_spark  <- inherits(spark_connection, "spark_connection")
    is_duckdb <- inherits(spark_connection, "duckdb_connection")
    if (!is_spark && !is_duckdb) {
        stop(paste0(
            "skinny_combined_features: the 'spark_connection' argument must be a spark_connection ",
            "or a duckdb_connection.\n",
            "  Received object of class: ", paste(class(spark_connection), collapse = ", "), "\n",
            "  Pass the active DuckDB connection (e.g. the 'con' object) or a valid Spark connection."
        ))
    }

    if (is_spark) {
        # Ensure date fields are properly parsed in Spark
        features <- features %>%
            mutate(
                vascular_start   = to_date(vascular_start),
                treatment_date   = to_date(treatment_date),
                death_date       = to_date(death_date),
                last_amp         = to_date(last_amp),
                concept_date     = to_date(concept_date)
            )

        # Generate synthetic Treated cohort
        treated_ppl <- features %>%
            filter(!is.na(treatment_date)) %>%
            mutate(days_plus = sql("datediff(vascular_start, treatment_date)")) %>%
            mutate(concept_id = "Treated") %>%
            rename(decision_date = treatment_date) %>%
            select(person_id, decision_date, days_plus, concept_id) %>%
            distinct() %>%
            collect()

        # Pull sample for synthetic Untreated dates
        days_plus_pool <- treated_ppl$days_plus

        untreated_ppl <- features %>%
            filter(is.na(treatment_date)) %>%  # correct filtering!
            select(person_id, vascular_start) %>%
            distinct() %>%
            collect()

        set.seed(random_seed)
        untreated_ppl$days_plus <- sample(days_plus_pool, nrow(untreated_ppl), replace = TRUE)
        untreated_ppl$decision_date <- as.Date(untreated_ppl$vascular_start) + untreated_ppl$days_plus
        untreated_ppl$concept_id <- "Untreated"

        # Combine cohorts and push to Spark
        base_ppl <- bind_rows(treated_ppl, untreated_ppl)
        base_ppl_spark <- copy_to(spark_connection, base_ppl, "tmp_base_ppl", overwrite = TRUE)

        # Join decision_date into original feature set
        features <- features %>%
            inner_join(base_ppl_spark %>% select(person_id, decision_date), by = "person_id")

        # Mark Dead cohort
        dead_ppl <- features %>%
            filter(!is.na(death_date)) %>%
            select(person_id) %>%
            distinct() %>%
            mutate(concept_id = "Dead") %>%
            collect()

        # Mark Amputated cohort after decision_date
        amputated_ppl <- features %>%
            filter(!is.na(last_amp) & last_amp > decision_date) %>%
            select(person_id) %>%
            distinct() %>%
            mutate(concept_id = "Amputated") %>%
            collect()

        # Combine pre-decision concepts
        features_min <- features %>%
            filter(concept_date < decision_date) %>%
            select(person_id, concept_id) %>%
            distinct()

        # Add treatment status + outcomes
        outcome_tags <- bind_rows(base_ppl %>% select(person_id, concept_id),
                                  amputated_ppl,
                                  dead_ppl)

        # Push outcomes to Spark
        outcome_tags_spark <- copy_to(spark_connection, outcome_tags, "tmp_outcomes", overwrite = TRUE)

        # Combine final feature set
        features_final <- features_min %>%
            sdf_bind_rows(outcome_tags_spark)

    } else if (is_duckdb) {
        # Ensure date fields are properly typed in R
        features <- features %>%
            mutate(
                vascular_start   = as.Date(vascular_start),
                treatment_date   = as.Date(treatment_date),
                death_date       = as.Date(death_date),
                last_amp         = as.Date(last_amp),
                concept_date     = as.Date(concept_date)
            )

        # Generate synthetic Treated cohort
        treated_ppl <- features %>%
            filter(!is.na(treatment_date)) %>%
            mutate(days_plus = as.numeric(vascular_start - treatment_date)) %>%
            mutate(concept_id = "Treated") %>%
            rename(decision_date = treatment_date) %>%
            select(person_id, decision_date, days_plus, concept_id) %>%
            distinct()

        # Pull sample for synthetic Untreated dates
        days_plus_pool <- treated_ppl$days_plus

        untreated_ppl <- features %>%
            filter(is.na(treatment_date)) %>%  # correct filtering!
            select(person_id, vascular_start) %>%
            distinct()

        set.seed(random_seed)
        untreated_ppl$days_plus <- sample(days_plus_pool, nrow(untreated_ppl), replace = TRUE)
        untreated_ppl$decision_date <- as.Date(untreated_ppl$vascular_start) + untreated_ppl$days_plus
        untreated_ppl$concept_id <- "Untreated"

        # Combine cohorts and register in DuckDB for any downstream SQL
        base_ppl <- bind_rows(treated_ppl, untreated_ppl)
        dbWriteTable(spark_connection, "tmp_base_ppl", base_ppl, overwrite = TRUE)

        # Join decision_date into original feature set
        features <- features %>%
            inner_join(base_ppl %>% select(person_id, decision_date), by = "person_id")

        # Mark Dead cohort
        dead_ppl <- features %>%
            filter(!is.na(death_date)) %>%
            select(person_id) %>%
            distinct() %>%
            mutate(concept_id = "Dead")

        # Mark Amputated cohort after decision_date
        amputated_ppl <- features %>%
            filter(!is.na(last_amp) & last_amp > decision_date) %>%
            select(person_id) %>%
            distinct() %>%
            mutate(concept_id = "Amputated")

        # Combine pre-decision concepts
        features_min <- features %>%
            filter(concept_date < decision_date) %>%
            select(person_id, concept_id) %>%
            distinct()

        # Add treatment status + outcomes
        outcome_tags <- bind_rows(base_ppl %>% select(person_id, concept_id),
                                  amputated_ppl,
                                  dead_ppl)

        dbWriteTable(spark_connection, "tmp_outcomes", outcome_tags, overwrite = TRUE)

        # Combine final feature set
        features_final <- bind_rows(features_min, outcome_tags)
    }

    results <- list(features = features_final, base_ppl = base_ppl)
    return(results)
}