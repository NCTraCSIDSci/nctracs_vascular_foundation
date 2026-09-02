# First feature rollup stage.
# Author(s): Tomas McIntee
# Last updated: 2025-04-17
#
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed
# under the MIT License and permission is granted to use in accordance with the
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.

# Load libraries and set parameters. This cell must be run before any other cell, but other cells may be run in isolation.
source("Dimensional/source_this_file.R") # De facto custom library (functions and R data).
require(duckdb)
require(dplyr)
require(tidyverse)

# User-defined parameters
minimum_count <- 1700 # Minimum frequency of about 1%.
specificity_naughty_list <- c() # c(764156, 746155, 764154, 746153) # "Disorder of X limb" concepts; the sidedness
max_roll_distance <- 4 # Maximum number of levels to use for the fast roll.
generality_naughty_list <- c() # Empty the generality list
#concept %>%
#    filter(grepl('disorder of',tolower(concept_name)) |
#        grepl('finding',tolower(concept_name)) |
#        grepl('disease of',tolower(concept_name))) %>%
#    pull(concept_id)

read_con <- dbConnect(duckdb::duckdb(), dbdir = "Z:\\vascular.duckdb",
    config = list(temp_directory = "C:\\Temp"),
    read_only = TRUE)
write_con <- dbConnect(duckdb::duckdb(), dbdir = "Z:\\vascular_model.duckdb",
    config = list(temp_directory = "C:\\Temp"))
gc()
# ---------------------------------------------------------------------------
# Create binary features table
# ---------------------------------------------------------------------------
binary_features <- dbGetQuery(read_con, "SELECT DISTINCT
        condition_visit_dates.DEID_PERSON_ID AS person_id,
        CONCEPT_NAME AS concept_name,
        CONCAT('F_', CONCEPT_ID) AS concept_id,
        VISIT_START_DATE AS concept_date,
        vascular_start,
        min_revasc_date AS first_revasc,
        max_revasc_date AS last_revasc,
        CASE WHEN death_date IS NOT NULL
                AND cod1 LIKE 'I%'
                AND (cod2 LIKE 'I%'
                    OR cod3 LIKE 'I%')
                    THEN death_date
            ELSE NULL END AS death_date,
        --death_date,
        min_amp_date AS first_amp,
        max_amp_date AS last_amp
    FROM condition_visit_dates
        INNER JOIN patient_summary_no_outliers
            ON condition_visit_dates.DEID_PERSON_ID = patient_summary_no_outliers.DEID_PERSON_ID
    WHERE patient_summary_no_outliers.vascular_start <= condition_visit_dates.VISIT_START_DATE
    UNION
    SELECT DISTINCT
        observation.DEID_PERSON_ID AS person_id,
        'Observation' AS concept_name,
        CONCAT('F_', OBSERVATION_CONCEPT_ID) AS concept_id,
        OBSERVATION_DATE AS concept_date,
        vascular_start,
        min_revasc_date AS first_revasc,
        max_revasc_date AS last_revasc,
        CASE WHEN death_date IS NOT NULL
                AND cod1 LIKE 'I%'
                AND (cod2 LIKE 'I%'
                    OR cod3 LIKE 'I%')
                    THEN death_date
            ELSE NULL END AS death_date,
        --death_date,
        min_amp_date AS first_amp,
        max_amp_date AS last_amp
    FROM observation
        INNER JOIN patient_summary_no_outliers
            ON observation.DEID_PERSON_ID = patient_summary_no_outliers.DEID_PERSON_ID
    WHERE patient_summary_no_outliers.vascular_start <= observation.OBSERVATION_DATE
")
dbWriteTable(conn = write_con,
    name = "binary_features",
    value = binary_features,
    overwrite = TRUE)

# ---------------------------------------------------------------------------
# Create roll_table_fast and write to DuckDB
# ---------------------------------------------------------------------------
# Read binary features from DuckDB into R
features_table <- dbReadTable(write_con, "binary_features")
gc()
patient_table <- dbReadTable(read_con, "patient_summary_no_outliers")
gc()
# Create extant_conditions_network.
extant_condition_network <- create_extant_network(concept_ancestor, features_table)
rm(features_table, patient_table)
gc()

# Create fast roll table
roll_table_fast <- generate_fast_roll_table(extant_condition_network,
    specificity_naughty_list,
    generality_naughty_list,
    minimum_count,
    max_roll_distance)
rm(extant_condition_network)
gc()

# Write roll_table_fast to DuckDB so the SQL below can reference it
dbWriteTable(write_con, "roll_table_fast_spark", roll_table_fast, overwrite = TRUE)
rm(roll_table_fast)
gc()

# ---------------------------------------------------------------------------
# Create rolled features table
# ---------------------------------------------------------------------------
dbExecute(write_con, "
    CREATE OR REPLACE TABLE rolled_features AS
    SELECT
        binary_features.concept_id,
        binary_features.person_id,
        binary_features.concept_date,
        binary_features.first_revasc,
        binary_features.last_revasc,
        binary_features.first_amp,
        binary_features.last_amp,
        binary_features.death_date,
        REPLACE(roll_table_fast_spark.rolled_concept_id, '`', '') AS rolled_concept_id
    FROM binary_features
        INNER JOIN roll_table_fast_spark
            ON binary_features.concept_id = roll_table_fast_spark.concept_id
")

# ---------------------------------------------------------------------------
# Create arranged_priority.csv
# ---------------------------------------------------------------------------

# Read rolled features from DuckDB into R
rolled_features <- dbReadTable(write_con, "rolled_features")
patient_table <- dbReadTable(read_con, "patient_summary_no_outliers")
rm(patient_table)
dbExecute(read_con, "CHECKPOINT")
dbExecute(read_con, "VACUUM")
dbDisconnect(read_con,  shutdown="TRUE")
gc()

# Create arranged priority list.
features_narrow <- rolled_features %>%
    select(person_id,
        rolled_concept_id,
        concept_date,
        first_revasc,
        last_revasc,
        first_amp,
        last_amp,
        death_date) %>%
    rename(concept_id = rolled_concept_id)
rm(rolled_features)
gc()

arranged_priority <- generate_priority_table(features_narrow, concept)
rm(features_narrow)
gc()

print(arranged_priority)
write.csv(arranged_priority, "arranged_priority.csv", row.names = FALSE)
rm(arranged_priority)
gc()

# ---------------------------------------------------------------------------
# Generate qofl_df_full_amp.csv
# ---------------------------------------------------------------------------

# Create possible_rollups
arranged_priority <- read.csv("arranged_priority.csv")
gc()
# Read rolled features from DuckDB into R
rolled_features <- dbReadTable(write_con, "rolled_features")

# Construct tall dataset
trimmed <- slim_features(rolled_features,
    concept_ancestor,
    sparky = FALSE,
    max_level = 4)
rm(rolled_features)
gc()

concept_ancestor_trimmed <- trimmed$ontology
features_min_trimmed <- trimmed$data
rm(trimmed)
gc()

possible_rollups <- generate_rollup_candidates(arranged_priority,
    concept_ancestor,
    generality_naughty_list,
    max_level = 1,
    min_level = 1) #This is used only for generating the rollup targets.
rm(arranged_priority)
gc()

iter_list <- possible_rollups %>%
    filter(!ancestor_concept_id %in% generality_naughty_list) %>%
    select(ancestor_concept_id) %>%
    distinct() %>%
    pull(ancestor_concept_id)
rm(possible_rollups)
gc()

# Split iter_list into chunks of 100
chunked_lists <- split(iter_list, ceiling(seq_along(iter_list) / 100))
rm(iter_list)
gc()

oldw <- getOption("warn")
options(warn = -1)

anc <- concept_ancestor_trimmed
rm(concept_ancestor_trimmed)
gc()

# Iterate over chunks and save results separately
for (i in seq_along(chunked_lists))
{
    chunk <- chunked_lists[[i]]
    chunk_name <- paste0("qofl_df_chunk_amp_", i, ".csv")
    if(file.exists(chunk_name))
    {
        print(paste0("Skipped chunk ", i, "\n"))
        next
    }
    results_chunk <- lapply(chunk,
        test_target_function,
        fileName = NULL,
        features = features_min_trimmed,
        ancestor = anc,
        addl_vars = c("Amputated", "Treated", "Untreated"),
        outcome_var = "Amputated",
        treatment_var = "Treated",
        tech = "glm")
    print(class(results_chunk))
    print(str(results_chunk))
    qofl_df_chunk <- do.call(rbind, results_chunk)
    rm(results_chunk)
    print(class(qofl_df_chunk))
    print(str(qofl_df_chunk))
    qofl_df_chunk_clean <- qofl_df_chunk %>%
        mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))
    rm(qofl_df_chunk)
    tryCatch(
        {
            write.csv(qofl_df_chunk_clean, chunk_name, row.names = FALSE)
        },
        error = function(e)
        {
            message(paste0("Failed to write chunk ", i, ": ", e$message))
        })
    rm(qofl_df_chunk_clean)
    gc()
}
rm(anc, features_min_trimmed, chunked_lists)
gc()

# List all chunk file names that match the expected pattern``
chunk_files <- list.files(pattern = "^qofl_df_chunk_amp_\\d+\\.csv$")

# Read each file and store in a list
chunk_data_list <- lapply(chunk_files, read.csv, stringsAsFactors = FALSE)

# Combine all the chunks into a single data frame
qofl_df_full_amp <- do.call(rbind, chunk_data_list)
rm(chunk_data_list)
gc()

# Write the combined data frame to a single CSV file
write.csv(qofl_df_full_amp, "qofl_df_full_amp.csv", row.names = FALSE)
rm(qofl_df_full_amp)
gc()

# Optional: confirm completion
message("Successfully stitched ", length(chunk_files), " chunks into qofl_df_full_amp.csv")

options(warn = oldw)

# ---------------------------------------------------------------------------
# Generate qofl_df_full_dead.csv
# ---------------------------------------------------------------------------

# Create possible_rollups
arranged_priority <- read.csv("arranged_priority.csv")

# Read rolled features from DuckDB into R
rolled_features <- dbReadTable(write_con, "rolled_features")

# Construct tall dataset
trimmed <- slim_features(rolled_features, concept_ancestor, sparky = FALSE, max_level = 4)
rm(rolled_features)
gc()

concept_ancestor_trimmed <- trimmed$ontology
features_min_trimmed <- trimmed$data
rm(trimmed)
gc()

possible_rollups <- generate_rollup_candidates(arranged_priority,
    concept_ancestor,
    generality_naughty_list,
    max_level = 1,
    min_level = 1) #This is used only for generating the rollup targets.
rm(arranged_priority)
gc()

iter_list <- possible_rollups %>%
    filter(!ancestor_concept_id %in% generality_naughty_list) %>%
    select(ancestor_concept_id) %>%
    distinct() %>%
    pull(ancestor_concept_id)
rm(possible_rollups)
gc()

# Split iter_list into chunks of 100
chunked_lists <- split(iter_list, ceiling(seq_along(iter_list) / 100))
rm(iter_list)
gc()

oldw <- getOption("warn")
options(warn = -1)

anc <- concept_ancestor_trimmed
rm(concept_ancestor_trimmed)
gc()

# Iterate over chunks and save results separately
for (i in seq_along(chunked_lists))
{
    chunk <- chunked_lists[[i]]
    chunk_name <- paste0("qofl_df_chunk_dead_", i, ".csv")
    if(file.exists(chunk_name))
    {
        print(paste0("Skipped chunk ", i, "\n"))
        next
    }
    results_chunk <- lapply(chunk,
        test_target_function,
        fileName = NULL,
        features = features_min_trimmed,
        ancestor = anc,
        addl_vars = c("Dead", "Treated", "Untreated"),
        outcome_var = "Dead",
        treatment_var = "Treated",
        tech = "glm")
    print(class(results_chunk))
    print(str(results_chunk))
    qofl_df_chunk <- do.call(rbind, results_chunk)
    rm(results_chunk)
    print(class(qofl_df_chunk))
    print(str(qofl_df_chunk))
    qofl_df_chunk_clean <- qofl_df_chunk %>%
        mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))
    rm(qofl_df_chunk)
    tryCatch(
        {
            write.csv(qofl_df_chunk_clean, chunk_name, row.names = FALSE)
        },
        error = function(e)
        {
            message(paste0("Failed to write chunk ", i, ": ", e$message))
        })
    rm(qofl_df_chunk_clean)
    gc()
}
rm(anc, features_min_trimmed, chunked_lists)
gc()

# List all chunk file names that match the expected pattern
chunk_files <- list.files(pattern = "^qofl_df_chunk_dead_\\d+\\.csv$")

# Read each file and store in a list
chunk_data_list <- lapply(chunk_files, read.csv, stringsAsFactors = FALSE)

# Combine all the chunks into a single data frame
qofl_df_full_dead <- do.call(rbind, chunk_data_list)
rm(chunk_data_list)
gc()

# Write the combined data frame to a single CSV file
write.csv(qofl_df_full_dead, "qofl_df_full_dead.csv", row.names = FALSE)
rm(qofl_df_full_dead)
gc()

# Optional: confirm completion
message("Successfully stitched ", length(chunk_files), " chunks into qofl_df_full_dead.csv")

options(warn = oldw)

# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Reroll candidates (amp)
# ---------------------------------------------------------------------------

qofl_df <- read.csv("qofl_df_full_amp.csv")

rolled_features <- dbReadTable(write_con, "rolled_features")
sum_criteria <- 0
min_criteria <- -1e-3

reroll_candidates <- qofl_df %>%
    filter(qofl_T > min_criteria) %>%
    filter(qofl_U > min_criteria) %>%
    select(target, qofl_T, qofl_U) %>%
    mutate(qofl_sum = qofl_T + qofl_U) %>%
    mutate(qofl_min = pmin(qofl_T, qofl_U)) %>%
    arrange(-qofl_sum) %>%
    rename(ancestor_concept_id = target) %>%
    inner_join(concept_ancestor) %>%
    rename(rolled_concept_id = descendant_concept_id,
        rerolled_concept_id = ancestor_concept_id)
rm(qofl_df)
gc()

reroll_candidates <- reroll_candidates %>%
    select(rolled_concept_id, rerolled_concept_id) %>%
    distinct()

# Write reroll_candidates to DuckDB so the join can be done in-database
dbWriteTable(write_con, "reroll_candidates",
    reroll_candidates %>% select(rolled_concept_id, rerolled_concept_id),
    overwrite = TRUE)


gc()
rerolled_features <- rolled_features %>%
    left_join(reroll_candidates,
        by = "rolled_concept_id")
rm(rolled_features)
gc()
rerolled_features <- rerolled_features %>%
    mutate(rerolled_concept_id = coalesce(rerolled_concept_id, rolled_concept_id))
gc()

dbWriteTable(write_con,name = "rerolled_features",rerolled_features, overwrite = TRUE)
summary(reroll_candidates)
rm(reroll_candidates)
gc()

str(rerolled_features)


# ---------------------------------------------------------------------------
rerolled_features <- dbReadTable(write_con,"rerolled_features")
gc()
rerolled_features_checker <- rerolled_features %>%
    select(-concept_id) %>%
    rename(concept_id = rerolled_concept_id)
rm(rerolled_features)
gc()

arranged_priority <- generate_priority_table(rerolled_features_checker, concept)

print(arranged_priority)
write.csv(arranged_priority, "arranged_priority_reroll.csv", row.names = FALSE)

rm(rerolled_features_checker)
gc()
dbExecute(write_con, "CHECKPOINT")
dbExecute(write_con, "VACUUM")
dbDisconnect(write_con,  shutdown="TRUE")
