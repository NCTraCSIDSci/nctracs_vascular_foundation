# Feature supplemental
# This workbook will seek to collect and synthesize supplemental data, including:
#
# * Creation of index and decision dates
# * Measurement-based features
# * Drug-based features
# * Features drawn from summary tables (synthesized from multiple observations / conditions / etc)
# * Demographic features
#
# This supplemental table is then combined to create the wide master feature tables, which are divided into a training subset and validation subset. Patients with no incidents of features are eliminated in this process; this is a quality control check.

# Logic to create date windows.
require(duckdb)
require(tidyverse)
require(lubridate)
source("Dimensional/source_this_file.R")
set.seed(27)

con <- dbConnect(duckdb::duckdb(), dbdir = "Z:\\vascular.duckdb")

# First step: Create
revasc_all <- dbGetQuery(con,
    "SELECT
        deid_person_id AS person_id,
        procedure_date AS revasc_date
    FROM deriv_revascularization")

vasc_starts <- dbGetQuery(con,
    "SELECT DISTINCT
        deriv_vascular_dx.DEID_PERSON_ID AS person_id,
        MIN(CONDITION_START_DATE) AS vascular_start
    FROM deriv_vascular_dx
    INNER JOIN patient_summary
        ON deriv_vascular_dx.DEID_PERSON_ID = patient_summary.DEID_PERSON_ID
    WHERE CONDITION_START_DATE >= visit_min AND CONDITION_START_DATE <= visit_max
    GROUP BY deriv_vascular_dx.DEID_PERSON_ID")

# 0. Core patient summary details
patient_summary <- dbGetQuery(con,
    "SELECT DISTINCT
        deid_person_id AS person_id,
        visit_min,
        visit_max,
        age,
        CASE WHEN GENDER_SOURCE_VALUE = 'F' THEN 1
            ELSE 0 END AS is_female, --Encoded variable
        CASE WHEN death_date IS NULL THEN 0
            ELSE 1 END AS Dead,
        cci,
        CASE WHEN freq_smoking > 1 THEN 1
            WHEN freq_smoking = 1 THEN 0.5 --Possible errors
            ELSE 0
        END AS smoker,
        CASE WHEN freq_depres > 1 THEN 1
            WHEN freq_depres = 1 THEN 0.5 --Possible errors
            ELSE 0
        END AS depressed,
        ldl_min,
        ldl_mean,
        ldl_max,
        alb_min,
        alb_mean,
        alb_max,
        gfr_min,
        gfr_mean,
        gfr_max,
        abs(a1c_min) AS a1c_min, --Temporary patch because there are negatives listed here.
        a1c_mean,
        a1c_max
    FROM patient_summary")

# Patient summary may have duplicates (it should not)
anomalies <- patient_summary %>%
    group_by(person_id) %>%
    summarize(count_records = n()) %>%
    filter(count_records > 1)
patient_summary <- patient_summary %>%
    anti_join(anomalies)

#Select random revascularization:
revasc_rand <- revasc_all %>%
    inner_join(vasc_starts) %>%
    filter(revasc_date > vascular_start) %>%
    group_by(person_id) %>%
    arrange(person_id, revasc_date) %>% # For replicability, as spark is disorderly
    summarize(treatment_date = sample(revasc_date, size = 1)) # Sample from within group!
revasc_useful <- revasc_rand %>%
    inner_join(revasc_all) %>%
    group_by(person_id, treatment_date) %>%
    summarize(prior_revascs = sum(revasc_date < treatment_date)) %>%
    inner_join(vasc_starts) %>%
    mutate(window_size = as.numeric(treatment_date - vascular_start), Treated = 1) %>%
    rename(decision_date = treatment_date)

untreated <- vasc_starts %>%
    anti_join(revasc_all) %>% # This eliminates patients with revasc_date <= vascular_start. There are about 300-400 of these.
    mutate(Treated = 0, prior_revascs = 0) %>%
    mutate(window_size = sample(revasc_useful$window_size, replace = TRUE, size = nrow(.))) %>%
    mutate(decision_date = vascular_start + window_size) # Generate synthetic "nontreatment" date for comparison_purposes
patients_dates <- bind_rows(revasc_useful, untreated)

features_supplement <- patients_dates %>%
    inner_join(patient_summary)

# Register relevant_dates in DuckDB so the drug/procedure queries below can JOIN against it
dbWriteTable(con, "relevant_dates",
             features_supplement %>% select(person_id, vascular_start, decision_date),
             overwrite = TRUE)

# Thromb exposure before decision date
thromb <- dbGetQuery(con,
                     "SELECT DISTINCT person_id
        FROM deriv_rx_thrombotic
        INNER JOIN relevant_dates
            ON DEID_PERSON_ID = person_id
        WHERE drug_exposure_start_date < decision_date") %>%
    mutate(thromb = 1)

# Statin exposure before decision date
statin <- dbGetQuery(con,
                     "SELECT DISTINCT person_id
        FROM deriv_rx_statins
        INNER JOIN relevant_dates
            ON DEID_PERSON_ID = person_id
        WHERE drug_exposure_start_date < decision_date") %>%
    mutate(statin = 1)

# Limb loss after decision date (earliest date per person)
limb_loss <- dbGetQuery(con,
    "SELECT person_id,
        MIN(procedure_date) AS limb_loss_date
    FROM deriv_amputations
    INNER JOIN relevant_dates
        ON DEID_PERSON_ID = person_id
    WHERE amp_leg = 1 AND procedure_date > decision_date
    GROUP BY person_id")

# Count prior amputations before decision date
prior_amp <- dbGetQuery(con,
    "SELECT person_id,
        COUNT(*) AS prior_amputations
    FROM deriv_amputations
    INNER JOIN relevant_dates
        ON DEID_PERSON_ID = person_id
    WHERE procedure_date < decision_date
    GROUP BY person_id")

patient_pvl_summary <- dbGetQuery(con,
    "SELECT deid_person_id AS person_id,
        --CASE WHEN MIN(CAST(value AS DOUBLE)) <= 0.4 THEN 1 ELSE 0 END AS very_low_pvl,
        --CASE WHEN MIN(CAST(value AS DOUBLE)) <= 0.9 THEN 1 ELSE 0 END AS low_pvl,
        --CASE WHEN MAX(CAST(value AS DOUBLE)) > 1.3 THEN 1 ELSE 0 END AS high_pvl,
        MIN(CAST(value AS DOUBLE)) AS min_PVL,
        MAX(CAST(value AS DOUBLE)) AS max_PVL,
        STDDEV(CAST(value AS DOUBLE)) AS std_PVL
    FROM deriv_pvl_final
    WHERE value IS NOT NULL
        AND CAST(value AS DOUBLE) > 0.0
        AND CAST(value AS DOUBLE) < 10.0
    GROUP BY deid_person_id") %>%
    mutate(min_PVL = as.numeric(min_PVL),
        max_PVL = as.numeric(max_PVL))

prior_visits <- dbGetQuery(con,
    "SELECT person_id,
        COUNT(DISTINCT visit_start_date) AS visit_ct
    FROM visit_occurrence
    INNER JOIN relevant_dates
        ON DEID_PERSON_ID = person_id
    WHERE visit_start_date < decision_date AND visit_start_date >= vascular_start
    GROUP BY person_id")

prior_conds <- dbGetQuery(con,
    "SELECT person_id,
        COUNT(DISTINCT condition_concept_id) AS condition_ct
    FROM condition_occurrence
    INNER JOIN relevant_dates
        ON DEID_PERSON_ID = person_id
    WHERE condition_start_date < decision_date AND condition_start_date >= vascular_start
    GROUP BY person_id")

prior_obs <- dbGetQuery(con,
    "SELECT person_id,
        COUNT(DISTINCT observation_concept_id) AS observation_ct
    FROM observation
    INNER JOIN relevant_dates
        ON DEID_PERSON_ID = person_id
    WHERE observation_date < decision_date AND observation_date >= vascular_start
    GROUP BY person_id")

prior_procedures <- dbGetQuery(con,
    "SELECT person_id,
        COUNT(DISTINCT procedure_concept_id) AS procedure_ct
    FROM procedure_occurrence
    INNER JOIN relevant_dates
        ON DEID_PERSON_ID = person_id
    WHERE procedure_date < decision_date AND procedure_date >= vascular_start
    GROUP BY person_id")

# Merge exposures, summary features, and activity counts
features_final <- features_supplement %>%
    # Add binary exposures
    left_join(thromb, by = "person_id") %>%
    left_join(statin, by = "person_id") %>%
    left_join(limb_loss %>% mutate(Amputated = 1), by = "person_id") %>%
    left_join(prior_amp, by = "person_id") %>%
    # Add PVL features
    left_join(patient_pvl_summary, by = "person_id") %>%
    # Add longitudinal activity summaries
    left_join(prior_visits, by = "person_id") %>%
    left_join(prior_conds, by = "person_id") %>%
    left_join(prior_obs, by = "person_id") %>%
    left_join(prior_procedures, by = "person_id") %>%
    # Fill in missing flags and counts with zeros
    mutate(across(c(thromb, statin, Amputated, prior_amputations, Treated,
                    #very_low_pvl, low_pvl, high_pvl,
                    visit_ct, condition_ct, observation_ct, procedure_ct), #Omits
                  ~ coalesce(., 0))) %>%
    rename_with(~ paste0("F_", .), .cols = -c(person_id, vascular_start, decision_date, Treated, Amputated, Dead, visit_min, visit_max, limb_loss_date)) # Add req'd prefix.

dbWriteTable(con, "features_final", features_final, overwrite = TRUE)

summary(features_final)


# ---------------------------------------------------------------------------
# Imputation with mice
# ---------------------------------------------------------------------------
if (!requireNamespace("mice", quietly = TRUE))
{
    install.packages("mice")
}
require(mice)
set.seed(27)
bad_cols <- c("decision_date","vascular_start","visit_min","visit_max","limb_loss_date","window_size","Dead","Amputated","Treated","Untreated")
features_df_base <- features_final %>%
    select(-any_of(bad_cols))
features_disinclude <- features_final %>%
    select(person_id, any_of(bad_cols))
id_col <- features_df_base["person_id"]
features_df_base <- features_df_base %>% select(-person_id)
mouse <- mice(features_df_base)
features_final_fixed <- complete(mouse, 1) %>%
    inner_join(features_disinclude) %>%
    mutate(Untreated = 1 - Treated)

# ---------------------------------------------------------------------------
# Write feature supplement table
# ---------------------------------------------------------------------------
dbWriteTable(con, "vascular_feature_supplement", features_final_fixed, overwrite = TRUE)

# ---------------------------------------------------------------------------
# Generate features_min
# ---------------------------------------------------------------------------
trainset_size <- 10000 # Choice of training set size. Rest of sample is validation.

# Data load
patient_summary <- dbReadTable(con, "vascular_feature_supplement")
rolled_features <- dbReadTable(con, "rerolled_features") %>%
    select(person_id, rerolled_concept_id, concept_date) %>%
    rename(concept_id = rerolled_concept_id)

if(file.exists("manual_rollups.csv"))
{
    manual_rollups <- read_csv("manual_rollups.csv") %>%
        rename(concept_id = descendant_concept_id) %>%
        select(concept_id, manual_concept)

    dbWriteTable(con, "manual_rollups", manual_rollups, overwrite = TRUE)

    rolled_features <- rolled_features %>%
        left_join(manual_rollups, by = "concept_id") %>%
        mutate(concept_id = coalesce(manual_concept, concept_id)) %>% # Roll up manual features
        filter(is.na(manual_concept) | grepl("^F_", manual_concept)) # Remove non-features
}

rolled_features <- rolled_features %>%
    inner_join(patient_summary %>% select(person_id, vascular_start, decision_date)) %>%
    filter(concept_date <= decision_date & concept_date >= vascular_start) %>%
    select(person_id, concept_id) %>%
    distinct() %>%
    mutate(present = 1L) %>%
    pivot_wider(names_from = concept_id, values_from = present, values_fill = 0L)

combined_features <- patient_summary %>%
    inner_join(rolled_features) # This will eliminate a small number of persons lacking conditions / observations. Probably okay.

# Normalize combined features to a 0 to 1 range. This is significant for age, CCI, and measurements.
# numeric_cols <- names(combined_features)[sapply(combined_features, is.numeric)]

#for (col_name in numeric_cols)
#{
#    cf_stats <- combined_features %>%
#        summarize(max_val = max(.data[[col_name]]),
#            min_val = min(.data[[col_name]]))
#    if(!(cf_stats$max_val == 1 & cf_stats$min_val == 0) & ! (cf_stats$max_val == cf_stats$min_val))
#    {
#        combined_features <- combined_features %>%
#            mutate(!!col_name := (.data[[col_name]] - cf_stats$min_val) / (cf_stats$max_val - cf_stats$min_val))
#    }
#}

# Split into testing and training set once and in replicable manner.

master_ids <- combined_features %>%
    select(person_id) %>%
    arrange(person_id)

set.seed(37) # Deterministic.
sample_indices <- sample(1:nrow(master_ids), trainset_size)
master_ids$include <- 0
master_ids$include[sample_indices] <- 1

wide_training_set <- master_ids %>%
    filter(include == 1) %>%
    select(-include) %>%
    inner_join(combined_features)

wide_validation_set <- master_ids %>%
    filter(include == 0) %>%
    select(-include) %>%
    inner_join(combined_features)

# ---------------------------------------------------------------------------
# Write training and validation tables
# ---------------------------------------------------------------------------
dbWriteTable(con, "vascular_wide_validation", wide_validation_set, overwrite = TRUE)
dbWriteTable(con, "vascular_wide_training",   wide_training_set,   overwrite = TRUE)

# ---------------------------------------------------------------------------

anomalies <- combined_features %>%
    group_by(person_id) %>%
    summarize(count = n()) %>%
    filter(count > 1)
print(anomalies)

# ---------------------------------------------------------------------------
# Missing value check: vascular_wide_training
# ---------------------------------------------------------------------------
features_wide_training <- dbReadTable(con, "vascular_wide_training")
# Count NA (null) values per column
# Create SQL query dynamically
columns <- colnames(features_wide_training)
query <- paste(
    "SELECT",
    paste0("SUM(CASE WHEN ", columns, " IS NULL THEN 1 ELSE 0 END) AS ", columns, collapse = ", "),
    "FROM vascular_wide_training"
)

missing_df_1 <- dbGetQuery(con, query) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    arrange(desc(missing_count))
summary(missing_df_1)

# ---------------------------------------------------------------------------
# Distinct concept count in rerolled features
# ---------------------------------------------------------------------------
dbGetQuery(con, "SELECT COUNT(DISTINCT concept_id) FROM rerolled_features")

# ---------------------------------------------------------------------------
# Missing value check: vascular_rerolled_features
# ---------------------------------------------------------------------------
vasc_reroll <- dbReadTable(con, "rerolled_features")
# Count NA (null) values per column
# Create SQL query dynamically
columns <- colnames(vasc_reroll)
query <- paste(
    "SELECT",
    paste0("SUM(CASE WHEN ", columns, " IS NULL THEN 1 ELSE 0 END) AS ", columns, collapse = ", "),
    "FROM rerolled_features"
)

missing_df_2 <- dbGetQuery(con, query) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    arrange(desc(missing_count))
summary(missing_df_2)

# ---------------------------------------------------------------------------

missing_df_1

# ---------------------------------------------------------------------------

str(rolled_features)

# ---------------------------------------------------------------------------
dbExecute(con, "CHECKPOINT")
dbExecute(con, "VACUUM")
dbDisconnect(con,  shutdown="TRUE")