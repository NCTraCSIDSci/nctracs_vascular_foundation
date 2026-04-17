# Create Patient-Level Summary Table
# Author(s): Peter Leese
# Started on 2024-12-22
# Last updated: 2025-01-12
# 
# This script creates the final analytical dataset by assembling a comprehensive
# patient-level summary table. Each row represents one patient with aggregated
# information from all clinical domains: demographics, conditions, procedures,
# medications, laboratory results, and social determinants of health.
# 
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.
#
# INPUTS:
#   - person: Patient demographics
#   - state_death: Mortality data
#   - condition_occurrence: Diagnosis records
#   - visit_occurrence: Encounter records
#   - measurement: Laboratory results
#   - deriv_* tables: All previously created derived tables
# 
# OUTPUTS:
#   - patient_summary: Comprehensive one-row-per-patient analytical dataset
#     containing 60+ aggregated clinical and social variables
#
# DATA MODEL STRUCTURE:
#   Base table: person (all patients)
#   Join strategy: LEFT JOIN to preserve all patients
#   Missingness: NULL values indicate no records in that domain (not missing data)
#   Granularity: One row per patient (deid_person_id is unique key)
#
# PROCESSING APPROACH:
#   1. Create temporary views for on-the-fly aggregations (not previously derived)
#   2. Join all derived tables and views to person table
#   3. Result is wide analytical dataset ready for statistical analysis
#
# VARIABLE DOMAINS INCLUDED:
#   - Demographics: Gender, race, ethnicity
#   - Mortality: Death date, cause of death
#   - Comorbidities: CCI score, depression, diagnosis counts
#   - Laboratory: A1c, LDL, GFR, albumin, Lp(a)
#   - Medications: Statins, antithrombotics, antihypertensives, antidiabetics
#   - Procedures: Revascularizations (by type), amputations (by level)
#   - Social: Financial strain, smoking, transportation barriers
#   - Utilization: Visit counts and date ranges


# Load required libraries
library(DBI)
library(duckdb)


# Configuration - UPDATE THIS PATH FOR YOUR ENVIRONMENT
duckdb_file <- "path/to/your/vascular.duckdb"

# Connect to DuckDB database
db_con <- dbConnect(duckdb::duckdb(), dbdir = duckdb_file)

# Verify connection
if (!dbIsValid(db_con)) {
  stop("Failed to connect to DuckDB database at: ", duckdb_file)
}

message("Connected to database. Creating patient summary table...")
message("This process creates temporary aggregation views then assembles final table.\n")


# =============================================================================
# SECTION 1: CREATE TEMPORARY AGGREGATION VIEWS
# These views aggregate data that wasn't previously derived as physical tables
# Using temporary views (vs tables) saves storage and ensures fresh aggregation
# =============================================================================

# VIEW 1: Charlson Comorbidity Index with Diagnosis Counts
# Combines CCI scores with overall diagnosis burden metrics. Total diagnosis
# count includes duplicates (repeated diagnoses) while unique count represents
# distinct conditions, providing complementary views of disease complexity.
message("Creating CCI and diagnosis counts view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_cci AS
SELECT 
    cci.*, 
    t.total_dx, 
    t.unique_dx 
FROM deriv_calc_cci AS cci 
LEFT JOIN (
    SELECT 
        deid_person_id, 
        COUNT(condition_source_value) AS total_dx, 
        COUNT(DISTINCT condition_source_value) AS unique_dx 
    FROM condition_occurrence 
    GROUP BY deid_person_id
) AS t
    ON cci.deid_person_id = t.deid_person_id
")


# VIEW 2: Healthcare Utilization - Visit Counts and Date Range
# Aggregates all encounters to characterize healthcare utilization intensity
# and longitudinal engagement. Visit count indicates utilization burden, while
# date range shows duration of observed care. High visit counts may indicate
# disease severity or good access; interpretation requires clinical context.
message("Creating visit utilization view...")

dbExecute(db_con, "
CREATE TEMP VIEW view_visits AS
SELECT 
    deid_person_id,
    COUNT(deid_visit_occurrence_id) AS visit_count,
    MIN(visit_start_date) AS visit_min,
    MAX(visit_start_date) AS visit_max
FROM visit_occurrence
GROUP BY deid_person_id
")


# VIEW 3: Glomerular Filtration Rate (GFR)
# Aggregates kidney function tests. Duplicate of deriv_gfr table logic but
# created as view for consistency with other lab aggregations in this script.
# Provides frequency and trajectory (min/mean/max) of GFR measurements.
message("Creating GFR aggregation view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_gfr AS
WITH gfr_values AS (
    SELECT * 
    FROM measurement
    WHERE measurement_concept_id IN (
        SELECT DISTINCT concept_id 
        FROM deriv_concept_freqs 
        WHERE LOWER(concept_name) LIKE '%glomerular filtration%' 
            AND domain = 'measurement'
    )
)
SELECT DISTINCT 
    deid_person_id, 
    COUNT(measurement_id) AS gfr_freq,
    CAST(MIN(value_as_number) AS DECIMAL(10,2)) AS gfr_min,
    CAST(MAX(value_as_number) AS DECIMAL(10,2)) AS gfr_max,
    CAST(AVG(value_as_number) AS DECIMAL(10,2)) AS gfr_mean
FROM gfr_values
GROUP BY deid_person_id
")


# VIEW 4: Serum Albumin
# Aggregates nutritional/hepatic status marker. Includes temporal range
# (first/last dates) to enable longitudinal analysis of nutritional trajectory.
message("Creating albumin aggregation view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_albumin AS
SELECT DISTINCT 
    deid_person_id, 
    MIN(measurement_date) AS alb_first, 
    MAX(measurement_date) AS alb_last, 
    COUNT(measurement_id) AS alb_freq,
    CAST(MIN(value_as_number) AS DECIMAL(10,2)) AS alb_min,
    CAST(AVG(value_as_number) AS DECIMAL(10,2)) AS alb_mean,
    CAST(MAX(value_as_number) AS DECIMAL(10,2)) AS alb_max
FROM measurement 
WHERE measurement_concept_id IN (3024561)
GROUP BY deid_person_id
")


# VIEW 5: Lipoprotein(a)
# Aggregates genetic cardiovascular risk marker. Frequency typically low as
# Lp(a) is recommended as one-time screening (doesn't change with treatment).
message("Creating lipoprotein(a) aggregation view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_lpa AS
SELECT 
    deid_person_id, 
    COUNT(measurement_id) AS freq_lpa,
    CAST(AVG(value_as_number) AS DECIMAL(10,2)) AS avg_lpa 
FROM measurement 
WHERE measurement_concept_id IN (
    4007663, 46284886, 4197857, 3013861, 3046664
)
    AND value_as_number IS NOT NULL
GROUP BY deid_person_id
")


# VIEW 6: Hemoglobin A1c
# Aggregates diabetes control marker. Temporal range enables assessment of
# monitoring frequency and control trajectory. Filter excludes erroneous values.
message("Creating hemoglobin A1c aggregation view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_a1c AS
SELECT DISTINCT 
    deid_person_id, 
    MIN(measurement_date) AS a1c_first, 
    MAX(measurement_date) AS a1c_last, 
    COUNT(measurement_id) AS a1c_freq,
    CAST(MIN(value_as_number) AS DECIMAL(10,2)) AS a1c_min,
    CAST(AVG(value_as_number) AS DECIMAL(10,2)) AS a1c_mean,
    CAST(MAX(value_as_number) AS DECIMAL(10,2)) AS a1c_max
FROM measurement 
WHERE measurement_concept_id IN (3004410)
    AND value_as_number < 100  -- Exclude erroneous values
GROUP BY deid_person_id
")


# VIEW 7: LDL Cholesterol
# Aggregates primary lipid target for cardiovascular risk reduction. Frequency
# and trajectory inform treatment monitoring and guideline adherence assessment.
message("Creating LDL cholesterol aggregation view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_ldl AS
WITH ldl AS (
    SELECT DISTINCT 
        d.deid_person_id, 
        c.concept_name, 
        d.*
    FROM measurement AS d  
    JOIN concept AS c 
        ON d.measurement_concept_id = c.concept_id
    JOIN deriv_concept_ancestors AS ca 
        ON d.measurement_concept_id = ca.descendant_concept_id
    WHERE ca.ancestor_concept_id = '4012479'
        AND (value_as_number > 0 AND value_as_number < 1000)
) 
SELECT 
    deid_person_id, 
    COUNT(measurement_id) AS ldl_freq,
    CAST(MIN(value_as_number) AS DECIMAL(10,2)) AS ldl_min,
    CAST(MAX(value_as_number) AS DECIMAL(10,2)) AS ldl_max,
    CAST(AVG(value_as_number) AS DECIMAL(10,2)) AS ldl_mean
FROM ldl 
GROUP BY deid_person_id
")


# VIEW 8: Antithrombotic Medications
# Aggregates antiplatelet and anticoagulant prescriptions. Frequency indicates
# treatment continuity; gaps may suggest adherence issues or contraindications.
message("Creating antithrombotic medication aggregation view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_rx_thromb AS
SELECT 
    deid_person_id, 
    COUNT(drug_exposure_id) AS freq_antithromb 
FROM deriv_rx_antithrombotic
GROUP BY deid_person_id
")


# VIEW 9: Statin Medications
# Aggregates lipid-lowering therapy. Date range shows treatment duration;
# frequency indicates refills and monitoring. Essential for guideline adherence.
message("Creating statin medication aggregation view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_rx_statin AS
SELECT 
    deid_person_id, 
    MIN(drug_exposure_start_date) AS min_statin_date,
    MAX(drug_exposure_start_date) AS max_statin_date,
    COUNT(drug_exposure_id) AS freq_statin_drug
FROM deriv_rx_statin
GROUP BY deid_person_id
")


# VIEW 10: Antihypertensive Medications
# Aggregates blood pressure medications. Multiple refills suggest ongoing
# treatment; date range indicates treatment persistence.
message("Creating antihypertensive medication aggregation view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_rx_htn AS
SELECT 
    deid_person_id, 
    MIN(drug_exposure_start_date) AS min_htn_drug,
    MAX(drug_exposure_start_date) AS max_htn_drug,
    COUNT(drug_exposure_id) AS freq_htn_drug
FROM deriv_rx_htn
GROUP BY deid_person_id
")


# VIEW 11: Antidiabetic Medications
# Aggregates glucose-lowering therapy. Treatment intensity and persistence
# inform diabetes management quality assessment.
message("Creating antidiabetic medication aggregation view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_rx_diab AS
SELECT 
    deid_person_id, 
    MIN(drug_exposure_start_date) AS min_diab_drug, 
    MAX(drug_exposure_end_date) AS max_diab_drug,
    COUNT(drug_exposure_id) AS freq_diab_drug
FROM deriv_rx_diab
GROUP BY deid_person_id
")


# VIEW 12: Revascularization Procedures
# Aggregates lower extremity vascular procedures with type-specific counts.
# Multiple procedures may indicate disease severity or initial failure.
# Procedure date range shows timing of interventions.
message("Creating revascularization procedure aggregation view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_revasc AS
SELECT DISTINCT 
    deid_person_id, 
    MIN(procedure_date) AS min_revasc_date, 
    MAX(procedure_date) AS max_revasc_date, 
    COUNT(DISTINCT procedure_date) AS proc_days,
    SUM(dilation) AS dilation,
    SUM(bypass) AS bypass,
    SUM(extirpation) AS extirpation,
    SUM(arterectomy) AS arterectomy,
    SUM(reoperation) AS reoperation,
    SUM(thrombectomy) AS thrombectomy
FROM deriv_revascularization
GROUP BY deid_person_id
")


# VIEW 13: Amputation Procedures
# Aggregates limb loss events by anatomical level. Progressive amputations
# (toe→foot→leg) indicate treatment failure. Date range shows timing relative
# to other interventions.
message("Creating amputation procedure aggregation view...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW view_amp AS
SELECT DISTINCT 
    deid_person_id, 
    MIN(procedure_date) AS min_amp_date, 
    MAX(procedure_date) AS max_amp_date, 
    SUM(amp_toe) AS amp_toe,
    SUM(amp_foot) AS amp_foot,
    SUM(amp_leg) AS amp_leg,
    SUM(amp_revision) AS amp_revision
FROM deriv_amputations
GROUP BY deid_person_id
")


# =============================================================================
# SECTION 2: ASSEMBLE FINAL PATIENT SUMMARY TABLE
# Join all views and derived tables to person table (base)
# LEFT JOINs preserve all patients; NULLs indicate no records in that domain
# =============================================================================

message("\nAssembling final patient summary table...")
message("Joining 15+ data sources into comprehensive patient-level dataset...\n")

dbExecute(db_con, "
CREATE OR REPLACE TABLE patient_summary AS
SELECT 
    -- Demographics and identifiers
    p.deid_person_id,
    p.gender_source_value,
    p.race_concept_id,
    p.ethnicity_concept_id,
    
    -- Mortality
    d.death_date, 
    d.cod1,  -- Cause of death (primary)
    
    -- Mental health comorbidity
    dep.freq_depres,
    
    -- Laboratory results - Hemoglobin A1c (diabetes control)
    a1c.a1c_first,
    a1c.a1c_last,
    a1c.a1c_freq,
    a1c.a1c_min,
    a1c.a1c_mean,
    a1c.a1c_max,
    
    -- Laboratory results - Lipoprotein(a) (genetic CV risk)
    lpa.freq_lpa,
    lpa.avg_lpa,
    
    -- Laboratory results - GFR (kidney function)
    gfr.gfr_freq,
    gfr.gfr_min,
    gfr.gfr_mean,
    gfr.gfr_max,
    
    -- Laboratory results - Albumin (nutrition/liver)
    albumin.alb_freq,
    albumin.alb_min,
    albumin.alb_mean,
    albumin.alb_max,
    
    -- Laboratory results - LDL cholesterol (lipid management)
    ldl.ldl_freq,
    ldl.ldl_min,
    ldl.ldl_mean,
    ldl.ldl_max,
    
    -- Medications - Antithrombotics
    trx.freq_antithromb,
    
    -- Medications - Statins
    srx.min_statin_date,
    srx.max_statin_date,
    srx.freq_statin_drug,
    
    -- Medications - Antihypertensives
    hrx.min_htn_drug,
    hrx.max_htn_drug,
    hrx.freq_htn_drug,
    
    -- Medications - Antidiabetics
    drx.min_diab_drug,
    drx.max_diab_drug,
    drx.freq_diab_drug,
    
    -- Social determinants of health - Financial strain
    deriv_sdoh.freq_finance,
    
    -- Social determinants of health - Smoking
    deriv_smoke.min_smoking,
    deriv_smoke.max_smoking,
    deriv_smoke.freq_smoking,
    
    -- Social determinants of health - Transportation
    deriv_trans.freq_transpo,
    
    -- Procedures - Revascularizations
    rv.min_revasc_date,
    rv.max_revasc_date,
    rv.dilation,
    rv.bypass,
    rv.extirpation,
    rv.arterectomy,
    rv.reoperation,
    rv.thrombectomy,
    
    -- Procedures - Amputations
    amp.min_amp_date,
    amp.max_amp_date,
    amp.amp_toe,
    amp.amp_foot,
    amp.amp_leg,
    amp.amp_revision,
    
    -- Comorbidity and diagnosis burden
    view_cci.cci,
    view_cci.total_dx,
    view_cci.unique_dx,
    
    -- Healthcare utilization
    view_visits.visit_count,
    view_visits.visit_min,
    view_visits.visit_max

FROM person AS p 
LEFT JOIN state_death AS d 
    ON p.deid_person_id = d.deid_person_id
LEFT JOIN deriv_depression AS dep 
    ON p.deid_person_id = dep.deid_person_id
LEFT JOIN view_a1c AS a1c 
    ON p.deid_person_id = a1c.deid_person_id
LEFT JOIN view_lpa AS lpa 
    ON p.deid_person_id = lpa.deid_person_id
LEFT JOIN view_gfr AS gfr 
    ON p.deid_person_id = gfr.deid_person_id
LEFT JOIN view_albumin AS albumin 
    ON p.deid_person_id = albumin.deid_person_id
LEFT JOIN view_ldl AS ldl 
    ON p.deid_person_id = ldl.deid_person_id
LEFT JOIN view_rx_thromb AS trx 
    ON p.deid_person_id = trx.deid_person_id
LEFT JOIN view_rx_statin AS srx 
    ON p.deid_person_id = srx.deid_person_id
LEFT JOIN view_rx_htn AS hrx 
    ON p.deid_person_id = hrx.deid_person_id
LEFT JOIN view_rx_diab AS drx 
    ON p.deid_person_id = drx.deid_person_id
LEFT JOIN deriv_sdoh 
    ON p.deid_person_id = deriv_sdoh.deid_person_id
LEFT JOIN deriv_smoke 
    ON p.deid_person_id = deriv_smoke.deid_person_id
LEFT JOIN deriv_trans 
    ON p.deid_person_id = deriv_trans.deid_person_id
LEFT JOIN view_revasc AS rv 
    ON p.deid_person_id = rv.deid_person_id
LEFT JOIN view_amp AS amp 
    ON p.deid_person_id = amp.deid_person_id
LEFT JOIN view_cci 
    ON p.deid_person_id = view_cci.deid_person_id
LEFT JOIN view_visits 
    ON p.deid_person_id = view_visits.deid_person_id
")


# =============================================================================
# SECTION 3: DATA QUALITY CHECKS AND SUMMARY STATISTICS
# =============================================================================

message("Patient summary table created successfully!")
message("\nGenerating summary statistics and data quality checks...\n")

# Basic table metrics
table_metrics <- dbGetQuery(db_con, "
    SELECT 
        COUNT(*) AS total_patients,
        COUNT(DISTINCT deid_person_id) AS unique_patients
    FROM patient_summary
")
message(sprintf("Table dimensions: %d total rows, %d unique patients", 
                table_metrics$total_patients, table_metrics$unique_patients))

# Column count
col_count <- dbGetQuery(db_con, "
    SELECT COUNT(*) AS n_columns
    FROM information_schema.columns
    WHERE table_name = 'patient_summary'
")
message(sprintf("Number of variables: %d\n", col_count$n_columns))

# Demographics summary
demo_summary <- dbGetQuery(db_con, "
    SELECT 
        COUNT(DISTINCT gender_source_value) AS n_genders,
        COUNT(DISTINCT race_concept_id) AS n_race_categories,
        COUNT(DISTINCT ethnicity_concept_id) AS n_ethnicity_categories
    FROM patient_summary
")
message(sprintf("Demographics: %d gender categories, %d race categories, %d ethnicity categories", 
                demo_summary$n_genders, demo_summary$n_race_categories, 
                demo_summary$n_ethnicity_categories))

# Mortality summary
mortality <- dbGetQuery(db_con, "
    SELECT 
        COUNT(*) AS n_deaths,
        CAST(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM patient_summary) AS DECIMAL(10,2)) AS pct_deaths
    FROM patient_summary
    WHERE death_date IS NOT NULL
")
message(sprintf("Mortality: %d deaths (%s%% of cohort)", 
                mortality$n_deaths, mortality$pct_deaths))

# Key outcome prevalence
outcomes <- dbGetQuery(db_con, "
    SELECT 
        COUNT(CASE WHEN min_revasc_date IS NOT NULL THEN 1 END) AS n_revasc,
        COUNT(CASE WHEN min_amp_date IS NOT NULL THEN 1 END) AS n_amp,
        COUNT(CASE WHEN amp_leg > 0 THEN 1 END) AS n_major_amp
    FROM patient_summary
")
message(sprintf("Procedures: %d with revascularization, %d with any amputation, %d with major amputation\n", 
                outcomes$n_revasc, outcomes$n_amp, outcomes$n_major_amp))

# Laboratory data completeness
lab_completeness <- dbGetQuery(db_con, "
    SELECT 
        COUNT(CASE WHEN a1c_freq IS NOT NULL THEN 1 END) AS n_a1c,
        COUNT(CASE WHEN ldl_freq IS NOT NULL THEN 1 END) AS n_ldl,
        COUNT(CASE WHEN gfr_freq IS NOT NULL THEN 1 END) AS n_gfr,
        COUNT(CASE WHEN alb_freq IS NOT NULL THEN 1 END) AS n_albumin,
        COUNT(CASE WHEN freq_lpa IS NOT NULL THEN 1 END) AS n_lpa,
        CAST(AVG(CASE WHEN a1c_freq IS NOT NULL THEN 1 ELSE 0 END) * 100 AS DECIMAL(10,1)) AS pct_a1c,
        CAST(AVG(CASE WHEN ldl_freq IS NOT NULL THEN 1 ELSE 0 END) * 100 AS DECIMAL(10,1)) AS pct_ldl,
        CAST(AVG(CASE WHEN gfr_freq IS NOT NULL THEN 1 ELSE 0 END) * 100 AS DECIMAL(10,1)) AS pct_gfr
    FROM patient_summary
")
message("Laboratory data completeness:")
message(sprintf("  A1c: %d patients (%s%%)", lab_completeness$n_a1c, lab_completeness$pct_a1c))
message(sprintf("  LDL: %d patients (%s%%)", lab_completeness$n_ldl, lab_completeness$pct_ldl))
message(sprintf("  GFR: %d patients (%s%%)", lab_completeness$n_gfr, lab_completeness$pct_gfr))
message(sprintf("  Albumin: %d patients", lab_completeness$n_albumin))
message(sprintf("  Lp(a): %d patients\n", lab_completeness$n_lpa))

# Medication exposure prevalence
meds <- dbGetQuery(db_con, "
    SELECT 
        COUNT(CASE WHEN freq_statin_drug IS NOT NULL THEN 1 END) AS n_statin,
        COUNT(CASE WHEN freq_antithromb IS NOT NULL THEN 1 END) AS n_antithromb,
        COUNT(CASE WHEN freq_htn_drug IS NOT NULL THEN 1 END) AS n_htn,
        COUNT(CASE WHEN freq_diab_drug IS NOT NULL THEN 1 END) AS n_diab,
        CAST(AVG(CASE WHEN freq_statin_drug IS NOT NULL THEN 1 ELSE 0 END) * 100 AS DECIMAL(10,1)) AS pct_statin,
        CAST(AVG(CASE WHEN freq_antithromb IS NOT NULL THEN 1 ELSE 0 END) * 100 AS DECIMAL(10,1)) AS pct_antithromb
    FROM patient_summary
")
message("Medication exposure:")
message(sprintf("  Statins: %d patients (%s%%)", meds$n_statin, meds$pct_statin))
message(sprintf("  Antithrombotics: %d patients (%s%%)", meds$n_antithromb, meds$pct_antithromb))
message(sprintf("  Antihypertensives: %d patients", meds$n_htn))
message(sprintf("  Antidiabetics: %d patients\n", meds$n_diab))

# Social determinants prevalence
sdoh <- dbGetQuery(db_con, "
    SELECT 
        COUNT(CASE WHEN freq_finance IS NOT NULL THEN 1 END) AS n_finance,
        COUNT(CASE WHEN freq_smoking IS NOT NULL THEN 1 END) AS n_smoking,
        COUNT(CASE WHEN freq_transpo IS NOT NULL THEN 1 END) AS n_transpo
    FROM patient_summary
")
message("Social determinants of health:")
message(sprintf("  Financial strain: %d patients", sdoh$n_finance))
message(sprintf("  Smoking/exposure: %d patients", sdoh$n_smoking))
message(sprintf("  Transportation barriers: %d patients\n", sdoh$n_transpo))

# Comorbidity summary
comorbidity <- dbGetQuery(db_con, "
    SELECT 
        COUNT(CASE WHEN cci IS NOT NULL THEN 1 END) AS n_with_cci,
        CAST(AVG(cci) AS DECIMAL(10,2)) AS mean_cci,
        MIN(cci) AS min_cci,
        MAX(cci) AS max_cci,
        CAST(AVG(total_dx) AS DECIMAL(10,1)) AS mean_total_dx,
        CAST(AVG(unique_dx) AS DECIMAL(10,1)) AS mean_unique_dx
    FROM patient_summary
    WHERE cci IS NOT NULL
")
message("Comorbidity burden:")
message(sprintf("  Patients with CCI: %d", comorbidity$n_with_cci))
message(sprintf("  Mean CCI: %s (range: %d-%d)", 
                comorbidity$mean_cci, comorbidity$min_cci, comorbidity$max_cci))
message(sprintf("  Mean total diagnoses: %s", comorbidity$mean_total_dx))
message(sprintf("  Mean unique diagnoses: %s\n", comorbidity$mean_unique_dx))

# Healthcare utilization
utilization <- dbGetQuery(db_con, "
    SELECT 
        CAST(AVG(visit_count) AS DECIMAL(10,1)) AS mean_visits,
        MIN(visit_count) AS min_visits,
        MAX(visit_count) AS max_visits
    FROM patient_summary
    WHERE visit_count IS NOT NULL
")
message("Healthcare utilization:")
message(sprintf("  Mean visits per patient: %s (range: %d-%d)\n", 
                utilization$mean_visits, utilization$min_visits, utilization$max_visits))


# Optimize database
message("Optimizing database...")
dbExecute(db_con, "CHECKPOINT")
dbExecute(db_con, "VACUUM")


# Close connection
dbDisconnect(db_con)
message("\n=============================================================================")
message("Patient summary table creation complete!")
message("Final analytical dataset ready for statistical analysis.")
message("=============================================================================")
