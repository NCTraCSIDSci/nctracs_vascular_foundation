# Create Medication-Based Derived Tables
# Author(s): Peter Leese
# Started on 2024-12-22
# 
# This script creates derived tables for key medication classes relevant to 
# peripheral vascular disease treatment and related comorbidities. These tables
# facilitate medication exposure assessment and treatment pattern analyses.
# 
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.
#
# INPUTS:
#   - drug_exposure: Medication records in OMOP format
#   - concept: OMOP vocabulary concept definitions
#   - deriv_concept_ancestors: OMOP vocabulary hierarchy (from data loading script)
# 
# OUTPUTS:
#   - deriv_rx_statin: Statin medications (lipid management)
#   - deriv_rx_antithrombotic: Antithrombotic medications (clot prevention)
#   - deriv_rx_htn: Antihypertensive medications (blood pressure management)
#   - deriv_rx_diab: Antidiabetic medications (glucose management)
#
# MEDICATION CLASSES & OMOP CONCEPT IDs:
#   Statins: 1510813, 1545958, 1539403, 1551860, 1592085, 1549686, 40165636
#     (Atorvastatin, Simvastatin, Pravastatin, Lovastatin, Rosuvastatin, 
#      Fluvastatin, Pitavastatin)
#   Antithrombotics: 21600960
#     (Antiplatelet agents, anticoagulants)
#   Antihypertensives: 21600381
#     (ACE inhibitors, ARBs, beta blockers, calcium channel blockers, diuretics)
#   Antidiabetics: 21600712
#     (Insulin, metformin, sulfonylureas, GLP-1 agonists, SGLT2 inhibitors, etc.)


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

message("Connected to database. Creating medication-based derived tables...")


# TABLE 1: Statin Medications
# Statins (HMG-CoA reductase inhibitors) are first-line therapy for lipid 
# management in vascular disease patients. They reduce cardiovascular events
# and may slow atherosclerosis progression.
#
# This table uses OMOP concept ancestors to capture all formulations and 
# combinations of the seven major statins (atorvastatin, simvastatin, 
# pravastatin, lovastatin, rosuvastatin, fluvastatin, pitavastatin)
message("Creating statin medications table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_rx_statin AS
WITH statins AS (
    SELECT de.*
    FROM drug_exposure AS de
    INNER JOIN deriv_concept_ancestors AS ca 
        ON de.drug_concept_id = ca.descendant_concept_id
    WHERE ca.ancestor_concept_id IN (
        1510813,   -- Atorvastatin
        1545958,   -- Simvastatin
        1539403,   -- Pravastatin
        1551860,   -- Lovastatin
        1592085,   -- Rosuvastatin
        1549686,   -- Fluvastatin
        40165636   -- Pitavastatin
    )
)
SELECT DISTINCT 
    drug_exposure_id,
    deid_person_id,
    drug_concept_id,
    drug_exposure_start_date,
    refills,
    quantity,
    days_supply,
    provider_id,
    drug_source_value,
    route_source_value
FROM statins
ORDER BY deid_person_id, drug_exposure_start_date
")

# Verify table creation
statin_count <- dbGetQuery(db_con, "
    SELECT COUNT(*) AS n_prescriptions,
           COUNT(DISTINCT deid_person_id) AS n_patients
    FROM deriv_rx_statin
")
message(sprintf("Statin medications: %d prescriptions for %d patients", 
                statin_count$n_prescriptions, statin_count$n_patients))


# TABLE 2: Antithrombotic Medications
# Antithrombotic medications include antiplatelet agents (e.g., aspirin, 
# clopidogrel) and anticoagulants (e.g., warfarin, DOACs). These are critical
# for preventing arterial thrombosis and cardiovascular events in vascular 
# disease patients.
#
# OMOP ancestor concept 21600960 = "Antithrombotic agent"
message("Creating antithrombotic medications table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_rx_antithrombotic AS
WITH antithrombotic AS (
    SELECT * 
    FROM drug_exposure 
    WHERE drug_concept_id IN (
        SELECT DISTINCT descendant_concept_id 
        FROM deriv_concept_ancestors 
        WHERE ancestor_concept_id = 21600960  -- Antithrombotic agent
    )
)
SELECT DISTINCT 
    drug_exposure_id,
    deid_person_id,
    drug_concept_id,
    drug_exposure_start_date,
    refills,
    quantity,
    days_supply,
    provider_id,
    drug_source_value,
    route_source_value
FROM antithrombotic
ORDER BY deid_person_id, drug_exposure_start_date
")

# Verify table creation
antithromb_count <- dbGetQuery(db_con, "
    SELECT COUNT(*) AS n_prescriptions,
           COUNT(DISTINCT deid_person_id) AS n_patients
    FROM deriv_rx_antithrombotic
")
message(sprintf("Antithrombotic medications: %d prescriptions for %d patients", 
                antithromb_count$n_prescriptions, antithromb_count$n_patients))


# TABLE 3: Antihypertensive Medications
# Hypertension is a major risk factor for vascular disease progression and
# cardiovascular events. Antihypertensive medications include ACE inhibitors,
# ARBs, beta blockers, calcium channel blockers, and diuretics.
#
# OMOP ancestor concept 21600381 = "Antihypertensive agent"
message("Creating antihypertensive medications table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_rx_htn AS
SELECT 
    b.concept_name, 
    a.* 
FROM drug_exposure AS a 
LEFT JOIN concept AS b 
    ON a.drug_concept_id = b.concept_id
WHERE a.drug_concept_id IN (
    SELECT DISTINCT descendant_concept_id 
    FROM deriv_concept_ancestors 
    WHERE ancestor_concept_id = 21600381  -- Antihypertensive agent
)
")

# Verify table creation
htn_count <- dbGetQuery(db_con, "
    SELECT COUNT(*) AS n_prescriptions,
           COUNT(DISTINCT deid_person_id) AS n_patients
    FROM deriv_rx_htn
")
message(sprintf("Antihypertensive medications: %d prescriptions for %d patients", 
                htn_count$n_prescriptions, htn_count$n_patients))


# TABLE 4: Antidiabetic Medications
# Diabetes is a major comorbidity and risk factor for peripheral vascular 
# disease. This table captures all antidiabetic medications including insulin,
# oral agents (metformin, sulfonylureas, etc.), and newer injectable therapies
# (GLP-1 agonists).
#
# OMOP ancestor concept 21600712 = "Drug used in diabetes"
message("Creating antidiabetic medications table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_rx_diab AS
SELECT 
    b.concept_name, 
    a.* 
FROM drug_exposure AS a 
LEFT JOIN concept AS b 
    ON a.drug_concept_id = b.concept_id
WHERE a.drug_concept_id IN (
    SELECT DISTINCT descendant_concept_id 
    FROM deriv_concept_ancestors 
    WHERE ancestor_concept_id = 21600712  -- Drug used in diabetes
)
")

# Verify table creation
diab_count <- dbGetQuery(db_con, "
    SELECT COUNT(*) AS n_prescriptions,
           COUNT(DISTINCT deid_person_id) AS n_patients
    FROM deriv_rx_diab
")
message(sprintf("Antidiabetic medications: %d prescriptions for %d patients", 
                diab_count$n_prescriptions, diab_count$n_patients))


# Display summary of all medication-based derived tables
message("\nSummary of medication-based derived tables:")
med_tables <- dbGetQuery(db_con, "
    SELECT table_name 
    FROM information_schema.tables 
    WHERE table_name LIKE 'deriv_rx_%'
    ORDER BY table_name
")
print(med_tables)


# Display medication class prevalence
message("\nMedication class prevalence:")
prevalence <- dbGetQuery(db_con, "
    SELECT 
        'Statin' AS medication_class,
        COUNT(DISTINCT deid_person_id) AS n_patients
    FROM deriv_rx_statin
    
    UNION ALL
    
    SELECT 
        'Antithrombotic' AS medication_class,
        COUNT(DISTINCT deid_person_id) AS n_patients
    FROM deriv_rx_antithrombotic
    
    UNION ALL
    
    SELECT 
        'Antihypertensive' AS medication_class,
        COUNT(DISTINCT deid_person_id) AS n_patients
    FROM deriv_rx_htn
    
    UNION ALL
    
    SELECT 
        'Antidiabetic' AS medication_class,
        COUNT(DISTINCT deid_person_id) AS n_patients
    FROM deriv_rx_diab
    
    ORDER BY n_patients DESC
")
print(prevalence)


# Optimize database
message("\nOptimizing database...")
dbExecute(db_con, "CHECKPOINT")
dbExecute(db_con, "VACUUM")


# Close connection
dbDisconnect(db_con)
message("Medication-based derived tables created! Database connection closed.")
