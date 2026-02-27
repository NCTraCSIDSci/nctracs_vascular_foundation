# Create Condition-Based Derived Tables
# Author(s): Peter Leese
# Started on 2024-12-22
# 
# This script creates derived tables for condition-based analyses including:
# 1. Vascular diagnoses that defined cohort inclusion criteria
# 2. Depression comorbidity frequency per patient
# 
# These tables support phenotyping and comorbidity assessment in downstream analyses.
# 
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.
#
# INPUTS:
#   - condition_occurrence: Patient diagnosis records in OMOP format
#   - concept: OMOP vocabulary concept definitions
#   - deriv_concept_ancestors: OMOP vocabulary hierarchy (from data loading script)
# 
# OUTPUTS:
#   - deriv_vascular_dx: Vascular diagnoses that qualified patients for cohort
#   - deriv_depression: Depression diagnosis frequency per patient


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

message("Connected to database. Creating condition-based derived tables...")


# TABLE 1: Vascular Diagnoses (Cohort Inclusion Criteria)
# This table identifies the specific vascular diagnoses that qualified each
# patient for inclusion in the study cohort. The ICD-10 codes capture:
# - Peripheral arterial disease (I70.x, I73.x, I77.x)
# - Arterial embolism and thrombosis (I74.x, I75.x, I76.x)
# - Diabetes with circulatory complications (E08.5-E13.5)
# - Chronic lower limb ulcers (L97.x)
# - Vascular insufficiency (I96.x)
# 
# Exclusions: Aortic aneurysms (I71.x, I72.x) and aortic atherosclerosis (I70.1)
# are excluded as they represent different clinical conditions
# 
# The row_number field ranks diagnoses chronologically per patient, allowing
# identification of the first qualifying diagnosis (index date)
message("Creating vascular diagnosis table...")

dbExecute(db_con, "          
CREATE OR REPLACE TABLE deriv_vascular_dx AS

SELECT 
    ROW_NUMBER() OVER(
        PARTITION BY deid_person_id 
        ORDER BY condition_start_date
    ) AS row_number, 
    x.* 
FROM (
    SELECT * 
    FROM condition_occurrence
    WHERE 
        -- Specific ICD-10 codes for vascular conditions
        condition_source_value IN (
            'I73.89',  -- Other specified peripheral vascular diseases
            'I73.9',   -- Peripheral vascular disease, unspecified
            'I77.70',  -- Dissection of unspecified artery
            'I77.72',  -- Dissection of iliac artery
            'I77.76',  -- Dissection of artery of lower extremity
            'I77.77',  -- Dissection of other specified arteries
            'I77.79',  -- Dissection of other specified artery
            'I79.8'    -- Other disorders of arteries
        )
        -- Diabetes with circulatory complications (all types)
        OR SUBSTR(condition_source_value, 1, 5) IN (
            'E08.5',   -- Diabetes due to underlying condition with circulatory complications
            'E09.5',   -- Drug/chemical induced diabetes with circulatory complications
            'E10.5',   -- Type 1 diabetes with circulatory complications
            'E11.5',   -- Type 2 diabetes with circulatory complications
            'E13.5'    -- Other specified diabetes with circulatory complications
        )
        -- Broad vascular disease categories
        OR SUBSTR(condition_source_value, 1, 3) IN (
            'I70',     -- Atherosclerosis
            'I74',     -- Arterial embolism and thrombosis
            'I75',     -- Atheroembolism
            'I76',     -- Septic arterial embolism
            'I96',     -- Gangrene, not elsewhere classified
            'L97'      -- Non-pressure chronic ulcer of lower limb
        )
        -- Exclude aortic conditions and aortic atherosclerosis
        AND SUBSTR(condition_source_value, 1, 3) NOT IN ('I71', 'I72')
        AND condition_source_value NOT IN ('I70.1')
    ORDER BY deid_person_id, condition_start_date
) AS x
")

# Verify table creation
vasc_dx_count <- dbGetQuery(db_con, "
    SELECT COUNT(*) AS n_records,
           COUNT(DISTINCT deid_person_id) AS n_patients
    FROM deriv_vascular_dx
")
message(sprintf("Vascular diagnoses: %d records for %d patients", 
                vasc_dx_count$n_records, vasc_dx_count$n_patients))


# TABLE 2: Depression Comorbidity
# This table identifies patients with depression diagnoses using the OMOP
# vocabulary hierarchy. The ancestor_concept_id 440383 represents "Depressive
# disorder" in OMOP, which includes all descendant concepts (specific types
# of depression: major depressive disorder, dysthymia, etc.)
# 
# The freq_depres field counts the number of distinct dates with depression
# diagnoses, which can serve as a proxy for depression severity or chronicity
message("Creating depression comorbidity table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_depression AS
SELECT DISTINCT 
    deid_person_id, 
    COUNT(DISTINCT condition_start_date) AS freq_depres 
FROM (
    SELECT 
        b.concept_name, 
        a.* 
    FROM condition_occurrence AS a 
    LEFT JOIN concept AS b 
        ON a.condition_concept_id = b.concept_id
    WHERE a.condition_concept_id IN (
        -- Get all depression concepts using OMOP hierarchy
        -- Ancestor concept 440383 = "Depressive disorder"
        SELECT DISTINCT descendant_concept_id 
        FROM deriv_concept_ancestors 
        WHERE ancestor_concept_id = 440383
    )
) AS x 
GROUP BY deid_person_id
")

# Verify table creation
depression_count <- dbGetQuery(db_con, "
    SELECT COUNT(DISTINCT deid_person_id) AS n_patients,
           CAST(AVG(freq_depres) AS DECIMAL(10,2)) AS mean_freq,
           MAX(freq_depres) AS max_freq
    FROM deriv_depression
")
message(sprintf("Depression comorbidity: %d patients (mean frequency: %s, max: %d)", 
                depression_count$n_patients, 
                depression_count$mean_freq,
                depression_count$max_freq))


# Display summary of all derived tables
message("\nSummary of condition-based derived tables:")
all_tables <- dbGetQuery(db_con, "
    SELECT table_name 
    FROM information_schema.tables 
    WHERE table_name LIKE 'deriv_%'
    ORDER BY table_name
")
print(all_tables)


# Optimize database
message("\nOptimizing database...")
dbExecute(db_con, "CHECKPOINT")
dbExecute(db_con, "VACUUM")


# Close connection
dbDisconnect(db_con)
message("Condition-based derived tables created! Database connection closed.")
