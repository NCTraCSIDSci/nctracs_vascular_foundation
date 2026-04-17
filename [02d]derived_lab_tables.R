# Create Laboratory Measurement Derived Tables
# Author(s): Peter Leese
# Started on 2024-12-22
# 
# This script creates derived tables aggregating key laboratory measurements for
# each patient. These tables support risk stratification, comorbidity assessment,
# and treatment monitoring analyses in peripheral vascular disease patients.
# 
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.
#
# INPUTS:
#   - measurement: Laboratory results in OMOP format
#   - concept: OMOP vocabulary concept definitions
#   - deriv_concept_freqs: Concept frequency table (from data loading script)
#   - deriv_concept_ancestors: OMOP vocabulary hierarchy (from data loading script)
# 
# OUTPUTS:
#   - deriv_gfr: Glomerular filtration rate (kidney function)
#   - deriv_albumin: Serum albumin (nutritional status, liver function)
#   - deriv_lpa: Lipoprotein(a) (cardiovascular risk marker)
#   - deriv_a1c: Hemoglobin A1c (diabetes control)
#   - deriv_ldl: LDL cholesterol (cardiovascular risk)
#
# LABORATORY TESTS & OMOP CONCEPT IDs:
#   GFR: Identified by concept name pattern '%glomerular filtration%'
#   Albumin: 3024561 (Albumin [Mass/volume] in Serum or Plasma)
#   Lipoprotein(a): 4007663, 46284886, 4197857, 3013861, 3046664
#   Hemoglobin A1c: 3004410 (Hemoglobin A1c/Hemoglobin.total in Blood)
#   LDL Cholesterol: Ancestor 4012479 (LDL cholesterol measurement)
#
# CLINICAL REFERENCE RANGES:
#   GFR: >60 mL/min/1.73m² normal; <60 indicates chronic kidney disease
#   Albumin: 3.5-5.5 g/dL normal; <3.5 indicates hypoalbuminemia
#   Lipoprotein(a): <30 mg/dL desirable; >50 mg/dL high risk
#   A1c: <5.7% normal; 5.7-6.4% prediabetes; ≥6.5% diabetes
#   LDL: <100 mg/dL optimal; <70 mg/dL goal for vascular disease patients


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

message("Connected to database. Creating laboratory measurement derived tables...")


# TABLE 1: Glomerular Filtration Rate (GFR)
# GFR is the primary measure of kidney function and is critical in vascular
# disease management. Chronic kidney disease (CKD) affects contrast imaging
# decisions, medication dosing, and cardiovascular risk. This table aggregates
# all GFR measurements to characterize each patient's renal function trajectory.
#
# Normal GFR: >60 mL/min/1.73m²
# CKD Stage 3: 30-59 (moderate kidney disease)
# CKD Stage 4: 15-29 (severe kidney disease)  
# CKD Stage 5: <15 (kidney failure)
#
# Uses pattern matching on concept names to capture various GFR calculation
# methods (MDRD, CKD-EPI, etc.)
message("Creating GFR (kidney function) table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_gfr AS
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

# Verify table creation
gfr_count <- dbGetQuery(db_con, "
    SELECT COUNT(DISTINCT deid_person_id) AS n_patients,
           CAST(AVG(gfr_mean) AS DECIMAL(10,2)) AS mean_gfr,
           SUM(CASE WHEN gfr_mean < 60 THEN 1 ELSE 0 END) AS n_ckd_stage3plus
    FROM deriv_gfr
")
message(sprintf("GFR: %d patients (mean GFR: %s mL/min/1.73m², %d with CKD stage 3+)", 
                gfr_count$n_patients, gfr_count$mean_gfr, gfr_count$n_ckd_stage3plus))


# TABLE 2: Serum Albumin
# Albumin is a multifunctional protein synthesized by the liver. Low albumin
# (hypoalbuminemia) indicates malnutrition, liver disease, kidney disease, or
# systemic inflammation. In vascular disease patients, albumin levels predict
# wound healing capacity and surgical outcomes. This table tracks albumin
# trends over time for each patient.
#
# Normal: 3.5-5.5 g/dL
# Mild hypoalbuminemia: 3.0-3.5 g/dL
# Moderate hypoalbuminemia: 2.5-3.0 g/dL
# Severe hypoalbuminemia: <2.5 g/dL (high mortality risk)
message("Creating serum albumin table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_albumin AS
SELECT DISTINCT 
    deid_person_id, 
    MIN(measurement_date) AS alb_first, 
    MAX(measurement_date) AS alb_last, 
    COUNT(measurement_id) AS alb_freq,
    CAST(MIN(value_as_number) AS DECIMAL(10,2)) AS alb_min,
    CAST(AVG(value_as_number) AS DECIMAL(10,2)) AS alb_mean,
    CAST(MAX(value_as_number) AS DECIMAL(10,2)) AS alb_max
FROM measurement 
WHERE measurement_concept_id IN (
    3024561  -- Albumin [Mass/volume] in Serum or Plasma
)
GROUP BY deid_person_id
")

# Verify table creation
alb_count <- dbGetQuery(db_con, "
    SELECT COUNT(DISTINCT deid_person_id) AS n_patients,
           CAST(AVG(alb_mean) AS DECIMAL(10,2)) AS mean_albumin,
           SUM(CASE WHEN alb_mean < 3.5 THEN 1 ELSE 0 END) AS n_hypoalbuminemia
    FROM deriv_albumin
")
message(sprintf("Albumin: %d patients (mean: %s g/dL, %d with hypoalbuminemia)", 
                alb_count$n_patients, alb_count$mean_albumin, alb_count$n_hypoalbuminemia))


# TABLE 3: Lipoprotein(a)
# Lipoprotein(a) [Lp(a)] is a genetically determined cardiovascular risk factor
# that is largely unresponsive to lifestyle modifications or most lipid therapies.
# Elevated Lp(a) may explain atherosclerotic disease in patients without 
# traditional risk factors. This table identifies patients with measured Lp(a)
# for risk stratification.
#
# Desirable: <30 mg/dL
# Borderline: 30-50 mg/dL
# High risk: >50 mg/dL (2-4x increased cardiovascular risk)
#
# Note: Lp(a) can be measured by mass (mg/dL) or molar concentration (nmol/L).
# This table captures both measurement types.
message("Creating lipoprotein(a) table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_lpa AS
SELECT 
    deid_person_id, 
    COUNT(measurement_id) AS freq_lpa,
    CAST(AVG(value_as_number) AS DECIMAL(10,2)) AS avg_lpa
FROM measurement 
WHERE measurement_concept_id IN (
    4007663,   -- Lipoprotein(a) [Mass/volume] in Serum or Plasma
    46284886,  -- Lipoprotein(a) [Moles/volume] in Serum or Plasma
    4197857,   -- Lipoprotein(a) cholesterol [Mass/volume] in Serum or Plasma
    3013861,   -- Lipoprotein(a) [Mass/volume] in Serum or Plasma by Immunoassay
    3046664    -- Lipoprotein(a) [Moles/volume] in Serum or Plasma by Immunoassay
)
    AND value_as_number IS NOT NULL
GROUP BY deid_person_id
")

# Verify table creation
lpa_count <- dbGetQuery(db_con, "
    SELECT COUNT(DISTINCT deid_person_id) AS n_patients,
           CAST(AVG(avg_lpa) AS DECIMAL(10,2)) AS mean_lpa,
           SUM(CASE WHEN avg_lpa > 50 THEN 1 ELSE 0 END) AS n_high_lpa
    FROM deriv_lpa
")
message(sprintf("Lipoprotein(a): %d patients (mean: %s mg/dL, %d with high Lp(a) >50)", 
                lpa_count$n_patients, lpa_count$mean_lpa, lpa_count$n_high_lpa))


# TABLE 4: Hemoglobin A1c
# Hemoglobin A1c (HbA1c) reflects average blood glucose over the preceding 
# 2-3 months and is the gold standard for diabetes diagnosis and monitoring.
# Diabetes is a major driver of peripheral vascular disease, and poor glycemic
# control accelerates disease progression. This table tracks A1c trends to
# assess diabetes presence and control quality.
#
# Normal: <5.7%
# Prediabetes: 5.7-6.4%
# Diabetes: ≥6.5%
# Good control: <7.0%
# Poor control: >9.0%
#
# Data quality filter: A1c values >100% are biologically implausible and likely
# represent data entry errors (e.g., glucose values coded as A1c) or decimal
# point errors. Maximum physiologic A1c is approximately 15-20%.
message("Creating hemoglobin A1c table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_a1c AS
SELECT DISTINCT 
    deid_person_id, 
    MIN(measurement_date) AS a1c_first, 
    MAX(measurement_date) AS a1c_last, 
    COUNT(measurement_id) AS a1c_freq,
    CAST(MIN(value_as_number) AS DECIMAL(10,2)) AS a1c_min,
    CAST(AVG(value_as_number) AS DECIMAL(10,2)) AS a1c_mean,
    CAST(MAX(value_as_number) AS DECIMAL(10,2)) AS a1c_max
FROM measurement 
WHERE measurement_concept_id IN (
    3004410  -- Hemoglobin A1c/Hemoglobin.total in Blood
)
    AND value_as_number < 100  -- Exclude erroneous values
GROUP BY deid_person_id
")

# Verify table creation
a1c_count <- dbGetQuery(db_con, "
    SELECT COUNT(DISTINCT deid_person_id) AS n_patients,
           CAST(AVG(a1c_mean) AS DECIMAL(10,2)) AS mean_a1c,
           SUM(CASE WHEN a1c_mean >= 6.5 THEN 1 ELSE 0 END) AS n_diabetes,
           SUM(CASE WHEN a1c_mean >= 9.0 THEN 1 ELSE 0 END) AS n_poor_control
    FROM deriv_a1c
")
message(sprintf("Hemoglobin A1c: %d patients (mean: %s%%, %d diabetes, %d poor control)", 
                a1c_count$n_patients, a1c_count$mean_a1c, 
                a1c_count$n_diabetes, a1c_count$n_poor_control))


# TABLE 5: LDL Cholesterol
# Low-density lipoprotein (LDL) cholesterol is the primary target of lipid-
# lowering therapy and a modifiable risk factor for atherosclerotic disease.
# Current guidelines recommend LDL <70 mg/dL for patients with established
# vascular disease (some recommend <55 mg/dL for very high-risk patients).
# This table aggregates LDL measurements for treatment monitoring.
#
# Optimal: <100 mg/dL
# Near optimal: 100-129 mg/dL
# Borderline high: 130-159 mg/dL
# High: 160-189 mg/dL
# Very high: ≥190 mg/dL
# Goal for vascular disease: <70 mg/dL
#
# Data quality filters:
# - Values must be >0 (negative values are impossible)
# - Values must be <1000 (values >1000 likely represent unit conversion errors
#   from mmol/L to mg/dL, or data entry errors)
#
# Uses OMOP ancestor concept 4012479 to capture all LDL measurement methods
# (calculated Friedewald equation, direct immunoassay, etc.)
message("Creating LDL cholesterol table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_ldl AS
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
    WHERE ca.ancestor_concept_id = '4012479'  -- LDL cholesterol measurement
        AND (value_as_number > 0 AND value_as_number < 1000)  -- Exclude erroneous values
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

# Verify table creation
ldl_count <- dbGetQuery(db_con, "
    SELECT COUNT(DISTINCT deid_person_id) AS n_patients,
           CAST(AVG(ldl_mean) AS DECIMAL(10,2)) AS mean_ldl,
           SUM(CASE WHEN ldl_mean < 70 THEN 1 ELSE 0 END) AS n_at_goal,
           SUM(CASE WHEN ldl_mean >= 160 THEN 1 ELSE 0 END) AS n_high_ldl
    FROM deriv_ldl
")
message(sprintf("LDL cholesterol: %d patients (mean: %s mg/dL, %d at goal <70, %d high ≥160)", 
                ldl_count$n_patients, ldl_count$mean_ldl, 
                ldl_count$n_at_goal, ldl_count$n_high_ldl))


# Display summary of all laboratory-based derived tables
message("\nSummary of laboratory measurement derived tables:")
lab_tables <- dbGetQuery(db_con, "
    SELECT table_name 
    FROM information_schema.tables 
    WHERE table_name IN ('deriv_gfr', 'deriv_albumin', 'deriv_lpa', 
                         'deriv_a1c', 'deriv_ldl')
    ORDER BY table_name
")
print(lab_tables)


# Display laboratory measurement coverage and key statistics
message("\nLaboratory measurement coverage and key statistics:")
coverage <- dbGetQuery(db_con, "
    SELECT 
        'GFR (Kidney Function)' AS lab_type,
        COUNT(DISTINCT deid_person_id) AS n_patients,
        CAST(AVG(gfr_mean) AS DECIMAL(10,2)) AS mean_value,
        'mL/min/1.73m²' AS units
    FROM deriv_gfr
    
    UNION ALL
    
    SELECT 
        'Albumin' AS lab_type,
        COUNT(DISTINCT deid_person_id) AS n_patients,
        CAST(AVG(alb_mean) AS DECIMAL(10,2)) AS mean_value,
        'g/dL' AS units
    FROM deriv_albumin
    
    UNION ALL
    
    SELECT 
        'Lipoprotein(a)' AS lab_type,
        COUNT(DISTINCT deid_person_id) AS n_patients,
        CAST(AVG(avg_lpa) AS DECIMAL(10,2)) AS mean_value,
        'mg/dL' AS units
    FROM deriv_lpa
    
    UNION ALL
    
    SELECT 
        'Hemoglobin A1c' AS lab_type,
        COUNT(DISTINCT deid_person_id) AS n_patients,
        CAST(AVG(a1c_mean) AS DECIMAL(10,2)) AS mean_value,
        '%' AS units
    FROM deriv_a1c
    
    UNION ALL
    
    SELECT 
        'LDL Cholesterol' AS lab_type,
        COUNT(DISTINCT deid_person_id) AS n_patients,
        CAST(AVG(ldl_mean) AS DECIMAL(10,2)) AS mean_value,
        'mg/dL' AS units
    FROM deriv_ldl
    
    ORDER BY n_patients DESC
")
print(coverage)


# Optimize database
message("\nOptimizing database...")
dbExecute(db_con, "CHECKPOINT")
dbExecute(db_con, "VACUUM")


# Close connection
dbDisconnect(db_con)
message("Laboratory measurement derived tables created! Database connection closed.")
