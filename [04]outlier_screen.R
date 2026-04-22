# Screen outliers
# Author(s): Tomas McIntee
# Last updated: 2025-04-17
# 
# This script creates the final analytical dataset by assembling a comprehensive
# patient-level summary table. Each row represents one patient with aggregated
# information from all clinical domains: demographics, conditions, procedures,
# medications, laboratory results, and social determinants of health.
# 
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.
require(duckdb)

con <- dbConnect(duckdb::duckdb(), dbdir = "Z:\\vascular.duckdb")

# ---------------------------------------------------------------------------
# Identify outliers (born 1925 or earlier)
# ---------------------------------------------------------------------------
dbExecute(con, "
    CREATE OR REPLACE TEMP VIEW outliers_time AS
    SELECT *
    FROM person
    WHERE year_of_birth <= 1925
")

# ---------------------------------------------------------------------------
# Patient summary without outliers
# ---------------------------------------------------------------------------
dbExecute(con, "
    CREATE OR REPLACE TABLE patient_summary_no_outliers AS
    (SELECT *
    FROM patient_summary
    WHERE DEID_PERSON_ID NOT IN (SELECT DEID_PERSON_ID FROM outliers_time)
        AND min_amp_date IS NOT NULL
    LIMIT 3500)
    UNION
    (SELECT *
    FROM patient_summary
    WHERE DEID_PERSON_ID NOT IN (SELECT DEID_PERSON_ID FROM outliers_time)
        AND min_amp_date IS NULL
        AND min_revasc_date IS NOT NULL
    LIMIT 3500)
    UNION
    (SELECT *
    FROM patient_summary
    WHERE DEID_PERSON_ID NOT IN (SELECT DEID_PERSON_ID FROM outliers_time)
        AND min_amp_date IS NULL
        AND min_revasc_date IS NULL
    LIMIT 10000)")
# The limit and subselection is for test runs on small data.
# Adjust or remove as appropriate.
# ---------------------------------------------------------------------------
# Gender counts
# ---------------------------------------------------------------------------
gender_counts <- dbGetQuery(con, "
    SELECT
        COUNT(CASE WHEN GENDER_SOURCE_VALUE = 'F' THEN 1 END) AS female,
        COUNT(CASE WHEN GENDER_SOURCE_VALUE = 'M' THEN 1 END) AS male
    FROM person")
print(gender_counts)

# ---------------------------------------------------------------------------
# Condition visit dates
# ---------------------------------------------------------------------------
dbExecute(con, "
    CREATE OR REPLACE TABLE condition_visit_dates AS
    SELECT DISTINCT
        patient_summary_no_outliers.DEID_PERSON_ID,
        concept.CONCEPT_NAME,
        concept.CONCEPT_ID,
        condition_occurrence.DEID_VISIT_OCCURRENCE_ID,
        condition_occurrence.CONDITION_OCCURRENCE_ID,
        VISIT_START_DATE,
        VISIT_END_DATE
    FROM patient_summary_no_outliers
    INNER JOIN condition_occurrence
        ON patient_summary_no_outliers.DEID_PERSON_ID = condition_occurrence.DEID_PERSON_ID
    LEFT JOIN visit_occurrence
        ON visit_occurrence.DEID_VISIT_OCCURRENCE_ID = condition_occurrence.DEID_VISIT_OCCURRENCE_ID
    INNER JOIN concept
        ON condition_occurrence.CONDITION_CONCEPT_ID = concept.CONCEPT_ID")

# ---------------------------------------------------------------------------
# Amputation procedures (AMP)
# ---------------------------------------------------------------------------
amp_concepts <- paste(c(
    2105806,4272232,4338257,4195136,4302020,4143797,2104720,44816423,2105450,4159766,2101638,2105448,2105223,2104721,
    2006243,2104719,2105449,4054983,4143795,4195136,4264289,4054983,2105211,4078404,4228939,4217608,2750427,4297321,
    2105222,2750729,2749720,40480578,2104036,4074138,2749719,4013504,42628022,2105447,4300370,4167170,4264289,4030398,
    2105451,4306196,2750426,4244214,2006242,1531713,4306760,36675618,2750722,2750985,4216997,4223824,4300370,4143795,
    4196243,2750725,2101620,4078558,4231940,2104352
), collapse = ",")

dbExecute(con, sprintf("
    CREATE OR REPLACE TEMP VIEW amp_fixed AS
    SELECT DISTINCT
        DEID_PERSON_ID,
        COUNT(procedure_concept_id) AS count_amp,
        COUNT(DISTINCT PROCEDURE_DATE) AS fixed_count,
        MIN(PROCEDURE_DATE) AS first_amp,
        MAX(PROCEDURE_DATE) AS last_amp,
        STRING_AGG(PROCEDURE_DATE::VARCHAR, ',') AS amp_date_list
    FROM procedure_occurrence
    WHERE PROCEDURE_CONCEPT_ID IN (%s)
    GROUP BY DEID_PERSON_ID
", amp_concepts))

# ---------------------------------------------------------------------------
# Revascularisation procedures (REVASC)
# ---------------------------------------------------------------------------
revasc_concepts <- paste(c(
    40756934,40756856,40757050,40756927,40756872,43528002,40756958,43528000,40756946,40757117,40757057,40757135,43528003,
    40757100,43533186,801208,40757029,801207,43533354,40757128,40757054,40756997,801204,43533223,2108302,801206,43533187,
    43528001,801205,43528004,801203,801391,2106965,2001517,40757094,43533242
), collapse = ",")

dbExecute(con, sprintf("
    CREATE OR REPLACE TEMP VIEW revasc_fixed AS
    SELECT DISTINCT
        DEID_PERSON_ID,
        COUNT(procedure_concept_id) AS count_revasc,
        COUNT(DISTINCT PROCEDURE_DATE) AS fixed_count,
        MIN(PROCEDURE_DATE) AS first_revasc,
        MAX(PROCEDURE_DATE) AS last_revasc,
        STRING_AGG(DISTINCT PROCEDURE_DATE::VARCHAR, ',') AS revasc_date_list
    FROM procedure_occurrence
    WHERE PROCEDURE_CONCEPT_ID IN (%s)
    GROUP BY DEID_PERSON_ID
", revasc_concepts))

# Check revasc patients with >10 procedure dates
revasc_outliers <- dbGetQuery(con, "
    SELECT * FROM revasc_fixed WHERE fixed_count > 10
")
print(revasc_outliers)

# ---------------------------------------------------------------------------
# Year of birth distribution
# ---------------------------------------------------------------------------
yob_dist <- dbGetQuery(con, "
    SELECT year_of_birth, COUNT(*) AS n
    FROM person
    GROUP BY year_of_birth
    ORDER BY year_of_birth ASC
")
print(yob_dist)

# ---------------------------------------------------------------------------
# Outlier patient conditions (for review)
# ---------------------------------------------------------------------------
outlier_conditions <- dbGetQuery(con, "
    SELECT
        outliers_time.*,
        CONCEPT_NAME,
        CONDITION_START_DATE,
        CONDITION_END_DATE,
        VISIT_START_DATE,
        VISIT_END_DATE,
        CONDITION_STATUS_SOURCE_VALUE
    FROM outliers_time
    INNER JOIN condition_occurrence
        ON outliers_time.DEID_PERSON_ID = condition_occurrence.DEID_PERSON_ID
    LEFT JOIN visit_occurrence
        ON visit_occurrence.DEID_VISIT_OCCURRENCE_ID = condition_occurrence.DEID_VISIT_OCCURRENCE_ID
    INNER JOIN concept
        ON condition_occurrence.CONDITION_CONCEPT_ID = concept.CONCEPT_ID
    ORDER BY outliers_time.DEID_PERSON_ID ASC, CONDITION_START_DATE ASC
")
print(outlier_conditions)

# ---------------------------------------------------------------------------
# AMP duplicate procedure dates per patient/concept
# ---------------------------------------------------------------------------
dbExecute(con, sprintf("
    CREATE OR REPLACE TEMP VIEW amp_double AS
    SELECT DISTINCT
        DEID_PERSON_ID,
        procedure_concept_id,
        concept_name,
        COUNT(DISTINCT PROCEDURE_DATE) AS duplicate_procedures
    FROM procedure_occurrence
    INNER JOIN concept ON procedure_concept_id = CONCEPT_ID
    WHERE PROCEDURE_CONCEPT_ID IN (%s)
    GROUP BY DEID_PERSON_ID, procedure_concept_id, CONCEPT_NAME
    HAVING COUNT(DISTINCT PROCEDURE_DATE) > 1
", amp_concepts))

amp_double_summary <- dbGetQuery(con, "
    SELECT
        COUNT(*) AS patients,
        duplicate_procedures,
        concept_name
    FROM amp_double
    GROUP BY concept_name, duplicate_procedures
    ORDER BY duplicate_procedures DESC, patients DESC
")
print(amp_double_summary)
dbExecute(con, "CHECKPOINT")
dbExecute(con, "VACUUM")
dbDisconnect(con,  shutdown="TRUE")
