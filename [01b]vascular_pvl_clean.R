# Parse PVL Narrative Measurements
# Author(s): Peter Leese
# Started on 2024-12-30
# 
# This file parses PVL (Peripheral Vascular Lab) narrative text to extract 
# discrete float measurements and attribute them to anatomical locations. The 
# parsing uses regex patterns and rules to handle various text permutations.
# 
# Note: This rule-based approach handles many common cases but may miss edge 
# cases. A large language model (LLM) approach could provide more robust parsing 
# in future iterations and is now be feasible with foundation models in 
# Databricks or small models in Jupyter notebooks.
# 
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.
#
# INPUTS:
#   - pvl_measurements table in DuckDB database (created by data loading script)
# 
# OUTPUTS:
#   - deriv_pvl_final: Cleaned table with parsed measurements, anatomical 
#     locations, and laterality
#
# PROCESSING APPROACH:
#   Uses temporary views for intermediate steps to avoid creating unnecessary 
#   physical tables. Only the final cleaned result is persisted as a physical 
#   table for downstream analysis.


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

message("Connected to database. Beginning PVL narrative parsing...")


# STEP 1: Initial parsing and feature extraction
# Extract float values, identify anatomical locations, and determine laterality
# from narrative text using regex patterns
message("Step 1: Extracting features from narrative text...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW pvl_1 AS
SELECT
    deid_person_id,
    proc_start_date,
    resulting_lab_name,
    description,
    narrative,

    -- Flag records containing float values
    CASE 
        WHEN regexp_matches(narrative, '\\d+\\.\\d+') THEN 1 
        ELSE 0 
    END AS float_check,

    -- Categorize narrative type
    CASE 
        WHEN LOWER(narrative) LIKE '%other%' THEN 'other' 
        ELSE 'measurement' 
    END AS type,

    -- Extract text following 'right' and 'left' keywords for laterality detection
    regexp_extract(LOWER(narrative), 'right\\s+(\\S+)', 1) AS right,
    regexp_extract(LOWER(narrative), 'left\\s+(\\S+)', 1) AS left,

    -- Flag unobtainable measurements by side
    -- These represent attempted measurements that could not be obtained
    CASE 
        WHEN LOWER(regexp_extract(LOWER(narrative), 'right\\s+(\\S+)', 1)) 
            LIKE '%unobtain%' THEN 999 
    END AS rt_unobtainable,

    CASE 
        WHEN LOWER(regexp_extract(LOWER(narrative), 'left\\s+(\\S+)', 1)) 
            LIKE '%unobtain%' THEN 999 
    END AS lt_unobtainable,

    -- Identify anatomical location based on keywords
    CASE 
        WHEN LOWER(narrative) LIKE '%ankle%' 
            OR LOWER(narrative) LIKE '%abi%' 
            OR LOWER(narrative) LIKE '%tibial%' 
            OR LOWER(narrative) LIKE '%peroneal%' 
            OR LOWER(narrative) LIKE '%dorsalis%' 
            OR LOWER(narrative) LIKE '%pta%'
            OR LOWER(narrative) LIKE '%ata%' 
            OR LOWER(narrative) LIKE '%dp%' THEN 'ankle'
        WHEN LOWER(narrative) LIKE '%brachial%' THEN 'arm'
        WHEN LOWER(narrative) LIKE '%toe%' 
            OR LOWER(narrative) LIKE '%digit%' THEN 'toe'
    END AS anatomy,

    -- Identify specific anatomical structures for ankle measurements
    CASE 
        WHEN LOWER(narrative) LIKE '%tibial%' THEN 'tibial'
        WHEN LOWER(narrative) LIKE '%peroneal%' THEN 'peroneal'
        WHEN LOWER(narrative) LIKE '%dorsalis%' THEN 'dorsalis'
        WHEN LOWER(narrative) LIKE '%abi%' THEN 'abi'
    END AS specific_anatomy,

    -- Extract laterality from structured description field
    CASE 
        WHEN description LIKE '%RIGHT%' THEN 'right'
        WHEN description LIKE '%LEFT%' THEN 'left'
        WHEN description LIKE '%BILAT%' THEN 'bilateral'
    END AS desc_laterality,

    -- Flag any unobtainable measurement
    CASE 
        WHEN LOWER(narrative) LIKE '%unobtainable%' THEN 1 
        ELSE 0 
    END AS unobtainable,

    -- Extract laterality from narrative text
    CASE 
        WHEN LOWER(narrative) LIKE '%right%' 
            OR LOWER(narrative) LIKE '%rt%' 
            OR narrative LIKE '%R %' THEN 'right'
    END AS narr_right,

    CASE 
        WHEN LOWER(narrative) LIKE '%left%' 
            OR LOWER(narrative) LIKE '%lt%' 
            OR narrative LIKE '%L %' THEN 'left'
    END AS narr_left,

    -- Extract numeric values and create array for easier access
    extracted_float,
    array_length(extracted_float) AS array_length,
    extracted_float[1] AS value1,
    extracted_float[2] AS value2,
    extracted_float[3] AS value3,
    extracted_float[4] AS value4

FROM (
    SELECT *,
        regexp_extract_all(narrative, '-?\\d*\\.\\d+') AS extracted_float
    FROM pvl_measurements
) t
")


# STEP 2: Handle unobtainable measurements
# These are measurements that were attempted but could not be obtained due to
# technical or physiological reasons. We code these as 999 for tracking purposes.
message("Step 2: Processing unobtainable measurements...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW pvl_unob AS
SELECT
    deid_person_id,
    proc_start_date,
    resulting_lab_name,
    description,
    narrative,
    anatomy,
    specific_anatomy,
    NULL AS desc_laterality,
    CASE WHEN rt_unobtainable IS NOT NULL THEN 'right' END AS narr_right,
    CASE WHEN lt_unobtainable IS NOT NULL THEN 'left' END AS narr_left,
    999 AS value
FROM pvl_1
WHERE type = 'measurement'
    AND unobtainable = 1
    AND anatomy != 'arm'
")


# STEP 3: Handle single-value measurements (unilateral)
# When only one float value is present and laterality is not bilateral,
# we can directly assign the value to the indicated side
message("Step 3: Processing unilateral single-value measurements...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW pvl_array1 AS
SELECT 
    deid_person_id,
    proc_start_date,
    resulting_lab_name,
    description,
    narrative,
    anatomy,
    specific_anatomy,
    desc_laterality,
    narr_right,
    narr_left,
    value1 AS value
FROM pvl_1
WHERE type = 'measurement'
    AND float_check = 1
    AND array_length = 1
    AND desc_laterality != 'bilateral'
    AND unobtainable = 0
")


# STEP 4: Handle single-value bilateral measurements
# When one value is present but marked as bilateral, we create records for both 
# sides with the same value (common for measurements taken simultaneously)
message("Step 4: Processing bilateral single-value measurements...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW pvl_array1b AS
SELECT 
    deid_person_id,
    proc_start_date,
    resulting_lab_name,
    description,
    narrative,
    anatomy, 
    specific_anatomy, 
    desc_laterality,
    narr_right,
    narr_left,  
    value1
FROM pvl_1
WHERE type = 'measurement'
    AND float_check = 1
    AND array_length = 1
    AND anatomy != 'arm'
    AND desc_laterality = 'bilateral'
")


# STEP 5: Handle two-value measurements
# When two float values are present, we assume the first is for the right side
# and the second is for the left side (standard clinical documentation convention)
message("Step 5: Processing two-value measurements...")

dbExecute(db_con, "
CREATE OR REPLACE TEMP VIEW pvl_array2 AS
SELECT 
    deid_person_id,
    proc_start_date,
    resulting_lab_name,
    description,
    narrative,
    anatomy, 
    specific_anatomy, 
    desc_laterality,
    'right' AS narr_right,
    NULL AS narr_left,
    value1 AS value
FROM pvl_1
WHERE type = 'measurement'
    AND float_check = 1
    AND array_length = 2
    AND anatomy != 'arm'

UNION ALL

SELECT 
    deid_person_id,
    proc_start_date,
    resulting_lab_name,
    description,
    narrative,
    anatomy, 
    specific_anatomy, 
    desc_laterality,
    NULL AS narr_right,
    'left' AS narr_left,
    value2 AS value
FROM pvl_1
WHERE type = 'measurement'
    AND float_check = 1
    AND array_length = 2
    AND anatomy != 'arm'
")


# STEP 6: Combine all parsed results into final table
# Union all the temporary views to create the final cleaned dataset
# This table contains one row per measurement per anatomical location per side
message("Step 6: Creating final combined table...")

dbExecute(db_con,
"CREATE OR REPLACE TABLE deriv_pvl_final AS
SELECT * FROM pvl_unob
UNION ALL
SELECT * FROM pvl_array1
UNION ALL
SELECT * FROM pvl_array1b
UNION ALL
SELECT * FROM pvl_array2
")

# Verify final table
record_count <- dbGetQuery(db_con, "SELECT COUNT(*) as n FROM deriv_pvl_final")
message(sprintf("Final table created with %d records", record_count$n))


# Optimize database
message("Optimizing database...")
dbExecute(db_con, "CHECKPOINT")
dbExecute(db_con, "VACUUM")


# Close connection
dbDisconnect(db_con)
message("PVL parsing complete! Database connection closed.")
