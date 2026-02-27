# Calculate Charlson Comorbidity Index (CCI)
# Author(s): Peter Leese
# Date Created: 2024-12-30
# Date Updated: 2026-02-27
# 
# This script calculates the Charlson Comorbidity Index (CCI) for each patient
# based on their condition diagnoses. The implementation follows the ICD code 
# mappings from Quan et al. (2005) and uses the original weights from 
# Charlson et al. (1987).
# 
# Reference implementation: 
# https://github.com/NCTraCSIDSci/charlson_comorbidity_omop_ordrd
# 
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.
#
# INPUTS:
#   - condition_occurrence table: Patient diagnosis records in OMOP format
#   - person table: Patient demographic information
# 
# OUTPUTS:
#   - deriv_calc_cci: Table with patient ID and their calculated CCI score
#
# METHODOLOGY:
#   1. Filter to EHR problem list conditions
#   2. Map ICD-9 and ICD-10 codes to 17 comorbidity categories (Quan 2005)
#   3. Handle special cases requiring double-counting across categories
#   4. Remove duplicate conditions within categories
#   5. Apply weights to each category (Charlson 1987)
#   6. Sum weights to produce final CCI score per patient


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

message("Connected to database. Calculating Charlson Comorbidity Index...")


# Calculate CCI using multi-step CTE approach
# The query follows the Quan et al. (2005) algorithm with original 
# Charlson et al. (1987) weights
dbExecute(db_con, "

DROP TABLE IF EXISTS deriv_calc_cci; 

CREATE TABLE deriv_calc_cci AS

-- Step 1: Filter to relevant condition records
-- Only include conditions from EHR problem list (concept_id 32840)
-- Optionally restrict to specific date ranges if needed for analysis
WITH condition_start_filter AS (
    SELECT vco.*
    FROM condition_occurrence AS vco
    LEFT JOIN person AS p
        ON vco.deid_person_id = p.deid_person_id
    WHERE condition_type_concept_id = 32840  -- EHR problem list
    -- Optional: only keep conditions with start date on or after birth date
    -- AND (vco.condition_start_date - p.birth_datetime::date) >= 0 
    -- Optional: restrict dates for CCI computation window
    -- AND vco.condition_start_date >= '2015-01-03' 
    -- AND vco.condition_start_date < '2016-01-03'
),

-- Step 2: Map ICD-9 and ICD-10 codes to comorbidity categories
-- Uses mappings from Quan et al. (2005) to assign conditions to 17 categories
-- Categories are assigned numeric codes 1-17, with 0 for non-comorbid conditions
conditions AS (
    SELECT DISTINCT
        deid_person_id,
        condition_start_date,
        condition_end_date,
        condition_source_value,

        -- Myocardial Infarction (Category 1)
        CASE WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('410', '412') THEN 1
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('I21', 'I22') THEN 1
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  = ('I252') THEN 1 

        -- Congestive Heart Failure (Category 2)
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 5) 
                  IN ('39891', '40201', '40211', '40291', '40401', '40403',
                      '40411', '40413', '40491', '40493') THEN 2
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('428') THEN 2 
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('I099', 'I110', 'I130', 'I132', 'I255', 'I420', 'I425', 
                      'I426', 'I427', 'I428', 'I429', 'P290', '4254', '4255', 
                      '4256', '4257', '4258', '4259') THEN 2
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('I43', 'I50') THEN 2
                 
        -- Peripheral Vascular Disease (Category 3)
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('0930', '4373', '4431', '4432', '4433', '4434', '4435', 
                      '4436', '4437', '4438', '4439', '4471', '5571', '5579', 
                      'V434') THEN 3
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('440', '441') THEN 3 
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('I731', 'I738', 'I739', 'I771', 'I790', 'I792', 'K551', 
                      'K558', 'K559', 'Z958', 'Z959') THEN 3
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('I70', 'I71') THEN 3
                 
        -- Cerebrovascular Disease (Category 4)
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('430', '431', '432', '433', '434', '435', '436', '437', 
                      '438') THEN 4
             WHEN TRANSLATE(condition_source_value, '.', '') = ('36234') THEN 4  
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('G45', 'G46', 'I60', 'I61', 'I62', 'I63', 'I64', 'I65', 
                      'I66', 'I67', 'I68', 'I69') THEN 4
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  = ('H340') THEN 4 
                 
        -- Dementia (Category 5)
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 5) 
                  IN ('29410', '29411') THEN 5
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('290') THEN 5  
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('F051', 'G311', '3312') THEN 5
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('F00', 'F01', 'F02', 'F03', 'G30') THEN 5

        -- Chronic Pulmonary Disease (Category 6)
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                 IN ('490', '491', '492', '493', '494', '495', '496', '500', 
                     '501', '502', '503', '504', '505') THEN 6
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                 IN ('4168', '4169', '5064', '5081', '5088') THEN 6 
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                 IN ('J40', 'J41', 'J42', 'J43', 'J44', 'J45', 'J46', 'J47', 
                     'J60', 'J61', 'J62', 'J63', 'J64', 'J65', 'J66', 'J67') THEN 6
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                 IN ('I278', 'I279', 'J684', 'J701', 'J703') THEN 6
            
        -- Connective Tissue Disease (Category 7)
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('4465', '7100', '7101', '7102', '7103', '7104', '7140', 
                      '7141', '7142', '7148') THEN 7
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('725') THEN 7  
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('M315', 'M351', 'M353', 'M360') THEN 7 
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('M05', 'M32', 'M33', 'M34', 'M06') THEN 7
                 
        -- Peptic Ulcer Disease (Category 8)
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('531', '532', '533', '534') THEN 8 
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('K25', 'K26', 'K27', 'K28') THEN 8
                 
        -- Mild Liver Disease (Category 9)
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 5) 
                  IN ('07022', '07023', '07032', '07033', '07044', '07054') THEN 9
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('570', '571') THEN 9 
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('K700', 'K701', 'K702', 'K703', 'K709', 'K717', 'K713', 
                      'K714', 'K715', 'K760', 'K762', 'K763', 'K764', 'K768', 
                      'K769', 'Z944', '0706', '0709', '5733', '5734', '5738', 
                      '5739', 'V427') THEN 9
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('B18', 'K73', 'K74') THEN 9
                     
        -- Diabetes Without Complications (Category 10)
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('2500', '2501', '2502', '2503', '2508', '2509') THEN 10
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('E100', 'E101', 'E106', 'E108', 'E109', 'E110', 'E111', 
                      'E116', 'E118', 'E119', 'E120', 'E121', 'E126', 'E128', 
                      'E129', 'E130', 'E131', 'E136', 'E138', 'E139', 'E140', 
                      'E141', 'E146', 'E148', 'E149') THEN 10
            
        -- Diabetes With Complications (Category 11)
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('2504', '2505', '2506', '2507') THEN 11 
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('E102', 'E103', 'E104', 'E105', 'E107', 'E112', 'E113', 
                      'E114', 'E115', 'E117', 'E122', 'E123', 'E124', 'E125', 
                      'E127', 'E132', 'E133', 'E134', 'E135', 'E137', 'E142', 
                      'E143', 'E144', 'E145', 'E147') THEN 11
                         
        -- Paralysis (Category 12)
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('3341', '3440', '3441', '3442', '3443', '3444', '3445', 
                      '3446', '3449') THEN 12
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('342', '343') THEN 12 
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('G041', 'G114', 'G801', 'G802', 'G830', 'G831', 'G832', 
                      'G833', 'G834', 'G839') THEN 12
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('G81', 'G82') THEN 12
                 
        -- Renal Disease (Category 13)
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 5) 
                  IN ('40301', '40311', '40391', '40402', '40403', '40412', 
                      '40413', '40492', '40493') THEN 13
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('582', '585', '586', 'V56') THEN 13
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                  IN ('N18', 'N19') THEN 13
             WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                  IN ('N052', 'N053', 'N054', 'N055', 'N056', 'N057', 'N250', 
                      'I120', 'I131', 'N032', 'N033', 'N034', 'N035', 'N036', 
                      'N037', 'Z490', 'Z491', 'Z492', 'Z940', 'Z992', '5830', 
                      '5831', '5832', '5834', '5836', '5837', '5880', 'V420', 
                      'V451') THEN 13
                 
        -- Cancer (Category 14)
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                 IN ('140', '141', '142', '143', '144', '145', '146', '147', 
                     '148', '149', '150', '151', '152', '153', '154', '155', 
                     '156', '157', '158', '159', '160', '161', '162', '163', 
                     '164', '165', '170', '171', '172', '174', '175', '176', 
                     '179', '180', '181', '182', '183', '184', '185', '186', 
                     '187', '188', '189', '190', '191', '192', '193', '194', 
                     '195', '200', '201', '202', '203', '204', '205', '206', 
                     '207', '208') THEN 14
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                 IN ('2386') THEN 14 
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                 IN ('C00', 'C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 
                     'C08', 'C09', 'C10', 'C11', 'C12', 'C13', 'C14', 'C15', 
                     'C16', 'C17', 'C18', 'C19', 'C20', 'C21', 'C22', 'C23', 
                     'C24', 'C25', 'C26', 'C30', 'C31', 'C32', 'C33', 'C34', 
                     'C37', 'C38', 'C39', 'C40', 'C41', 'C43', 'C45', 'C46', 
                     'C47', 'C48', 'C49', 'C50', 'C51', 'C52', 'C53', 'C54', 
                     'C55', 'C56', 'C57', 'C58', 'C60', 'C61', 'C62', 'C63', 
                     'C64', 'C65', 'C66', 'C67', 'C68', 'C69', 'C70', 'C71', 
                     'C72', 'C73', 'C74', 'C75', 'C76', 'C81', 'C82', 'C83', 
                     'C84', 'C85', 'C88', 'C90', 'C91', 'C92', 'C93', 'C94', 
                     'C95', 'C96', 'C97') THEN 14
            
        -- Moderate or Severe Liver Disease (Category 15)
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                 IN ('4560', '4561', '4562', '5722', '5723', '5724', '5728') THEN 15 
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                 IN ('K704', 'K711', 'K721', 'K729', 'K765', 'K766', 'K767', 
                     'I850', 'I859', 'I864', 'I982') THEN 15
            
        -- Metastatic Cancer (Category 16)
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                 IN ('196', '197', '198', '199') THEN 16 
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                 IN ('C77', 'C78', 'C79', 'C80') THEN 16
            
        -- HIV/AIDS (Category 17)
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                 IN ('042', '043', '044') THEN 17
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 3) 
                 IN ('B20', 'B21', 'B22', 'B24') THEN 17

            ELSE 0 
        END AS cc_group
    FROM condition_start_filter
),

-- Step 3: Handle special double-counting cases
-- Certain ICD codes need to be counted in multiple comorbidity categories
-- per the Quan algorithm
conditions_expanded AS (
    SELECT *
    FROM conditions
    
    UNION ALL
    
    SELECT 
        deid_person_id,
        condition_start_date,
        condition_end_date,
        condition_source_value,
        CASE 
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 5) 
                 IN ('40403', '40413', '40493') THEN 13
            WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
                 IN ('4373') THEN 4
            ELSE 0 
        END AS cc_group
    FROM condition_occurrence
    WHERE CASE
        WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 5) 
             IN ('40403', '40413', '40493') THEN 1
        WHEN SUBSTRING(TRANSLATE(condition_source_value, '.', ''), 1, 4) 
             IN ('4373') THEN 1
        ELSE 0 
    END = 1
),

-- Step 4: Remove exclusion codes
-- Per Quan 2005, certain ICD-9 procedure codes (V codes) should be excluded
-- as they represent procedures rather than conditions
conditions_expanded_filtered AS (
    SELECT *
    FROM conditions_expanded
    WHERE NOT (
        (TRANSLATE(condition_source_value, '.', '') LIKE 'V434%' OR
         TRANSLATE(condition_source_value, '.', '') LIKE 'V4276%' OR
         TRANSLATE(condition_source_value, '.', '') LIKE 'V420%' OR
         TRANSLATE(condition_source_value, '.', '') LIKE 'V451%' OR
         TRANSLATE(condition_source_value, '.', '') LIKE 'V56%')
    )
),

-- Step 5: Remove duplicate comorbidity categories per patient
-- Each patient should only be counted once per comorbidity category
-- regardless of how many qualifying diagnoses they have
no_duplicate_conditions AS (
    SELECT DISTINCT 
        deid_person_id, 
        cc_group 
    FROM conditions_expanded_filtered
    WHERE cc_group > 0
),

-- Step 6: Apply weights to each comorbidity category
-- Uses original weights from Charlson et al. (1987)
weights AS (
    SELECT 
        deid_person_id,
        cc_group,
        CASE 
            WHEN cc_group = 1 THEN 1   -- Myocardial infarction
            WHEN cc_group = 2 THEN 1   -- Congestive heart failure
            WHEN cc_group = 3 THEN 1   -- Peripheral vascular disease
            WHEN cc_group = 4 THEN 1   -- Cerebrovascular disease
            WHEN cc_group = 5 THEN 1   -- Dementia
            WHEN cc_group = 6 THEN 1   -- Chronic pulmonary disease
            WHEN cc_group = 7 THEN 1   -- Connective tissue disease
            WHEN cc_group = 8 THEN 1   -- Peptic ulcer disease
            WHEN cc_group = 9 THEN 1   -- Mild liver disease
            WHEN cc_group = 10 THEN 1  -- Diabetes without complications
            WHEN cc_group = 11 THEN 2  -- Diabetes with complications
            WHEN cc_group = 12 THEN 2  -- Paralysis
            WHEN cc_group = 13 THEN 2  -- Renal disease
            WHEN cc_group = 14 THEN 2  -- Cancer
            WHEN cc_group = 15 THEN 3  -- Moderate or severe liver disease
            WHEN cc_group = 16 THEN 6  -- Metastatic cancer
            WHEN cc_group = 17 THEN 6  -- HIV/AIDS
            ELSE 0 
        END AS cc_weights
    FROM no_duplicate_conditions
)

-- Step 7: Calculate final CCI score
-- Sum all comorbidity weights for each patient to produce their CCI
SELECT 
    deid_person_id, 
    SUM(cc_weights) AS cci
FROM weights
GROUP BY deid_person_id

")

# Verify table creation
record_count <- dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM deriv_calc_cci")
message(sprintf("CCI calculated for %d patients", record_count$n))

# Display summary statistics
cci_summary <- dbGetQuery(db_con, "
    SELECT 
        MIN(cci) AS min_cci,
        CAST(AVG(cci) AS DECIMAL(10,2)) AS mean_cci,
        MAX(cci) AS max_cci
    FROM deriv_calc_cci
")
message("CCI Summary Statistics:")
print(cci_summary)


# Optimize database
message("Optimizing database...")
dbExecute(db_con, "CHECKPOINT")
dbExecute(db_con, "VACUUM")


# Close connection
dbDisconnect(db_con, shutdown=TRUE)
message("CCI calculation complete! Database connection closed.")
