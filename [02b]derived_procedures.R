# Create Procedure-Based Derived Tables
# Author(s): Peter Leese
# Started on 2024-12-22
# Last updated: 2024-12-30
# 
# This script creates derived tables for key surgical procedures including
# revascularization procedures and lower extremity amputations. These tables
# support outcome assessment and treatment pattern analyses.
# 
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.
#
# INPUTS:
#   - procedure_occurrence: Procedure records in OMOP format
#   - concept: OMOP vocabulary concept definitions
# 
# OUTPUTS:
#   - deriv_revascularization: Lower extremity revascularization procedures
#   - deriv_amputations: Lower extremity amputation procedures


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

message("Connected to database. Creating procedure-based derived tables...")


# TABLE 1: Revascularization Procedures
# This table captures lower extremity revascularization procedures including:
# - Percutaneous interventions (angioplasty, stenting)
# - Open surgical bypass procedures  
# - Endarterectomy and thrombectomy
# - Revision procedures
#
# Procedure codes include both CPT codes (for professional billing) and 
# ICD-10-PCS codes (for inpatient procedures). The procedure_concept_id in 
# OMOP maps to standard SNOMED procedure concepts.
#
# Binary flags are created to categorize procedure types based on the 
# standardized concept_name, enabling flexible querying and analysis
message("Creating revascularization procedures table...")

dbExecute(db_con, "

CREATE OR REPLACE TABLE deriv_revascularization AS
SELECT DISTINCT
    deid_person_id, 
    procedure_date, 
    procedure_source_value, 
    concept_name,

    -- Extract laterality from standardized concept name
    CASE 
        WHEN LOWER(concept_name) LIKE '%right%' THEN 'right'
        WHEN LOWER(concept_name) LIKE '%left%' THEN 'left'
        ELSE NULL 
    END AS laterality,

    -- Procedure type flags based on concept name keywords
    CASE WHEN LOWER(concept_name) LIKE '%dilation%' THEN 1 ELSE 0 END AS dilation,
    CASE WHEN LOWER(concept_name) LIKE '%bypass%' THEN 1 ELSE 0 END AS bypass,
    CASE WHEN LOWER(concept_name) LIKE '%extirpation%' THEN 1 ELSE 0 END AS extirpation,
    CASE WHEN LOWER(concept_name) LIKE '%arterectomy%' THEN 1 ELSE 0 END AS arterectomy,
    CASE WHEN LOWER(concept_name) LIKE '%reoperation%' THEN 1 ELSE 0 END AS reoperation,
    CASE WHEN LOWER(concept_name) LIKE '%thrombectomy%' THEN 1 ELSE 0 END AS thrombectomy

FROM procedure_occurrence AS a 
LEFT JOIN concept AS b 
    ON a.procedure_concept_id = b.concept_id

WHERE procedure_source_value IN (
    -- CPT Codes for lower extremity revascularization
    -- Bypass procedures (35xxx series)
    '34201', '34203',  -- Embolectomy/thrombectomy
    '35302', '35303', '35304', '35305', '35306',  -- Thromboendarterectomy
    '35331',  -- Direct repair of blood vessel
    '35351', '35355', '35361', '35363',  -- Thromboendarterectomy with patch
    '35371', '35372',  -- Endarterectomy with patch graft
    '35537', '35637', '35538', '35638', '35539', '35647',  -- Bypass procedures (splenic to renal)
    '35540', '35646',  -- Bypass (renal to aorta)
    '35556', '35656', '35558', '35661', '35563', '35663',  -- Fem-pop bypass variations
    '35565', '35665', '35566', '35666',  -- Fem-anterior tibial bypass
    '35570', '35571', '35671',  -- Popliteal-tibial/peroneal bypass
    '35583', '35585', '35587',  -- In-situ vein bypass
    '35621', '35623', '35654',  -- Axillary-femoral bypass
    '35700',  -- Reoperation with revision
    '35875', '35879', '35881', '35883', '35884'  -- Thrombectomy procedures
)
OR procedure_source_value IN (
    -- ICD-10-PCS Codes for lower extremity revascularization
    -- Format: 04 = Lower arteries, various approaches and devices
    '04CK0ZZ', '04CL0ZZ', '04CK3ZZ', '04CL3ZZ',  -- Extirpation (clot removal)
    '047L3ZZ', '047K3ZZ', '047Q3ZZ', '047P3ZZ', '047N3ZZ',  -- Dilation procedures
    '047R3ZZ', '047U3ZZ', '047M3ZZ', '047T3ZZ', '047S3ZZ',  -- Additional dilation
    '04CM0ZZ', '04CN0ZZ', '04CM3ZZ', '04CN3ZZ',  -- More extirpation
    '047C3DZ', '047D3DZ', '047K3DZ', '047L3DZ', '047H3DZ',  -- Dilation with intraluminal device
    '047J3DZ', '047D3ZZ', '047C3ZZ', '047Y3ZZ',  -- Various dilation approaches
    '04CH0ZZ', '041L09L', '04CJ0ZZ', '047J3ZZ',  -- Bypass and extirpation combinations
    '047L34Z', '041K09L', '041K0JJ', '04100JK',  -- Bypass with synthetic substitute
    '04CP3ZZ', '047K34Z', '04CR3ZZ', '04CS0ZZ',  -- Additional procedures
    '041L0JH', '04CC0ZZ', '04CR0ZZ', '041L0JN',  -- Bypass variations
    '04CU3ZZ', '04CP0ZZ', '047N34Z', '047M34Z',  -- Dilation and extirpation
    '04CQ3ZZ', '04CT3ZZ', '04CS3ZZ', '04CD0ZZ',  -- More extirpation
    '04CQ0ZZ', '047M3DZ', '04703DZ', '04CU0ZZ',  -- Various approaches
    '041K0JN', '047K0ZZ', '047D34Z', '041K09N',  -- Bypass procedures
    '04CT0ZZ', '047H0DZ', '067D3DZ', '047C34Z',  -- Dilation with devices
    '04C00ZZ', '041K0ZL', '047L0ZZ', '047C0DZ',  -- Open and percutaneous
    '047P34Z', '04CH3ZZ', '047J34Z', '047D0DZ',  -- Additional variations
    '047H34Z', '047Q34Z', '041L09N', '047J0DZ',  -- More procedure variations
    '03743DZ', '047S34Z', '041K0JM', '04CJ3ZZ',  -- Upper and lower combinations
    '04CD3ZZ', '04CC3ZZ', '041K0JQ', '041L0JM',  -- Bypass with various substitutes
    '04CY3ZZ', '041N09Q', '041K0JH', '047P3DZ',  -- Additional codes
    '041L0KL', '03150J8', '047U34Z', '047K0DZ',  -- Synthetic and autologous grafts
    '041L0ZL', '047T34Z', '041K0KL', '047V3ZZ',  -- More bypass variations
    '041L0JJ', '047Y3DZ', '047H0ZZ', '041K0KN',  -- Open approach procedures
    '041L09M', '03160J7', '041M09Q', '057Y3DZ',  -- Additional bypass codes
    '03150J6', '047N0ZZ', '041L0KN', '047P0ZZ',  -- Various graft materials
    '047R34Z', '047M0ZZ', '04BL0ZZ', '047C0ZZ',  -- Excision and dilation
    '04BK0ZZ', '04100J8', '047W3ZZ', '041L0ZH',  -- Bypass and excision
    '047C04Z', '067F3DZ', '047Y34Z', '047D0ZZ',  -- Device-based procedures
    '03150J9', '047K04Z', '041K09M', '04LL3DZ',  -- Upper artery procedures
    '047H04Z', '047L0DZ', '041L09Q', '041K0ZJ',  -- Open and percutaneous mix
    '03160JC', '047T3DZ', '047J04Z', '047J0ZZ',  -- Additional variations
    '047L04Z', '041K09Q', '041K0JK', '04LK3ZZ',  -- Occlusion and bypass
    '047S3DZ', '04703ZZ', '047S0ZZ', '04LK0ZZ',  -- More occlusion codes
    '041L0ZN', '04104J8', '047E3ZZ', '04100JJ',  -- Bypass with synthetic
    '04100ZK', '047M0DZ', '047R0ZZ', '04CK4ZZ',  -- Various device approaches
    '047D04Z', '04C03ZZ', '041K0ZN', '047Y0ZZ',  -- Extirpation variations
    '041M09M', '047T0ZZ', '047Q0ZZ', '041C0JH',  -- Additional bypass codes
    '047Q3DZ', '047U3DZ', '047F3ZZ', '041N0JQ',  -- Dilation with devices
    '047U0ZZ', '041H0JH', '047F34Z', '041L0AH',  -- Various graft types
    '041J0JJ', '04BM0ZZ', '041M09P', '041L0JQ',  -- Excision and bypass
    '04700DZ', '041J0JH', '04CW3ZZ', '041L4JL',  -- Multiple approaches
    '047E34Z', '03160JB', '04BY0ZZ', '047R3DZ',  -- Additional codes
    '041M0JQ', '04CM4ZZ', '04LL3ZZ', '041L0JS',  -- More bypass variations
    '04CV3ZZ', '041M09L', '041D0JJ', '041M0JL',  -- Various substitutes
    '047E3DZ', '04LK3DZ', '04LL0ZZ', '041K0ZM',  -- Occlusion procedures
    '041L0KH', '04CE0ZZ', '04CW0ZZ', '047C4DZ',  -- Extirpation codes
    '061M09Y', '041K4ZL', '041N09P', '041L0ZJ',  -- Transfer and bypass
    '04100JG', '04CE3ZZ', '04CR4ZZ', '041L0AL',  -- Various approaches
    '047N0DZ', '047M04Z', '045Y0ZZ', '047C44Z',  -- Destruction and dilation
    '04100JH', '047N4ZZ', '041L0KM', '047N04Z',  -- Additional variations
    '03160Z7', '041K0KS', '041K49L', '04CN4ZZ',  -- More procedure codes
    '041K49N', '041M0JM', '047J44Z', '047E0DZ',  -- Bypass variations
    '03150A8', '041L09P', '047D4ZZ', '047M4ZZ',  -- Autologous tissue
    '04CT4ZZ', '04BY4ZZ', '047K44Z', '047034Z',  -- Device-based codes
    '041M0KS', '041K0KM', '041K0ZS', '047P04Z',  -- Various substitutes
    '047F0DZ', '047D4DZ', '041N0JL', '04CV0ZZ',  -- Dilation and bypass
    '041L4JH', '041N09M', '041L4KL', '041L09S',  -- More bypass codes
    '041K0JS', '041L0KJ', '047Q4ZZ', '04CP4ZZ',  -- Additional procedures
    '041K4JJ', '047S4ZZ', '04LN0ZZ', '047C4ZZ',  -- Occlusion and dilation
    '041L0JK', '041N09L', '03150JC', '041K0KK',  -- Bypass variations
    '047Y0DZ', '04LU0ZZ', '041K0KJ', '047T4ZZ',  -- Various devices
    '041N0KQ', '041K0KQ', '04LQ0ZZ', '041L4AL',  -- Occlusion codes
    '04BP0ZZ', '04100KH', '04100ZF', '041L0ZK',  -- Excision and bypass
    '04RK07Z', '041K4JM', '03160A8', '04LK0CZ',  -- Replacement procedures
    '047T0DZ', '04BN0ZZ', '041L4JJ', '041K0AK',  -- Various approaches
    '047U0DZ', '041K0AL', '03150Z6', '047N4DZ',  -- Dilation codes
    '041L0ZS', '04100JR', '041K0ZH', '041N0ZS',  -- Bypass variations
    '041K09J', '04BS0ZZ', '041L4ZH', '041F0JJ',  -- Additional codes
    '041K09P', '041L0AJ', '041L0KQ', '04BR0ZZ',  -- More bypass procedures
    '047J4DZ', '047L4ZZ', '04LR3DZ', '041M0KQ',  -- Occlusion and dilation
    '041N0KP', '04CF0ZZ', '04100J7', '04100JQ',  -- Extirpation codes
    '041K4JL', '047E0ZZ', '047U4ZZ', '041L0KS',  -- Various procedures
    '041K4JK', '047R44Z', '03150AC', '04LM3ZZ',  -- Additional variations
    '03150J7', '04CF3ZZ', '047D44Z', '04RK0JZ',  -- Replacement codes
    '047K4ZZ', '03160A7', '04CU4ZZ', '04100JF',  -- Dilation and bypass
    '041K0AJ', '04LL0CZ', '047H4ZZ', '041K4KN',  -- Occlusion procedures
    '04RK0KZ', '041E0JH', '04104JC', '041K4ZM',  -- Replacement variations
    '047P0DZ', '03150A6', '04RL0KZ', '047H4DZ',  -- Device-based codes
    '047Q0DZ', '047N44Z', '041N0JP', '041K09H',  -- Dilation procedures
    '04104KD', '04100J6', '041K0KH', '04LK0DZ',  -- Various approaches
    '04LP0ZZ', '04100Z8', '04LS3ZZ', '041K4JN',  -- Occlusion and bypass
    '04104ZK', '041N0ZM', '04100ZD', '047L4DZ',  -- Additional codes
    '041L49L', '047E04Z', '061G0JY', '041D0JH',  -- Transfer and bypass
    '04LR3ZZ', '041M0ZQ', '04100AK', '041N09S',  -- Various procedures
    '041N0JM', '047J4ZZ', '041K0ZK', '047H44Z'   -- Final procedure codes
)

ORDER BY deid_person_id, procedure_date
")

# Verify table creation
revasc_count <- dbGetQuery(db_con, "
    SELECT COUNT(*) AS n_procedures,
           COUNT(DISTINCT deid_person_id) AS n_patients,
           MIN(procedure_date) AS earliest_date,
           MAX(procedure_date) AS latest_date
    FROM deriv_revascularization
")
message(sprintf("Revascularization procedures: %d procedures for %d patients", 
                revasc_count$n_procedures, revasc_count$n_patients))


# TABLE 2: Amputation Procedures
# This table captures lower extremity amputations at three anatomical levels:
# - Toe (digital) amputations: Least severe, often performed for localized 
#   infection or gangrene
# - Foot (transmetatarsal) amputations: Mid-level severity, preserves some 
#   ambulation potential
# - Leg (below/above knee) amputations: Most severe, major impact on mobility
# - Revision procedures: Subsequent procedures on previous amputation site
#
# Note: A patient may have multiple amputations on the same date (e.g., 
# multiple toes) or sequential amputations at different levels over time.
# This is reflected in the binary flags which are summed, not deduplicated.
message("Creating amputation procedures table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_amputations AS
SELECT DISTINCT 
    deid_person_id, 
    procedure_date,
    concept_name,

    -- Amputation level flags based on procedure concept IDs
    -- These are binary indicators that can be summed to count multiple 
    -- amputations on the same date
    CASE 
        WHEN procedure_concept_id IN (
            4159766,  -- Amputation of toe
            40480578, -- Disarticulation of toe
            2105806,  -- Amputation through metatarsophalangeal joint
            4143797,  -- Amputation of foot at metatarsophalangeal joint
            4272232,  -- Ray amputation of foot
            4302020   -- Amputation of digit of foot
        ) THEN 1 
        ELSE 0 
    END AS amp_toe,

    CASE 
        WHEN procedure_concept_id IN (
            4054983,  -- Transmetatarsal amputation
            4264289,  -- Amputation of foot
            2105451,  -- Syme amputation
            2006242   -- Chopart amputation
        ) THEN 1 
        ELSE 0 
    END AS amp_foot,

    CASE 
        WHEN procedure_concept_id IN (
            2105448,  -- Below knee amputation
            4195136,  -- Above knee amputation
            4338257,  -- Amputation of lower limb
            2105223,  -- Through knee amputation
            2105222,  -- Gritti-Stokes amputation
            2101638,  -- Hindquarter amputation
            2006243,  -- Hip disarticulation
            4143795,  -- Amputation of leg through tibia and fibula
            2105450,  -- Amputation through femur
            2105449,  -- Amputation above knee
            36675618, -- Knee disarticulation
            2105211,  -- Revision of amputation stump
            2105447   -- Guillotine amputation of lower limb
        ) THEN 1 
        ELSE 0 
    END AS amp_leg,

    CASE 
        WHEN procedure_concept_id IN (
            4078404   -- Revision of amputation of lower extremity
        ) THEN 1 
        ELSE 0 
    END AS amp_revision

FROM procedure_occurrence AS a 
LEFT JOIN concept AS b 
    ON a.procedure_concept_id = b.concept_id 

WHERE procedure_concept_id IN (
    -- All amputation-related procedure concept IDs
    -- Organized by anatomical level for clarity
    
    -- Toe amputations
    4159766, 40480578, 2105806, 4272232, 4302020, 4143797,
    
    -- Foot amputations  
    4054983, 4264289, 2105451, 2006242,
    
    -- Leg amputations
    2105448, 4195136, 4338257, 2105223, 2105222, 2101638,
    2006243, 4143795, 2105450, 2105449, 36675618, 2105211,
    2105447,
    
    -- Revision procedures
    4078404,
    
    -- Additional procedure codes (may include wound care, etc.)
    4345680, 42536891, 4176170, 4308713, 4103642, 4202322,
    133088, 135722, 196613, 4101660, 4103640, 4103638, 4113102
) 

ORDER BY deid_person_id, procedure_date
")

# Verify table creation
amp_count <- dbGetQuery(db_con, "
    SELECT COUNT(*) AS n_procedures,
           COUNT(DISTINCT deid_person_id) AS n_patients,
           SUM(amp_toe) AS n_toe_amps,
           SUM(amp_foot) AS n_foot_amps,
           SUM(amp_leg) AS n_leg_amps,
           SUM(amp_revision) AS n_revisions
    FROM deriv_amputations
")
message(sprintf("Amputation procedures: %d total procedures for %d patients", 
                amp_count$n_procedures, amp_count$n_patients))
message(sprintf("  - Toe: %d, Foot: %d, Leg: %d, Revisions: %d",
                amp_count$n_toe_amps, amp_count$n_foot_amps, 
                amp_count$n_leg_amps, amp_count$n_revisions))


# Display summary of all procedure-based derived tables
message("\nSummary of procedure-based derived tables:")
proc_tables <- dbGetQuery(db_con, "
    SELECT table_name 
    FROM information_schema.tables 
    WHERE table_name IN ('deriv_revascularization', 'deriv_amputations')
    ORDER BY table_name
")
print(proc_tables)


# Optimize database
message("\nOptimizing database...")
dbExecute(db_con, "CHECKPOINT")
dbExecute(db_con, "VACUUM")


# Close connection
dbDisconnect(db_con)
message("Procedure-based derived tables created! Database connection closed.")
