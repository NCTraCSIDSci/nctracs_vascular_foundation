# Create Social Determinants of Health (SDoH) Derived Tables
# Author(s): Peter Leese
# Started on 2024-12-22
# 
# This script creates derived tables capturing social determinants of health
# from patient-reported screening data. SDoH factors significantly impact
# health outcomes in vascular disease patients, affecting treatment adherence,
# wound healing, and disease progression.
# 
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.
#
# INPUTS:
#   - observation: Patient observations and screening responses in OMOP format
#   - concept: OMOP vocabulary concept definitions
# 
# OUTPUTS:
#   - deriv_sdoh: Financial resource strain (food, housing, medical, utilities)
#   - deriv_smoke: Current smoking status and exposure
#   - deriv_trans: Transportation access barriers
#
# SDOH DOMAINS & OMOP CONCEPT IDs:
#   Financial Strain: 37116643, 36306143, 36304041, 46234789, 4010022
#     (Food insecurity, housing insecurity, medical cost barriers, utility shutoff)
#   Smoking: 903656, 903657
#     (Current smoker, passive smoke exposure)
#   Transportation: 3964826, 37020730
#     (Transportation barriers to medical care)
#
# RESPONSE FILTERING LOGIC:
#   Includes: Affirmative responses, "choose not to respond", "don't know"
#   Excludes: Clear negative responses ("No", "Never")
#   Rationale: Non-response may indicate discomfort or actual barriers; 
#              conservative approach flags potential needs


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

message("Connected to database. Creating social determinants of health derived tables...")


# TABLE 1: Financial Resource Strain
# Financial insecurity encompasses multiple domains: food, housing, medical care,
# and utilities. These social determinants directly impact health outcomes in
# vascular disease patients. Food insecurity affects diabetes control and wound
# healing. Housing instability disrupts care continuity. Medical cost barriers
# reduce medication adherence. Utility shutoffs create unsafe environments for
# wound care and increase stress.
#
# This table flags patients who report financial strain in any domain OR who
# choose not to respond to screening questions (which may indicate discomfort
# or actual barriers). This conservative approach helps identify patients who
# may benefit from social work referral or resource navigation.
#
# OMOP Concept IDs:
# - 37116643: Food insecurity screening
# - 36306143: Housing insecurity screening  
# - 36304041: Medical cost barriers screening
# - 46234789: Utility shutoff risk screening
# - 4010022: General financial strain screening
#
# Excluded value concepts (clear negative responses):
# - 36309869: "No" / negative response
# - 21498606: "Never"
# - 1621261: Other negative response codes
message("Creating financial resource strain table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_sdoh AS
WITH finance AS (
    SELECT 
        b.concept_name, 
        a.* 
    FROM observation AS a 
    LEFT JOIN concept AS b 
        ON a.observation_concept_id = b.concept_id
    WHERE observation_concept_id IN (
        37116643,  -- Food insecurity screening
        36306143,  -- Housing insecurity screening
        36304041,  -- Medical cost barriers screening
        46234789,  -- Utility shutoff risk screening
        4010022    -- General financial strain screening
    )
        -- Exclude clear negative responses
        AND value_as_concept_id NOT IN (
            36309869,  -- No
            21498606,  -- Never
            1621261    -- Other negative responses
        )
        -- Only include records with actual response data
        AND value_as_string IS NOT NULL
)
SELECT DISTINCT 
    deid_person_id, 
    COUNT(observation_id) AS freq_finance
FROM finance
GROUP BY deid_person_id
")

# Verify table creation
finance_count <- dbGetQuery(db_con, "
    SELECT COUNT(DISTINCT deid_person_id) AS n_patients,
           CAST(AVG(freq_finance) AS DECIMAL(10,2)) AS mean_screenings,
           MAX(freq_finance) AS max_screenings
    FROM deriv_sdoh
")
message(sprintf("Financial strain: %d patients flagged (mean: %s screenings, max: %d)", 
                finance_count$n_patients, finance_count$mean_screenings, 
                finance_count$max_screenings))


# TABLE 2: Smoking Status and Exposure
# Tobacco use is the single most important modifiable risk factor for peripheral
# vascular disease. Current smoking dramatically accelerates atherosclerosis,
# impairs wound healing, increases amputation risk, and reduces revascularization
# procedure success rates. Even passive smoke exposure increases cardiovascular
# risk. This table identifies patients with current smoking or smoke exposure,
# who are priority targets for smoking cessation interventions.
#
# OMOP Concept IDs:
# - 903656: Current smoker
# - 903657: Passive smoke exposure / second-hand smoke exposure
#
# The table tracks the date range of documented smoking status and frequency
# of documentation, which may indicate ongoing counseling efforts.
message("Creating smoking status and exposure table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_smoke AS
SELECT 
    deid_person_id, 
    MIN(observation_date) AS min_smoking,
    MAX(observation_date) AS max_smoking,
    COUNT(observation_id) AS freq_smoking
FROM observation
WHERE observation_concept_id IN (
    903656,  -- Current smoker
    903657   -- Passive smoke exposure
)
GROUP BY deid_person_id
")

# Verify table creation
smoke_count <- dbGetQuery(db_con, "
    SELECT COUNT(DISTINCT deid_person_id) AS n_patients,
           CAST(AVG(freq_smoking) AS DECIMAL(10,2)) AS mean_documentation,
           MAX(freq_smoking) AS max_documentation
    FROM deriv_smoke
")
message(sprintf("Smoking/exposure: %d patients documented (mean: %s entries, max: %d)", 
                smoke_count$n_patients, smoke_count$mean_documentation, 
                smoke_count$max_documentation))


# TABLE 3: Transportation Access Barriers
# Reliable transportation is essential for accessing medical care, filling
# prescriptions, and attending wound care appointments. Transportation barriers
# are associated with missed appointments, medication non-adherence, emergency
# department visits, and disease progression. In vascular disease patients,
# transportation insecurity can delay critical interventions and increase
# amputation risk.
#
# This table flags patients who report transportation barriers OR who choose
# not to respond to screening. Non-response may indicate actual barriers or
# discomfort discussing social needs. This conservative approach ensures
# patients with potential access issues are identified for care coordination
# or telehealth options.
#
# OMOP Concept IDs:
# - 3964826: Transportation barriers to medical care
# - 37020730: Lack of reliable transportation
#
# Filtering logic: Excludes only clear "No" responses; includes affirmative
# responses and "choose not to respond" / missing data
message("Creating transportation access barriers table...")

dbExecute(db_con, "
CREATE OR REPLACE TABLE deriv_trans AS
WITH transpo AS (
    SELECT 
        b.concept_name, 
        a.* 
    FROM observation AS a 
    LEFT JOIN concept AS b 
        ON a.observation_concept_id = b.concept_id
    WHERE observation_concept_id IN (
        3964826,   -- Transportation barriers to medical care
        37020730   -- Lack of reliable transportation
    )
        -- Exclude only clear negative responses
        AND value_as_string != 'No'
)
SELECT 
    deid_person_id, 
    COUNT(observation_id) AS freq_transpo
FROM transpo
GROUP BY deid_person_id
")

# Verify table creation
trans_count <- dbGetQuery(db_con, "
    SELECT COUNT(DISTINCT deid_person_id) AS n_patients,
           CAST(AVG(freq_transpo) AS DECIMAL(10,2)) AS mean_screenings,
           MAX(freq_transpo) AS max_screenings
    FROM deriv_trans
")
message(sprintf("Transportation barriers: %d patients flagged (mean: %s screenings, max: %d)", 
                trans_count$n_patients, trans_count$mean_screenings, 
                trans_count$max_screenings))


# Display summary of all SDoH derived tables
message("\nSummary of social determinants of health derived tables:")
sdoh_tables <- dbGetQuery(db_con, "
    SELECT table_name 
    FROM information_schema.tables 
    WHERE table_name IN ('deriv_sdoh', 'deriv_smoke', 'deriv_trans')
    ORDER BY table_name
")
print(sdoh_tables)


# Display SDoH prevalence across cohort
message("\nSocial determinants of health prevalence:")
prevalence <- dbGetQuery(db_con, "
    SELECT 
        'Financial Strain' AS sdoh_domain,
        COUNT(DISTINCT deid_person_id) AS n_patients_flagged
    FROM deriv_sdoh
    
    UNION ALL
    
    SELECT 
        'Smoking/Exposure' AS sdoh_domain,
        COUNT(DISTINCT deid_person_id) AS n_patients_flagged
    FROM deriv_smoke
    
    UNION ALL
    
    SELECT 
        'Transportation Barriers' AS sdoh_domain,
        COUNT(DISTINCT deid_person_id) AS n_patients_flagged
    FROM deriv_trans
    
    ORDER BY n_patients_flagged DESC
")
print(prevalence)


# Calculate multiple SDoH burden
message("\nPatients with multiple SDoH barriers:")
multi_sdoh <- dbGetQuery(db_con, "
    WITH patient_sdoh AS (
        SELECT DISTINCT deid_person_id, 1 AS has_finance FROM deriv_sdoh
    ),
    patient_smoke AS (
        SELECT DISTINCT deid_person_id, 1 AS has_smoke FROM deriv_smoke
    ),
    patient_trans AS (
        SELECT DISTINCT deid_person_id, 1 AS has_trans FROM deriv_trans
    ),
    all_patients AS (
        SELECT DISTINCT deid_person_id 
        FROM observation
        WHERE observation_concept_id IN (
            37116643, 36306143, 36304041, 46234789, 4010022,
            903656, 903657, 3964826, 37020730
        )
    )
    SELECT 
        COALESCE(has_finance, 0) + COALESCE(has_smoke, 0) + COALESCE(has_trans, 0) AS n_sdoh_domains,
        COUNT(*) AS n_patients
    FROM all_patients AS ap
    LEFT JOIN patient_sdoh AS pf ON ap.deid_person_id = pf.deid_person_id
    LEFT JOIN patient_smoke AS ps ON ap.deid_person_id = ps.deid_person_id
    LEFT JOIN patient_trans AS pt ON ap.deid_person_id = pt.deid_person_id
    GROUP BY n_sdoh_domains
    ORDER BY n_sdoh_domains
")
print(multi_sdoh)


# Optimize database
message("\nOptimizing database...")
dbExecute(db_con, "CHECKPOINT")
dbExecute(db_con, "VACUUM")


# Close connection
dbDisconnect(db_con)
message("Social determinants of health derived tables created! Database connection closed.")
