# Load EHR Source Data to DuckDB Database
# Author(s): Peter Leese
# Started on 2024-12-18
# 
# This file loads source EHR data in parquet format for the vascular foundation 
# project and writes tables to DuckDB for compressed storage and analysis. 
# The parquet format compresses data significantly, so DuckDB is used to maintain 
# compression while enabling efficient querying. Source parquet files serve as 
# backup while the DuckDB database becomes the primary working environment.
# 
# © 2024, The University of North Carolina at Chapel Hill. The code is licensed 
# under the MIT License and permission is granted to use in accordance with the 
# MIT License.
#
# INPUTS:
#   - Parquet files in specified directory (OMOP CDM format)
#     * CONCEPT.parquet, PERSON.parquet, OBSERVATION.parquet, etc.
# 
# OUTPUTS:
#   - vascular.duckdb: DuckDB database with imported OMOP tables
#   - deriv_concept_freqs: Derived table with concept frequencies across domains
#   - deriv_concept_ancestors: Derived table with OMOP vocabulary hierarchies
#
# USAGE:
#   1. Update base_dir and parquet_dir paths below for your environment
#   2. Ensure all required parquet files are in the specified directory
#   3. Run script to create and populate DuckDB database


# Load required libraries
library(arrow)
library(DBI)
library(duckdb)


# Configuration - UPDATE THESE PATHS FOR YOUR ENVIRONMENT
base_dir <- "path/to/your/working/directory"
parquet_dir <- file.path(base_dir, "source_parquet_files")
duckdb_file <- file.path(base_dir, "vascular.duckdb")


# Create and connect to DuckDB database
db_con <- dbConnect(duckdb::duckdb(), dbdir = duckdb_file)

# Verify connection was successful
if (!dbIsValid(db_con)) {
  stop("Failed to connect to DuckDB database at: ", duckdb_file)
}

message("Successfully connected to DuckDB database")


# Define OMOP CDM tables to load
# Using named vector to handle cases where parquet filename differs from table name
omop_tables <- c(
  "CONCEPT" = "CONCEPT",
  "OBSERVATION" = "OBSERVATION",
  "PERSON" = "PERSON",
  "VISIT_OCCURRENCE" = "VISIT_OCCURRENCE",
  "CONDITION_OCCURRENCE" = "CONDITION_OCCURRENCE",
  "PROCEDURE_OCCURRENCE" = "PROCEDURE_OCCURRENCE",
  "DRUG_EXPOSURE" = "DRUG_EXPOSURE",
  "PVL_MEASUREMENTS" = "PVL_MEASUREMENTS",
  "STATE_DEATH" = "STATE_DEATH_DATA",
  "CONCEPT_ANCESTOR" = "CONCEPT_ANCESTOR",
  "MEASUREMENT" = "MEASUREMENT"
)


# Load each parquet file as a table in DuckDB
# Using DuckDB's read_parquet() function allows loading without fully reading 
# into R memory, which is critical for large datasets
message("Loading OMOP tables from parquet files...")

for (table_name in names(omop_tables)) {
  parquet_file <- omop_tables[table_name]
  parquet_path <- file.path(parquet_dir, paste0(parquet_file, ".parquet"))
  
  message(sprintf("  Loading %s...", table_name))
  
  sql_query <- sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT * 
     FROM read_parquet('%s')",
    table_name,
    parquet_path
  )
  
  dbExecute(db_con, sql_query)
}

message("All OMOP tables loaded successfully")


# Verify tables were created
tables_created <- dbGetQuery(db_con, "SHOW TABLES")
message(sprintf("Total tables in database: %d", nrow(tables_created)))
print(tables_created)


# Create derived table: concept frequencies across all domains
# This table aggregates concept usage across all clinical domains to enable
# quick lookup of concept prevalence in the dataset, which is useful for
# phenotype development and data quality assessment
message("Creating derived table: deriv_concept_freqs...")

dbExecute(db_con, "
DROP TABLE IF EXISTS deriv_concept_freqs;

CREATE TABLE deriv_concept_freqs AS (
    SELECT 'condition' AS domain, 
           COUNT(condition_concept_id) AS freq, 
           concept_id, 
           concept_name, 
           condition_source_value AS source_value
    FROM condition_occurrence AS a 
    LEFT JOIN concept AS b 
        ON a.condition_concept_id = b.concept_id
    GROUP BY concept_id, concept_name, condition_source_value

    UNION ALL

    SELECT 'procedure' AS domain, 
           COUNT(procedure_concept_id) AS freq, 
           concept_id, 
           concept_name, 
           procedure_source_value AS source_value
    FROM procedure_occurrence AS a 
    LEFT JOIN concept AS b 
        ON a.procedure_concept_id = b.concept_id
    GROUP BY concept_id, concept_name, procedure_source_value

    UNION ALL

    SELECT 'observation' AS domain, 
           COUNT(observation_concept_id) AS freq, 
           concept_id, 
           concept_name, 
           observation_source_value AS source_value
    FROM observation AS a 
    LEFT JOIN concept AS b 
        ON a.observation_concept_id = b.concept_id
    GROUP BY concept_id, concept_name, observation_source_value

    UNION ALL

    SELECT 'drug' AS domain, 
           COUNT(drug_concept_id) AS freq, 
           concept_id, 
           concept_name, 
           drug_source_value AS source_value
    FROM drug_exposure AS a 
    LEFT JOIN concept AS b 
        ON a.drug_concept_id = b.concept_id
    GROUP BY concept_id, concept_name, drug_source_value

    UNION ALL

    SELECT 'measurement' AS domain, 
           COUNT(measurement_concept_id) AS freq, 
           concept_id, 
           concept_name, 
           measurement_source_value AS source_value
    FROM measurement AS a 
    LEFT JOIN concept AS b 
        ON a.measurement_concept_id = b.concept_id
    GROUP BY concept_id, concept_name, measurement_source_value

    UNION ALL

    SELECT 'visit_occurrence' AS domain, 
           COUNT(visit_concept_id) AS freq, 
           concept_id, 
           concept_name, 
           visit_source_value AS source_value
    FROM visit_occurrence AS a 
    LEFT JOIN concept AS b 
        ON a.visit_concept_id = b.concept_id
    GROUP BY concept_id, concept_name, visit_source_value
)
")

message("deriv_concept_freqs table created successfully")


# Create derived table: concept ancestor relationships
# This table provides the hierarchical relationships in the OMOP vocabulary,
# enabling traversal of parent-child concept relationships. We exclude self-
# relationships and concept pairs where neither ancestor nor descendant appears
# in the actual data to reduce table size
message("Creating derived table: deriv_concept_ancestors...")

dbExecute(db_con, "
DROP TABLE IF EXISTS deriv_concept_ancestors;

CREATE TABLE IF NOT EXISTS deriv_concept_ancestors AS
SELECT DISTINCT
    c1.domain_id AS ancestor_domain,
    ca.ancestor_concept_id,
    c1.concept_name AS ancestor,
    cf1.freq AS ancestor_freq,
    '---------------' AS buffer,
    c2.domain_id AS descendant_domain,
    ca.descendant_concept_id,
    c2.concept_name AS descendant,
    cf2.freq AS descendant_freq,
    ca.min_levels_of_separation,
    ca.max_levels_of_separation
FROM concept_ancestor AS ca
LEFT JOIN concept AS c1 
    ON ca.ancestor_concept_id = c1.concept_id
LEFT JOIN concept AS c2 
    ON ca.descendant_concept_id = c2.concept_id
LEFT JOIN deriv_concept_freqs AS cf1 
    ON ca.ancestor_concept_id = cf1.concept_id
LEFT JOIN deriv_concept_freqs AS cf2 
    ON ca.descendant_concept_id = cf2.concept_id
WHERE ca.ancestor_concept_id != ca.descendant_concept_id
    AND NOT (cf1.freq IS NULL AND cf2.freq IS NULL)
")

message("deriv_concept_ancestors table created successfully")


# Display final table list
message("\nFinal database contents:")
final_tables <- dbGetQuery(db_con, "SHOW TABLES")
print(final_tables)


# Optimize database performance
# CHECKPOINT ensures all changes are written to disk
# VACUUM reclaims space and optimizes file structure
message("\nOptimizing database...")
dbExecute(db_con, "CHECKPOINT")
dbExecute(db_con, "VACUUM")
message("Database optimization complete")


# Close database connection
dbDisconnect(db_con)
message("\nDatabase connection closed. Setup complete!")
