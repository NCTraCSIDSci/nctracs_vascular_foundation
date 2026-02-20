##Author:  Peter Leese
##Date Created: 12/18/2025
##Date Updated: 
##Purpose:  This file loads the source EHR data for the vascular foundation project obtained from the EHR data team. 
##			The parquet format is compressing the data quite a bit, so there are several considerations for processing and loading the data 
##			via R. Files are written as tables to Duckdb to keep them compressed and make processing possible.  The source parquet files
##			become backup and the duckdb database is the primary working & analytic environment.  


install.packages(c("duckdb", "arrow", "DBI"))


library(arrow)
library(DBI)
library(duckdb)

# Create & connect to duckdb 
duckdb_file <- "Z:\\vascular.duckdb" # Path to your DuckDB file

con <- dbConnect(duckdb::duckdb(), dbdir = "Z:\\vascular.duckdb")

## Explicitly read each parquet file & write as a table to the database
dbExecute(con, 
          "CREATE OR REPLACE TABLE CONCEPT AS
          SELECT * 
          FROM read_parquet('Z:\\vascular data - DO NOT TOUCH\\CONCEPT.parquet')")

dbExecute(con, 
          "CREATE OR REPLACE TABLE OBSERVATION AS
          SELECT * 
          FROM read_parquet('Z:\\vascular data - DO NOT TOUCH\\OBSERVATION.parquet')")

dbExecute(con, 
          "CREATE OR REPLACE TABLE PERSON AS
          SELECT * 
          FROM read_parquet('Z:\\vascular data - DO NOT TOUCH\\PERSON.parquet')")

dbExecute(con, 
          "CREATE OR REPLACE TABLE VISIT_OCCURRENCE AS
          SELECT * 
          FROM read_parquet('Z:\\vascular data - DO NOT TOUCH\\VISIT_OCCURRENCE.parquet')")

dbExecute(con, 
          "CREATE OR REPLACE TABLE CONDITION_OCCURRENCE AS
          SELECT * 
          FROM read_parquet('Z:\\vascular data - DO NOT TOUCH\\CONDITION_OCCURRENCE.parquet')")

dbExecute(con, 
          "CREATE OR REPLACE TABLE PROCEDURE_OCCURRENCE AS
          SELECT * 
          FROM read_parquet('Z:\\vascular data - DO NOT TOUCH\\PROCEDURE_OCCURRENCE.parquet')")

dbExecute(con,
          "CREATE OR REPLACE TABLE DRUG_EXPOSURE AS
          SELECT * 
          FROM read_parquet('Z:\\vascular data - DO NOT TOUCH\\DRUG_EXPOSURE.parquet')")

dbExecute(con, 
          "CREATE OR REPLACE TABLE PVL_MEASUREMENTS AS
          SELECT * 
          FROM read_parquet('Z:\\vascular data - DO NOT TOUCH\\PVL_MEASUREMENTS.parquet')")

dbExecute(con, 
          "CREATE OR REPLACE TABLE STATE_DEATH AS
          SELECT * 
          FROM read_parquet('Z:\\vascular data - DO NOT TOUCH\\STATE_DEATH_DATA.parquet')")

dbExecute(con,
          "CREATE OR REPLACE TABLE CONCEPT_ANCESTOR AS
          SELECT *
          from read_parquet('Z:\\vascular data - DO NOT TOUCH\\CONCEPT_ANCESTOR.parquet')")

dbExecute(con,
          "CREATE OR REPLACE TABLE MEASUREMENT AS
          SELECT *
          from read_parquet('Z:\\vascular data - DO NOT TOUCH\\MEASUREMENT.parquet')")


dbGetQuery(con, "SHOW TABLES")

## CREATE CONCEPT_FREQS TABLE BY UNION
## this creates the fundamental concept_freqs table we use when working in OMOP

dbExecute(con, "
drop table if exists deriv_concept_freqs;

create table deriv_concept_freqs as (
select 'condition' as domain, count(CONDITION_CONCEPT_ID) as freq, concept_id, concept_name, CONDITION_SOURCE_VALUE 
from condition_occurrence a LEFT JOIN concept b on a.CONDITION_CONCEPT_ID = b.CONCEPT_ID
group by concept_id, concept_name, condition_source_value

UNION ALL

select 'procedure' as domain, count(procedure_concept_id) as freq, concept_id, concept_name, procedure_source_value
from procedure_occurrence a LEFT JOIN concept b on a.procedure_concept_id = b.CONCEPT_ID
group by concept_id, concept_name, procedure_source_value

UNION ALL

select 'observation' as domain, count(observation_concept_id) as freq, concept_id, concept_name, observation_source_value
from observation a LEFT JOIN concept b on a.observation_concept_id = b.CONCEPT_ID
group by concept_id, concept_name, observation_source_value

UNION ALL

select 'drug' as domain, count(drug_concept_id) as freq, concept_id, concept_name, drug_source_value
from drug_exposure a LEFT JOIN concept b on a.drug_concept_id = b.CONCEPT_ID
group by concept_id, concept_name, drug_source_value

UNION ALL

select 'measurement' as domain, count(measurement_concept_id) as freq, concept_id, concept_name, measurement_source_value
from measurement a LEFT JOIN concept b on a.measurement_concept_id = b.CONCEPT_ID
group by concept_id, concept_name, measurement_source_value

UNION ALL

select 'visit_occurrence' as domain, count(visit_concept_id) as freq, concept_id, concept_name, visit_source_value
from visit_occurrence a LEFT JOIN concept b on a.visit_concept_id = b.CONCEPT_ID
group by concept_id, concept_name, visit_source_value
)
")
dbGetQuery(con, 'select * from visit_occurrence limit 10')

## CREATE CONCEPT_ANCESTORS
## this creates the fundamental concept_ancestors table we use when working
## with OMOP as an ontology

dbExecute(con, "
drop table if exists deriv_concept_ancestors;

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
FROM CONCEPT_ANCESTOR ca
LEFT JOIN CONCEPT c1 ON ca.ancestor_concept_id = c1.concept_id
LEFT JOIN CONCEPT c2 ON ca.descendant_concept_id = c2.concept_id
LEFT JOIN deriv_concept_freqs cf1 ON ca.ancestor_concept_id = cf1.concept_id
LEFT JOIN deriv_concept_freqs cf2 ON ca.descendant_concept_id = cf2.concept_id
WHERE ca.ancestor_concept_id != ca.descendant_concept_id
AND NOT (cf1.freq IS NULL AND cf2.freq IS NULL)
")


##call duckdb to specify physical tables created to confirm success
dbGetQuery(con, "SHOW TABLES")

##checkpoint & vacuum
dbExecute(con, "CHECKPOINT")
dbExecute(con, "VACUUM")


##close database connection
dbDisconnect(con)

