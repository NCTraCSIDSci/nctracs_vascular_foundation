##This script creates derived tables of relevant medication concepts to facilitate subsequent aggregation / inclusion in the vascular patient summary table

##load packages
library(DBI)
library(duckdb)


##specify & connect to database
duckdb_file <- "Z:\\vascular.duckdb" 

con = dbConnect(duckdb::duckdb(), dbdir = duckdb_file)


##table of statin meds
dbExecute(con,"
create or replace table deriv_rx_statins as
with statins as (
  SELECT de.*
    FROM drug_exposure de
  INNER JOIN concept_ancestor ca ON de.drug_concept_id = ca.descendant_concept_id
  WHERE ca.ancestor_concept_id IN (
    1510813, 1545958, 1539403, 1551860, 1592085, 1549686, 40165636)
)
select distinct DRUG_EXPOSURE_ID,
deid_person_id,
DRUG_CONCEPT_ID,
drug_exposure_start_date,
refills,
quantity,
days_supply,
provider_id,
drug_source_value,
route_source_value
from statins
order by deid_person_id, drug_exposure_start_date
")

##table of anti-thrombotic meds

dbExecute(con,"
create or replace table deriv_rx_thrombotic as
with antithromb as (
  select * from drug_exposure where drug_concept_id in (select distinct descendant_concept_id from CONCEPT_ANCESTOR where ancestor_concept_id = 21600960)
)

select distinct DRUG_EXPOSURE_ID,
deid_person_id,
DRUG_CONCEPT_ID,
drug_exposure_start_date,
refills,
quantity,
days_supply,
provider_id,
drug_source_value,
route_source_value
from antithromb
order by deid_person_id, drug_exposure_start_date
")


##STATINS

dbExecute(con, "
CREATE OR REPLACE table deriv_rx_statin as
  SELECT de.*
    FROM drug_exposure de
  JOIN deriv_concept_ancestors ca ON de.drug_concept_id = ca.descendant_concept_id
  WHERE ca.ancestor_concept_id IN (
    1510813, 1545958, 1539403, 1551860, 1592085, 1549686, 40165636)
")

##ANTIHYPERTENSIVES

dbExecute(con, "
CREATE OR REPLACE table deriv_rx_htn as
  select b.concept_name, a.* 
    from drug_exposure a LEFT JOIN concept b on a.drug_concept_id = b.CONCEPT_ID
  where a.drug_concept_id IN (select distinct descendant_concept_id from deriv_concept_ancestors where ancestor_concept_id=21600381)
")


##ANTIDIABETES DRUGS

dbExecute(con, "
CREATE OR REPLACE table deriv_rx_diab AS
  select b.concept_name, a.* 
    from drug_exposure a LEFT JOIN concept b on a.drug_concept_id = b.CONCEPT_ID
  where a.drug_concept_id IN (select distinct descendant_concept_id from deriv_concept_ancestors where ancestor_concept_id=21600712)
")


dbExecute(con, "CHECKPOINT")

dbExecute(con, "VACUUM")

dbDisconnect(con)
