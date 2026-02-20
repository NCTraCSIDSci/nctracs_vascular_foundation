##Author:  Peter Leese

##Date Created: 12/22/2025

##Date Updated: 

##Purpose:   Create derived tables focused on labs as source  

##Original File: 

##load packages
library(DBI)
library(duckdb)

##specify & connect to database
duckdb_file <- "Z:\\vascular.duckdb" 

con = dbConnect(duckdb::duckdb(), dbdir = duckdb_file)

##GFR
dbExecute(con, "
CREATE OR REPLACE table deriv_gfr as
with gfr_values as(
  select * from measurement
  where MEASUREMENT_CONCEPT_ID in (select distinct concept_id from deriv_concept_freqs where lower(concept_name) like ('%glomerular filtration%') and domain='measurement' )
)
select distinct deid_person_id, 
count(measurement_id) as gfr_freq,
min(value_as_number) as gfr_min,
max(value_as_number) as gfr_max,
avg(value_as_number) as gfr_mean
from gfr_values
group by DEID_PERSON_ID
")



##ALBUMIN LABS
dbExecute(con, "
CREATE OR REPLACE TABLE deriv_albumin as
select distinct deid_person_id, 
min(MEASUREMENT_DATE) as alb_first, 
max(measurement_date) as alb_last, 
count(measurement_id) as alb_freq,
min(VALUE_AS_NUMBER) as alb_min,
avg(VALUE_AS_NUMBER) as alb_mean,
max(VALUE_AS_NUMBER) as alb_max
from measurement where measurement_concept_id in (3024561)
group by deid_person_id
")

##LIPOPROTEIN A

dbExecute(con, "
CREATE OR REPLACE TABLE deriv_lpa as
select deid_person_id, 
count(measurement_id) as freq_lpa,
avg(VALUE_AS_NUMBER) as avg_lpa from measurement 
where MEASUREMENT_CONCEPT_ID in (4007663,46284886,4197857,3013861,3046664)
and VALUE_AS_NUMBER is not null
group by DEID_PERSON_ID
")


##A1c LABS
##filter out impossible a1c values that are over 100 (in theory top end of a1c is around 20)
dbExecute(con, "
CREATE OR REPLACE TABLE deriv_a1c as
select distinct deid_person_id, 
min(MEASUREMENT_DATE) as a1c_first, 
max(measurement_date) as a1c_last, 
count(measurement_id) as a1c_freq,
min(VALUE_AS_NUMBER) as a1c_min,
avg(VALUE_AS_NUMBER) as a1c_mean,
max(VALUE_AS_NUMBER) as a1c_max
from measurement 
where measurement_concept_id in (3004410)
and value_as_number<100
group by deid_person_id
")

##LDL labs

##ancestor option A  4012479:  LDL cholesterol measurement -> gives 453,693 rows
##ancestor option B 4331302:  LDL measurement -> gives 454,687 - this gives weird nmol values

dbExecute(con, "
CREATE OR REPLACE TABLE deriv_ldl as
WITH LDL as(
  SELECT DISTINCT d.DEID_PERSON_ID, c.concept_name, d.*
    FROM  measurement d  
  JOIN concept c ON d.measurement_CONCEPT_ID = c.concept_id
  JOIN deriv_concept_ancestors ca ON d.measurement_concept_id = ca.descendant_concept_id
  WHERE ca.ancestor_concept_id = '4012479'
  and (value_as_number>0 and value_as_number<1000)
) 
select deid_person_id, 
count(measurement_id) as ldl_freq,
min(value_as_number) as ldl_min,
max(value_as_number) as ldl_max,
avg(value_as_number) as ldl_mean
from LDL 
group by deid_person_id
")


dbExecute(con, "CHECKPOINT")

dbExecute(con, "VACUUM")
dbDisconnect(con)