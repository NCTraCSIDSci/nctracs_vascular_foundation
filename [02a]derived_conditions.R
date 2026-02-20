##Author:  Peter Leese

##Date Created: 12/22/2025

##Date Updated: 

##Purpose:   Create derived tables focused on conditions as source  

##Original File: 

##load packages
library(DBI)
library(duckdb)

##specify & connect to database
duckdb_file <- "Z:\\vascular.duckdb" 

con = dbConnect(duckdb::duckdb(), dbdir = duckdb_file)


##CREATE VASCULAR DX TABLE
##THIS REPLICATES THE ORIGINAL COHORT DX INCLUSION CRITERIA
##THIS ESSENTIALLY TELLS US WHY THE PT IS INCLUDED IN THIS DATASET

dbExecute(con, 
          "          
drop table if exists deriv_vascular_dx;

create table deriv_vascular_dx as

select ROW_NUMBER() OVER(PARTITION BY deid_person_id ORDER BY condition_start_date) as row_number, x.* from (
  
  select * from condition_occurrence
  where condition_source_value in ('I73.89','I73.9','I77.70','I77.72','I77.76','I77.77','I77.79','I79.8') 
  or substr(condition_source_value, 1,5) in ('E08.5','E09.5','E10.5','E11.5','E13.5')
  or substr(condition_source_value, 1,3) in ('I70','I74','I75','I76','I96','L97')
  and substr(condition_source_value,1,3) not in ('I71','I72') and condition_source_value not in ('I70.1')
  order by deid_person_id, condition_start_date
) x
")

##DEPRESSIVE DISORDER
dbExecute(con, "
CREATE OR REPLACE table deriv_depression as
select distinct deid_person_id, count(distinct condition_start_date) as freq_depres from (
  select b.concept_name, a.* 
    from condition_occurrence a LEFT JOIN concept b on a.condition_concept_id = b.CONCEPT_ID
  where a.condition_concept_id IN (select distinct descendant_concept_id from deriv_concept_ancestors where ancestor_concept_id=440383)
) X group by deid_person_id
")


dbExecute(con, "CHECKPOINT")
dbExecute(con, "VACUUM")

dbDisconnect(con)
