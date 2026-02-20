##Author:  Peter Leese

##Date Created: 12/22/2025

##Date Updated: 

##Purpose:   Create derived tables focused on SDoH from observations table 

##Original File: 

##load packages
library(DBI)
library(duckdb)

##specify & connect to database
duckdb_file <- "Z:\\vascular.duckdb" 

con = dbConnect(duckdb::duckdb(), dbdir = duckdb_file)

##financial for food (or housing, medical care, heating)
##excluded are clear negative responses
##included are positive or choose not to respond or don't know

dbExecute(con, '
CREATE OR REPLACE TABLE deriv_sdoh as
with finance as(
select b.concept_name, a.* from observation a LEFT JOIN concept b on a.observation_concept_id = b.CONCEPT_ID
where observation_concept_id in (37116643,36306143,36304041,46234789,4010022)
and VALUE_AS_CONCEPT_ID not in (36309869,21498606,1621261)
AND VALUE_AS_STRING is not null
)
select distinct deid_person_id, count(observation_id) as freq_finance
from finance
group by deid_person_id
')


##active cigarette smoker or passive smoker

dbExecute(con, '
CREATE OR REPLACE TABLE deriv_smoke as
select deid_person_id, 
min(observation_date) as min_smoking,
max(observation_date) as max_smoking,
count(observation_id) as freq_smoking
 from observation
where observation_concept_id in (903656, 903657) 
group by deid_person_id
')


##TRANSPORTATION ISSUES
##excludes response of 'no'
##includes both affirmative and choose not to respond

dbExecute(con, "
CREATE OR REPLACE TABLE deriv_trans as
with transpo as(
select b.concept_name, a.* from observation a LEFT JOIN concept b on a.observation_concept_id = b.CONCEPT_ID
where observation_concept_id in (3964826, 37020730)
and value_as_string !='No'
)
select deid_person_id, count(observation_id) as freq_transpo
from transpo
group by deid_person_id
")


dbExecute(con, "CHECKPOINT")

dbExecute(con, "VACUUM")

dbDisconnect(con)