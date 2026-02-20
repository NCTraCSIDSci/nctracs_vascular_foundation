##Author:  Peter Leese

##Date Created: 12/30/2025

##Date Updated: 

##Purpose:  This file works with the PVL narrative and attempts to parse and extract the discrete float measurements and 
##attribute them to anatomical locations.  This is challenging -- there are many permutations to account for and this approach
##is essentially coming up with individual rules to handle as many of the permutations as possible within reason.  
##An LLM would most likely be a superior way to do this but it wasn't possible when we started the project.  It could be
##possible now with a large foundation model in databricks or a very small model that could attempt to be loaded 
##into a jupyter  notebook

##Original file:  databricks/[01b]vascular_pvl_clean.txt


##load packages
library(DBI)
library(duckdb)


##specify & connect to database
duckdb_file <- "Z:\\vascular.duckdb" 

con = dbConnect(duckdb::duckdb(), dbdir = duckdb_file)


##begin parsing and cleaning pvl data
##note that most of this process occurs by temporary views and only the final, cleaned table is physically written
dbExecute(con, "
CREATE OR REPLACE TEMP VIEW pvl_1 AS
SELECT
deid_person_id,
proc_start_date,
resulting_lab_name,
description,
narrative,

CASE 
WHEN regexp_matches(narrative, '\\d+\\.\\d+') THEN 1 
ELSE 0 
END AS float_check,

CASE 
WHEN lower(narrative) LIKE '%other%' THEN 'other' 
ELSE 'measurement' 
END AS type,

regexp_extract(lower(narrative), 'right\\s+(\\S+)', 1) AS right,
regexp_extract(lower(narrative), 'left\\s+(\\S+)', 1) AS left,

CASE 
WHEN lower(regexp_extract(lower(narrative), 'right\\s+(\\S+)', 1)) 
LIKE '%unobtain%' THEN 999 
END AS rt_unobtainable,

CASE 
WHEN lower(regexp_extract(lower(narrative), 'left\\s+(\\S+)', 1)) 
LIKE '%unobtain%' THEN 999 
END AS lt_unobtainable,

CASE 
WHEN lower(narrative) LIKE '%ankle%' 
OR lower(narrative) LIKE '%abi%' 
OR lower(narrative) LIKE '%tibial%' 
OR lower(narrative) LIKE '%peroneal%' 
OR lower(narrative) LIKE '%dorsalis%' 
OR lower(narrative) LIKE '%pta%'
OR lower(narrative) LIKE '%ata%' 
OR lower(narrative) LIKE '%dp%' THEN 'ankle'
WHEN lower(narrative) LIKE '%brachial%' THEN 'arm'
WHEN lower(narrative) LIKE '%toe%' 
OR lower(narrative) LIKE '%digit%' THEN 'toe'
END AS anatomy,

CASE 
WHEN lower(narrative) LIKE '%tibial%' THEN 'tibial'
WHEN lower(narrative) LIKE '%peroneal%' THEN 'peroneal'
WHEN lower(narrative) LIKE '%dorsalis%' THEN 'dorsalis'
WHEN lower(narrative) LIKE '%abi%' THEN 'abi'
END AS specific_anatomy,

CASE 
WHEN description LIKE '%RIGHT%' THEN 'right'
WHEN description LIKE '%LEFT%' THEN 'left'
WHEN description LIKE '%BILAT%' THEN 'bilateral'
END AS desc_laterality,

CASE 
WHEN lower(narrative) LIKE '%unobtainable%' THEN 1 
ELSE 0 
END AS unobtainable,

CASE 
WHEN lower(narrative) LIKE '%right%' 
OR lower(narrative) LIKE '%rt%' 
OR narrative LIKE '%R %' THEN 'right'
END AS narr_right,

CASE 
WHEN lower(narrative) LIKE '%left%' 
OR lower(narrative) LIKE '%lt%' 
OR narrative LIKE '%L %' THEN 'left'
END AS narr_left,

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
) t")


dbExecute(con, 
          "create or replace temp view pvl_unob as
select
deid_person_id,
proc_start_date,
resulting_lab_name,
description,
narrative,
anatomy,
specific_anatomy,
null as desc_laterality,
case when rt_unobtainable is not null then 'right' end as narr_right,
case when lt_unobtainable is not null then 'left' end as narr_left,
999 as value
from pvl_1
where type='measurement'
and unobtainable=1
and anatomy!='arm'
")

dbExecute(con, 
          "create or replace temp view pvl_array1 as
select 
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
value1 as value
from pvl_1
where type='measurement'
and float_check=1
and array_length=1
and desc_laterality!='bilateral'
and unobtainable=0")

dbExecute(con, 
          "create or replace temp view pvl_array1b as
select 
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
from pvl_1
where type='measurement'
and float_check=1
and array_length=1
and anatomy!='arm'
and desc_laterality='bilateral'
")


dbExecute(con, 
          "create or replace temp view pvl_array2 as
select 
deid_person_id,
proc_start_date,
resulting_lab_name,
description,
narrative,
anatomy, 
specific_anatomy, 
desc_laterality,
'right' as narr_right,
null as narr_left,
value1 as value
from pvl_1
where type='measurement'
and float_check=1
and array_length=2
and anatomy!='arm'

union all

select 
deid_person_id,
proc_start_date,
resulting_lab_name,
description,
narrative,
anatomy, 
specific_anatomy, 
desc_laterality,
null as narr_right,
'left' as narr_left,
value2 as value
from pvl_1
where type='measurement'
and float_check=1
and array_length=2
and anatomy!='arm'
")


##union all the temp views together into a physical, clean table
dbExecute(con,
          "
create or replace table deriv_pvl_final as
select * from pvl_unob
union all
select * from pvl_array1
union all
select * from pvl_array1b
union all
select * from pvl_array2
")


dbExecute(con, "CHECKPOINT")
dbExecute(con, "VACUUM")

dbDisconnect(con)

