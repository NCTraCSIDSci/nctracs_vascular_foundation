##Author:  Peter Leese

##Date Created: 12/22/2025

##Date Updated: 1/12/2026

##Purpose:   Create the patient summary table with required aggregation views at top of script.

##Original File: 


##load packages
library(DBI)
library(duckdb)


##specify & connect to database
duckdb_file <- "Z:\\vascular.duckdb" 

con = dbConnect(duckdb::duckdb(), dbdir = duckdb_file)



##CREATE VIEW for CCI data & diagnosis counts

dbExecute(con, '
create or replace temp view view_cci as
select cci.*, t.total_dx, t.unique_dx 
from deriv_calc_cci cci 
LEFT JOIN (select deid_person_id, count(condition_source_value) as total_dx, count(distinct condition_source_value) as unique_dx from condition_occurrence group by deid_person_id) t
ON cci.deid_person_id = t.deid_person_id
')

##create temp view of visits data

dbExecute(con, '
create temp view view_visits as
select DEID_PERSON_ID,
count(deid_visit_occurrence_id) as visit_count,
min(VISIT_START_DATE) as visit_min,
max(VISIT_START_DATE) as visit_max
from visit_occurrence
group by DEID_PERSON_ID
')


##create view of GFR lab aggregations
dbExecute(con, "
CREATE OR REPLACE temp view view_gfr as
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
CREATE OR REPLACE TEMP VIEW view_albumin as
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
dbExecute(con, '
CREATE OR REPLACE TEMP VIEW view_lpa as
select deid_person_id, 
count(measurement_id) as freq_lpa,
avg(VALUE_AS_NUMBER) as avg_lpa from measurement 
where MEASUREMENT_CONCEPT_ID in (4007663,46284886,4197857,3013861,3046664)
and VALUE_AS_NUMBER is not null
group by DEID_PERSON_ID
')


##A1c LABS
##filter out impossible a1c values that are over 100 (in theory top end of a1c is around 20)
dbExecute(con, "
CREATE OR REPLACE TEMP VIEW view_a1c as
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
CREATE OR REPLACE TEMP VIEW view_ldl as
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


##aggregate anti-thrombotic meds
dbExecute(con, '
CREATE OR REPLACE TEMP VIEW view_rx_thromb as
select deid_person_id, count(drug_exposure_id) as freq_antithromb 
from deriv_rx_thrombotic
group by deid_person_id
')

##aggregate statins
dbExecute(con, '
CREATE OR REPLACE TEMP VIEW view_rx_statin as
select deid_person_id, min(drug_exposure_start_date) as min_statin_date,
max(drug_exposure_start_date) as max_statin_date,
count(drug_exposure_id) as freq_statin_drug
from deriv_rx_statin
group by deid_person_id
')


##aggregate HTN meds
dbExecute(con, '
CREATE OR REPLACE TEMP VIEW view_rx_htn as
select deid_person_id, min(drug_exposure_start_date) as min_htn_drug,
max(drug_exposure_start_date) as max_htn_drug,
count(drug_exposure_id) as freq_htn_drug
from deriv_rx_htn
group by deid_person_id
')

##aggregate diabetes meds
dbExecute(con, '
CREATE OR REPLACE TEMP VIEW view_rx_diab as
select deid_person_id, min(drug_exposure_start_date) as min_diab_drug, 
max(drug_exposure_end_date) as max_diab_drug,
count(drug_exposure_id) as freq_diab_drug
from deriv_rx_diab
group by deid_person_Id
')

##aggregate revascularizations
dbExecute(con, '
CREATE OR REPLACE TEMP VIEW view_revasc as
select distinct DEID_PERSON_ID, 
min(PROCEDURE_DATE) as min_revasc_date, 
max(PROCEDURE_DATE) as max_revasc_date, 
count(distinct PROCEDURE_DATE) as proc_days,
sum(dilation) as dilation,
sum(bypass) as bypass,
sum(extirpation) as extirpation,
sum(arterectomy) as arterectomy,
sum(reoperation) as reoperation,
sum(thrombectomy) as thrombectomy
from deriv_revascularization
group by DEID_PERSON_ID
')


##aggregate amputations
dbExecute(con, '
CREATE OR REPLACE TEMP VIEW view_amp as
select distinct DEID_PERSON_ID, 
min(PROCEDURE_DATE) as min_amp_date, 
max(PROCEDURE_DATE) as max_amp_date, 
sum(amp_toe) as amp_toe,
sum(amp_foot) as amp_foot,
sum(amp_leg) as amp_leg,
sum(amp_revision) as amp_revision
from deriv_amputations
group by DEID_PERSON_ID
')

##floor(date_diff(current_date(), p.birth_datetime)/365.25) as age,


dbExecute(con, '
CREATE OR REPLACE TABLE patient_summary as
select 
p.deid_person_id,
p.gender_source_value,

p.race_concept_id,
p.ethnicity_concept_id,
d.death_date, 
d.cod1,
dep.freq_depres,
a1c.a1c_first,
a1c.a1c_last,
a1c.a1c_freq,
a1c.a1c_min,
a1c.a1c_mean,
a1c.a1c_max,
lpa.freq_lpa,
lpa.avg_lpa,
gfr.gfr_freq,
gfr.gfr_min,
gfr.gfr_mean,
gfr.gfr_max,
albumin.alb_freq,
albumin.alb_min,
albumin.alb_mean,
albumin.alb_max,
ldl.ldl_freq,
ldl.ldl_min,
ldl.ldl_mean,
ldl.ldl_max,
trx.freq_antithromb,
srx.min_statin_date,
srx.max_statin_date,
srx.freq_statin_drug,
hrx.min_htn_drug,
hrx.max_htn_drug,
hrx.freq_htn_drug,
drx.min_diab_drug,
drx.max_diab_drug,
drx.freq_diab_drug,
deriv_sdoh.freq_finance,
deriv_smoke.min_smoking,
deriv_smoke.max_smoking,
deriv_smoke.freq_smoking,
deriv_trans.freq_transpo,
rv.min_revasc_date,
rv.max_revasc_date,
rv.dilation,
rv.bypass,
rv.extirpation,
rv.arterectomy,
rv.reoperation,
rv.thrombectomy,
amp.min_amp_date,
amp.max_amp_date,
amp.amp_toe,
amp.amp_foot,
amp.amp_leg,
amp.amp_revision,
view_cci.cci,
view_cci.total_dx,
view_cci.unique_dx,
view_visits.visit_count,
view_visits.visit_min,
view_visits.visit_max
from person p LEFT JOIN state_death d on p.deid_person_id=d.deid_person_id
LEFT JOIN deriv_depression dep on p.deid_person_id = dep.deid_person_id
LEFT JOIN view_a1c a1c on p.deid_person_id = a1c.deid_person_id
LEFT JOIN view_lpa lpa on p.DEID_PERSON_ID = lpa.deid_person_id
LEFT JOIN view_gfr gfr on p.DEID_PERSON_ID = gfr.deid_person_id
LEFT JOIN view_albumin albumin on p.DEID_PERSON_ID = albumin.deid_person_id
LEFT JOIN view_ldl ldl on p.DEID_PERSON_ID = ldl.deid_person_id
LEFT JOIN view_rx_thromb trx on p.DEID_PERSON_ID = trx.deid_person_id
LEFT JOIN view_rx_statin srx on p.DEID_PERSON_ID = srx.deid_person_id
LEFT JOIN view_rx_htn hrx on p.DEID_PERSON_ID = hrx.deid_person_id
LEFT JOIN view_rx_diab drx on p.DEID_PERSON_ID = drx.deid_person_id
LEFT JOIN deriv_sdoh on p.DEID_PERSON_ID = deriv_sdoh.deid_person_id
LEFT JOIN deriv_smoke on p.DEID_PERSON_ID = deriv_smoke.deid_person_id
LEFT JOIN deriv_trans on p.DEID_PERSON_ID = deriv_trans.deid_person_id
LEFT JOIN view_revasc rv on p.DEID_PERSON_ID = rv.deid_person_id
LEFT JOIN view_amp amp on p.DEID_PERSON_ID = amp.deid_person_id
LEFT JOIN view_cci on p.DEID_PERSON_ID = view_cci.deid_person_id
LEFT JOIN view_visits on p.DEID_PERSON_ID = view_visits.deid_person_id
')

dbExecute(con, "CHECKPOINT")

dbExecute(con, "VACUUM")

dbDisconnect(con)