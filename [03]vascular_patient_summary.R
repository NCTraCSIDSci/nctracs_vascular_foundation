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
    CREATE OR REPLACE TEMP VIEW view_cci AS
    SELECT cci.*, t.total_dx, t.unique_dx
    FROM deriv_calc_cci cci
        LEFT JOIN (SELECT deid_person_id,
                      count(condition_source_value) AS total_dx,
                      count(distinct condition_source_value) AS unique_dx
                  FROM condition_occurrence
                  GROUP BY deid_person_id) t
        ON cci.deid_person_id = t.deid_person_id
')

##create TEMP VIEW of visits data

dbExecute(con, '
    create TEMP VIEW view_visits as
    SELECT DEID_PERSON_ID,
    count(deid_visit_occurrence_id) AS visit_count,
    min(VISIT_START_DATE) AS visit_min,
    max(VISIT_START_DATE) AS visit_max
    FROM visit_occurrence
    GROUP BY DEID_PERSON_ID
')


##create view of GFR lab aggregations
dbExecute(con, "
    CREATE OR REPLACE TEMP VIEW view_gfr AS
        WITH gfr_values AS (
            SELECT * FROM measurement
            WHERE MEASUREMENT_CONCEPT_ID in (SELECT distinct concept_id FROM deriv_concept_freqs WHERE lower(concept_name) like ('%glomerular filtration%') and domain='measurement' )
        )
    SELECT distinct deid_person_id,
    count(measurement_id) AS gfr_freq,
    min(value_as_number) AS gfr_min,
    max(value_as_number) AS gfr_max,
    avg(value_as_number) AS gfr_mean
    FROM gfr_values
    GROUP BY DEID_PERSON_ID
")



##ALBUMIN LABS
dbExecute(con, "
    CREATE OR REPLACE TEMP VIEW view_albumin AS
    SELECT distinct deid_person_id,
        min(MEASUREMENT_DATE) AS alb_first,
        max(measurement_date) AS alb_last,
        count(measurement_id) AS alb_freq,
        min(VALUE_AS_NUMBER) AS alb_min,
        avg(VALUE_AS_NUMBER) AS alb_mean,
        max(VALUE_AS_NUMBER) AS alb_max
    FROM measurement WHERE measurement_concept_id in (3024561)
    GROUP BY deid_person_id
")

##LIPOPROTEIN A
dbExecute(con, '
    CREATE OR REPLACE TEMP VIEW view_lpa as
    SELECT
        deid_person_id,
        count(measurement_id) AS freq_lpa,
        avg(VALUE_AS_NUMBER) AS avg_lpa FROM measurement
    WHERE MEASUREMENT_CONCEPT_ID in (4007663,46284886,4197857,3013861,3046664)
    and VALUE_AS_NUMBER is not null
    GROUP BY DEID_PERSON_ID
')


##A1c LABS
##filter out impossible a1c values that are over 100 (in theory top end of a1c is around 20)
dbExecute(con, "
    CREATE OR REPLACE TEMP VIEW view_a1c as
    SELECT
        distinct deid_person_id,
        min(MEASUREMENT_DATE) AS a1c_first,
        max(measurement_date) AS a1c_last,
        count(measurement_id) AS a1c_freq,
        min(VALUE_AS_NUMBER) AS a1c_min,
        avg(VALUE_AS_NUMBER) AS a1c_mean,
        max(VALUE_AS_NUMBER) AS a1c_max
    FROM measurement
    WHERE measurement_concept_id in (3004410)
    and value_as_number<100
    GROUP BY deid_person_id
")

##LDL labs

##ancestor option A  4012479:  LDL cholesterol measurement -> gives 453,693 rows
##ancestor option B 4331302:  LDL measurement -> gives 454,687 - this gives weird nmol values

dbExecute(con, "
    CREATE OR REPLACE TEMP VIEW view_ldl as
    WITH LDL as(
        SELECT DISTINCT
            d.DEID_PERSON_ID,
            c.concept_name,
            d.*
        FROM  measurement d
            JOIN concept c ON d.measurement_CONCEPT_ID = c.concept_id
            JOIN deriv_concept_ancestors ca ON d.measurement_concept_id = ca.descendant_concept_id
        WHERE ca.ancestor_concept_id = '4012479'
            AND (value_as_number>0 and value_as_number<1000)
    )
    SELECT
        deid_person_id,
        count(measurement_id) AS ldl_freq,
        min(value_as_number) AS ldl_min,
        max(value_as_number) AS ldl_max,
        avg(value_as_number) AS ldl_mean
    FROM LDL
    GROUP BY deid_person_id
")


##aggregate anti-thrombotic meds
dbExecute(con, '
    CREATE OR REPLACE TEMP VIEW view_rx_thromb AS
    SELECT deid_person_id, count(drug_exposure_id) AS freq_antithromb
    FROM deriv_rx_thrombotic
    GROUP BY deid_person_id
')

##aggregate statins
dbExecute(con, '
    CREATE OR REPLACE TEMP VIEW view_rx_statin AS
    SELECT deid_person_id,
        min(drug_exposure_start_date) AS min_statin_date,
        max(drug_exposure_start_date) AS max_statin_date,
    count(drug_exposure_id) AS freq_statin_drug
    FROM deriv_rx_statin
    GROUP BY deid_person_id
')


##aggregate HTN meds
dbExecute(con, '
    CREATE OR REPLACE TEMP VIEW view_rx_htn as
    SELECT deid_person_id, min(drug_exposure_start_date) AS min_htn_drug,
    max(drug_exposure_start_date) AS max_htn_drug,
    count(drug_exposure_id) AS freq_htn_drug
    FROM deriv_rx_htn
    GROUP BY deid_person_id
')

##aggregate diabetes meds
dbExecute(con, '
    CREATE OR REPLACE TEMP VIEW view_rx_diab as
    SELECT deid_person_id, min(drug_exposure_start_date) AS min_diab_drug,
    max(drug_exposure_end_date) AS max_diab_drug,
    count(drug_exposure_id) AS freq_diab_drug
    FROM deriv_rx_diab
    GROUP BY deid_person_Id
')

##aggregate revascularizations
dbExecute(con, '
    CREATE OR REPLACE TEMP VIEW view_revasc as
    SELECT distinct DEID_PERSON_ID,
        min(PROCEDURE_DATE) AS min_revasc_date,
        max(PROCEDURE_DATE) AS max_revasc_date,
        count(distinct PROCEDURE_DATE) AS proc_days,
        sum(dilation) AS dilation,
        sum(bypass) AS bypass,
        sum(extirpation) AS extirpation,
        sum(arterectomy) AS arterectomy,
        sum(reoperation) AS reoperation,
        sum(thrombectomy) AS thrombectomy
    FROM deriv_revascularization
    GROUP BY DEID_PERSON_ID
')


##aggregate amputations
dbExecute(con, '
    CREATE OR REPLACE TEMP VIEW view_amp as
    SELECT distinct DEID_PERSON_ID,
        min(PROCEDURE_DATE) AS min_amp_date,
        max(PROCEDURE_DATE) AS max_amp_date,
        sum(amp_toe) AS amp_toe,
        sum(amp_foot) AS amp_foot,
        sum(amp_leg) AS amp_leg,
        sum(amp_revision) AS amp_revision
    FROM deriv_amputations
    GROUP BY DEID_PERSON_ID
')

##aggregate vascular_start
dbExecute(con,"
    CREATE OR REPLACE TEMP VIEW vascular_starts AS
    SELECT deid_person_id,
        MIN(START_DATE) AS vascular_start
    FROM deriv_vascular_dx
    GROUP BY deid_person_id")

##floor(date_diff(current_date(), p.birth_datetime)/365.25) AS age,


dbExecute(con,
    "CREATE OR REPLACE TABLE patient_summary as
    SELECT
        p.deid_person_id,
        datediff('year',p.DATA_DATE, p.birth_datetime) AS age,
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
        view_visits.visit_max,
        vascular_starts.vascular_start
    FROM person p
    LEFT JOIN state_death d ON p.deid_person_id=d.deid_person_id
    LEFT JOIN deriv_depression dep ON p.deid_person_id = dep.deid_person_id
    LEFT JOIN view_a1c a1c ON p.deid_person_id = a1c.deid_person_id
    LEFT JOIN view_lpa lpa ON p.DEID_PERSON_ID = lpa.deid_person_id
    LEFT JOIN view_gfr gfr ON p.DEID_PERSON_ID = gfr.deid_person_id
    LEFT JOIN view_albumin albumin ON p.DEID_PERSON_ID = albumin.deid_person_id
    LEFT JOIN view_ldl ldl ON p.DEID_PERSON_ID = ldl.deid_person_id
    LEFT JOIN view_rx_thromb trx ON p.DEID_PERSON_ID = trx.deid_person_id
    LEFT JOIN view_rx_statin srx ON p.DEID_PERSON_ID = srx.deid_person_id
    LEFT JOIN view_rx_htn hrx ON p.DEID_PERSON_ID = hrx.deid_person_id
    LEFT JOIN view_rx_diab drx ON p.DEID_PERSON_ID = drx.deid_person_id
    LEFT JOIN deriv_sdoh ON p.DEID_PERSON_ID = deriv_sdoh.deid_person_id
    LEFT JOIN deriv_smoke ON p.DEID_PERSON_ID = deriv_smoke.deid_person_id
    LEFT JOIN deriv_trans ON p.DEID_PERSON_ID = deriv_trans.deid_person_id
    LEFT JOIN vascular_starts ON p.DEID_PERSON_ID = vascular_starts.deid_person_id
    LEFT JOIN view_revasc rv ON p.DEID_PERSON_ID = rv.deid_person_id
    LEFT JOIN view_amp amp ON p.DEID_PERSON_ID = amp.deid_person_id
    LEFT JOIN view_cci ON p.DEID_PERSON_ID = view_cci.deid_person_id
    LEFT JOIN view_visits ON p.DEID_PERSON_ID = view_visits.deid_person_id")

dbExecute(con, "CHECKPOINT")

dbExecute(con, "VACUUM")

dbDisconnect(con)