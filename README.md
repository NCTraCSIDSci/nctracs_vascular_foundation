[![DOI](https://zenodo.org/badge/1073823077.svg)](https://doi.org/10.5281/zenodo.20090512)

# OMOP CDM Data Loading for Vascular Foundation Project


This repository contains code developed by the TraCS Data Science Lab, which is part of the School of Medicine at the University of North Carolina at Chapel Hill. This code may have been modified from its original form to protect proprietary information and improve interpretability out of context. For example, most paths have been removed and some table field names have been changed.

## Description

This R script loads EHR data in OMOP Common Data Model (CDM) format from parquet files into a DuckDB database. The parquet format provides significant compression, and DuckDB maintains this compression while enabling efficient SQL querying without loading entire datasets into R memory. 

The script creates:
- OMOP CDM tables (Concept, Person, Observation, etc.)
- Derived concept frequency table for phenotype development
- Derived concept ancestor table for vocabulary hierarchy navigation

## Code Source Environment Notes

This code was developed in R (version 4.0+) using RStudio. It requires the following R packages:
- `duckdb` - For database creation and management
- `arrow` - For parquet file handling
- `DBI` - For database interface

The code uses DuckDB's SQL dialect, which is PostgreSQL-compatible with some extensions.

## Requirements

Install required R packages:

```r
install.packages(c("duckdb", "arrow", "DBI"))
```

## Usage
1. Clone this repository
2. Update the configuration paths at the top of the script:
   - base_dir: Your working directory
   - parquet_dir: Location of your OMOP parquet files
3. Ensure all required parquet files are present (see Input Files section)
4. Run the script in R or RStudio

## Input Files  
AS written he script expects the following parquet files in the specified directory, though this can easily be changed to other storage formats:

- CONCEPT.parquet
- OBSERVATION.parquet
- PERSON.parquet
- VISIT_OCCURRENCE.parquet
- CONDITION_OCCURRENCE.parquet
- PROCEDURE_OCCURRENCE.parquet
- DRUG_EXPOSURE.parquet
- PVL_MEASUREMENTS.parquet
- STATE_DEATH_DATA.parquet
- CONCEPT_ANCESTOR.parquet
- MEASUREMENT.parquet
  
## Output  
Creates a DuckDB database file (`vascular.duckdb`) containing the OMOP-based vascular extension framework that consists of:

- All input OMOP tables
- `deriv_concept_freqs`: Concept usage frequencies across domains
- `deriv_concept_ancestors`: OMOP vocabulary hierarchies filtered to concepts present in data
- `deriv_pvl_final`: final, cleaned PVL data attempting to extract discrete measurements from text
- `deriv_calc_cci`: table of Charlson comorbidity index scores
`deriv_vascular_dx`: table to recreate the vascular inclusion diagnoses for the cohort
`deriv_depression`: table to identify just depression conditions for patients
`deriv_revascularization`: table to identify all revascularizations procedures for patients
`deriv_amputations`: table to identify all amputations for patients
`deriv_rx_statin`: table to identify all statin medications for patients from DRUG_EXPOSURE
`deriv_rx_antithrombotic`: same as statins but for antithrombotics
`deriv_rx_htn`:  same as other medications but for hypertensive medications
`deriv_rx_diab`: same as as other medications but for medications used for diabetes
`deriv_gfr`:  table to identify all glomerular filtration rate labs from MEASUREMENTS
`deriv_albumin`:  same as gfr but for albumin labs
`deriv_lpa`:  same as other labs but for lipoprotein a
`deriv_a1c`:  same but for hemoglobin A1c
`deriv_ldl`:  same but for cholesterol, specifically low-density lipoprotein
`deriv_sdoh`: table to identify discrete social determinant of health responses from patients in the OBSERVATION table
`deriv_smoke`:  all patient smoking responses and assessments from the OBSERVATION table 
`deriv_trans`: any indications of transportation issues for patients in the OBSERVATION table
`patient_summary`:  the final, patient-level table that is assembled from both OMOP input tables for the cohort, as well as derived tables
  
## Authors
Peter Leese, Tomas McIntee

## Support
The project described was supported by the National Center for Advancing Translational Sciences (NCATS), National Institutes of Health, through Grant Award Number UM1TR004406. The content is solely the responsibility of the authors and does not necessarily represent the official views of the NIH.

## Citation
If you use this software in your work, please cite it using the CITATION.cff file or by clicking "Cite this repository" on the right.

## License
Copyright (c) 2026 The University of North Carolina at Chapel Hill

This project is licensed under the MIT License - see the LICENSE file for details.
