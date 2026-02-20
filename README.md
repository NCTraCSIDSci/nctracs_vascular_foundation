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
Creates a DuckDB database file (vascular.duckdb) containing:

- All input OMOP tables
- `deriv_concept_freqs`: Concept usage frequencies across domains
- `deriv_concept_ancestors`: OMOP vocabulary hierarchies filtered to concepts present in data
  
## Authors
Peter Leese

## Support
The project described was supported by the National Center for Advancing Translational Sciences (NCATS), National Institutes of Health, through Grant Award Number UM1TR004406. The content is solely the responsibility of the authors and does not necessarily represent the official views of the NIH.

## Citation
If you use this software in your work, please cite it using the CITATION.cff file or by clicking "Cite this repository" on the right.

## License
Copyright (c) 2024 The University of North Carolina at Chapel Hill

This project is licensed under the MIT License - see the LICENSE file for details.
