# Loads the package and requirements without actually installing anything (avoiding environmental issues)
if(!requireNamespace("dplyr",quietly = TRUE)) {install.packages("dplyr")}
if(!requireNamespace("duckdb",quietly = TRUE)) {install.packages("duckdb")}
# if(!requireNamespace("sparklyr",quietly = TRUE)) {install.packages("sparklyr")}
require(dplyr)
require(duckdb)
# require(sparklyr)
source("Dimensional/rolled_glm_coeff.R")
source("Dimensional/prune_leaves.R")
source("Dimensional/rolled_covariances.R")
source("Dimensional/roll_cov_glm.R")
source("Dimensional/select_targets.R")
source("Dimensional/find_leaves.R")
source("Dimensional/create_extant_network.R")
source("Dimensional/generate_fast_roll_table.R")
source("Dimensional/generate_rollup_candidates.R")
source("Dimensional/generate_priority_table.R")
source("Dimensional/model_build.R")
source("Dimensional/test_target.R")
source("Dimensional/slim_features.R")
#load("Dimensional/concept_ancestor.Rda")
#load("Dimensional/concept.Rda")
con <- dbConnect(duckdb::duckdb(), dbdir = "Z:\\vascular.duckdb",read_only = TRUE)
concept_ancestor <- dbReadTable(con,"concept_ancestor")
concept <- dbReadTable(con,"concept")

# Prefixed versions

names(concept) <- tolower(names(concept))
names(concept_ancestor) <- tolower(names(concept_ancestor))
concept<- concept %>%
    mutate(concept_id = paste0("F_",concept_id))
concept_ancestor <- concept_ancestor %>%
    mutate(ancestor_concept_id = paste0("F_",ancestor_concept_id),
        descendant_concept_id = paste0("F_",descendant_concept_id))

dbExecute(con, "CHECKPOINT")
dbExecute(con, "VACUUM")
dbDisconnect(con,  shutdown="TRUE")