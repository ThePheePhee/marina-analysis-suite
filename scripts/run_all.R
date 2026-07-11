# Convenience runner for the full analysis pipeline.

source("scripts/01_import_clean.R")
source("scripts/02_descriptives.R")
source("scripts/03_primary_baseline.R")
source("scripts/04_prepost_change.R")
source("scripts/05_exploratory_age_type_verification.R")
source("scripts/06_sensitivity_ae_unknown.R")
source("scripts/07_key_differences.R")
source("scripts/08_observations.R")

message("Full analysis pipeline complete.")
