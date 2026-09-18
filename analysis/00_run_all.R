# Run the complete HGEN 603 analysis from the repository root.
# Review each script before sourcing this file. Data extraction can be costly.

source("analysis/00_setup.R")

analysis_scripts <- c(
  "analysis/01_demographics.R",
  "analysis/02_conditions.R",
  "analysis/03_medications_and_trd_phenotype.R",
  "analysis/04_measurements.R",
  "analysis/05_family_history.R",
  "analysis/06_healthcare_utilization.R",
  "analysis/07_merge_and_quality_checks.R",
  "analysis/08_statistical_analysis.R"
)

for (script in analysis_scripts) {
  message("Running ", script)
  source(script, echo = FALSE)
}
