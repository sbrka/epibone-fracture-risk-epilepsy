epibone_entry_file <- normalizePath(sys.frame(1)$ofile, mustWork = TRUE)
epibone_entry_root <- dirname(epibone_entry_file)
Sys.setenv(AOU_EPIBONE_ROOT = epibone_entry_root)

source(file.path(epibone_entry_root, "R", "00_setup.R"), local = FALSE)
epibone_initialize()
for (file in c("01_definitions.R", "03_build_sql.R", "05_outcomes.R", "06_validate.R")) {
  source(file.path(epibone_entry_root, "R", file), local = FALSE)
}

epibone_model_cohort <- epibone_build_or_resume_cohort()
epibone_run_outcome_stage(epibone_model_cohort)
rm(epibone_model_cohort)
invisible(gc())
epibone_run_validation()

message("\nSTAGE B COMPLETE. Create the disclosure-safe archive with:")
message("source('02_PACKAGE_SAFE_RESULTS.R')")
