# EPI-BONE Hotfix 05: exploratory broad medication phenotype and Stage A repair.
#
# Medication modules V3/V4 use a pre-outcome, direct-source phenotype:
#   1) >=1 G40 / ICD-9-CM 345 diagnosis on or before the new-ASM index; OR
#   2) R56 / ICD-9-CM 780.3 followed by the index epilepsy-specific ASM
#      on day 0-180.
# The R56 route is forward-only. The index remains the first recorded oral ASM,
# exactly one ingredient is required on the index day, and the existing washout,
# observation, alternative-indication, fracture-free landmark and outcome rules
# remain unchanged. V3/V4 are explicitly exploratory.
#
# This fresh-session entry point also repairs the Stage A ingredient-composition
# aggregation and logit-PS matching sensitivity. It performs one necessary
# staged cohort rebuild because V3/V4 eligibility changes. No outcome model is
# fitted; Stage B remains owner-gated.

epibone_entry_file <- normalizePath(sys.frame(1)$ofile, mustWork = TRUE)
epibone_entry_root <- dirname(epibone_entry_file)
Sys.setenv(AOU_EPIBONE_ROOT = epibone_entry_root)

source(file.path(epibone_entry_root, "R", "00_setup.R"), local = FALSE)
epibone_initialize()
for (file in c("01_definitions.R", "03_build_sql.R", "04_design.R", "06_validate.R")) {
  source(file.path(epibone_entry_root, "R", file), local = FALSE)
}

condition_ctes <- epibone_condition_map_ctes(
  requests = epibone_model_condition_requests(), prefix = "cond"
)
cohort_sql <- epibone_model_cohort_sql()
design_text <- paste(
  readLines(file.path(epibone_entry_root, "R", "04_design.R"), warn = FALSE),
  collapse = "\n"
)
plan <- epibone_contrast_plan()
stopifnot(
  grepl("ICD10CM", condition_ctes, fixed = TRUE),
  grepl("ICD9CM", condition_ctes, fixed = TRUE),
  !grepl("concept_ancestor", condition_ctes, fixed = TRUE),
  grepl("m.source_concept_id=co.condition_source_concept_id", cohort_sql, fixed = TRUE),
  !grepl("m.concept_id=co.condition_concept_id", cohort_sql, fixed = TRUE),
  grepl(
    "ce.set_name='convulsion' AND ce.event_date BETWEEN DATE_SUB(x.index_date,INTERVAL 180 DAY) AND x.index_date",
    cohort_sql, fixed = TRUE
  ),
  grepl("c.n_epi_dates>=1 OR c.n_r56_dates_180>=1", cohort_sql, fixed = TRUE),
  grepl("exploratory_broad30", cohort_sql, fixed = TRUE),
  plan$role[plan$contrast_id == "V3_EIA_VS_SELECTED_NEIA"] ==
    "exploratory_medication",
  grepl(
    "population = rep(\"calendar_eligible_preweighting\", nrow(d))",
    design_text, fixed = TRUE
  ),
  grepl("exposure ~ logit_ps", design_text, fixed = TRUE),
  grepl("matching_status = \"MATCHED\"", design_text, fixed = TRUE)
)
rm(condition_ctes, cohort_sql, design_text, plan)

message(paste0(
  "Hotfix 05 verified: V3/V4 are exploratory and use direct G40/ICD9-345 once, ",
  "or forward R56/ICD9-780.3 -> new epilepsy-specific ASM on day 0-180."
))
message(paste0(
  "Running the vocabulary/source-pattern audit, one necessary staged cohort rebuild, ",
  "and Stage A diagnostics. No outcome model will be fitted."
))

epibone_run_model_vocabulary_audit()
epibone_model_cohort <- epibone_build_or_resume_cohort()
epibone_run_design_stage(epibone_model_cohort)
rm(epibone_model_cohort)
invisible(gc())
epibone_run_validation()

message("\nHOTFIX 05 STAGE A COMPLETE. Review results/design/; do not run Stage B yet.")
message("Create a disclosure-safe review ZIP with: source('02_PACKAGE_SAFE_RESULTS.R')")
