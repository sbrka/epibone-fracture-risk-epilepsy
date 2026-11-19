# EPI-BONE Hotfix 07: cached design-only route adjustment and matching repair.
#
# This patch makes two pre-outcome, outcome-independent corrections:
#   1) the broad medication-epilepsy ascertainment route is included in V3/V4
#      propensity models; and
#   2) MatchIt rows are recovered from the input-order weight vector rather than
#      inherited data-frame row names.
#
# The frozen Patch05 cohort is read directly from its enclave-only checkpoint.
# This entry point cannot submit a BigQuery query, does not rebuild any cohort,
# and does not fit an outcome model. It reruns Stage A diagnostics, requires a
# balanced V1 matching sensitivity, validates the safe tree and creates a new
# immutable disclosure-safe review archive.

epibone_entry_file <- normalizePath(sys.frame(1)$ofile, mustWork = TRUE)
epibone_entry_root <- dirname(epibone_entry_file)
Sys.setenv(AOU_EPIBONE_ROOT = epibone_entry_root)

source(file.path(epibone_entry_root, "R", "00_setup.R"), local = FALSE)
epibone_initialize()
for (file in c("01_definitions.R", "03_build_sql.R", "04_design.R", "06_validate.R")) {
  source(file.path(epibone_entry_root, "R", file), local = FALSE)
}

design_text <- paste(
  readLines(file.path(epibone_entry_root, "R", "04_design.R"), warn = FALSE),
  collapse = "\n"
)
stopifnot(
  grepl(
    "medication_epilepsy_route_r56_supported", design_text, fixed = TRUE
  ),
  grepl("index <- unname(which(fit$weights > 0))", design_text, fixed = TRUE),
  !grepl("as.integer(rownames(md))", design_text, fixed = TRUE)
)
rm(design_text)

cohort_sql <- epibone_model_cohort_staged_sql()
cohort_signature <- digest::digest(list(
  sql = cohort_sql,
  cdr = Sys.getenv("WORKSPACE_CDR"),
  phenotype = paste0(
    "direct_icd_source_concept_id_no_snomed_expansion_v1;",
    "medication_exploratory_g40_once_or_r56_then_new_asm_0_180d_v1"
  )
), algo = "sha256")
cohort_cache <- file.path(
  epibone_entry_root, "private_enclave_only",
  paste0("epibone_model_cohort_", cohort_signature, ".rds")
)
rm(cohort_sql, cohort_signature)

if (!file.exists(cohort_cache)) {
  stop(
    paste0(
      "The exact frozen Patch05 cohort checkpoint is missing. Hotfix 07 is ",
      "query-free by design and will not rebuild it. Restore the existing ",
      "private_enclave_only checkpoint before continuing."
    ),
    call. = FALSE
  )
}

message("Hotfix 07 verified: using the exact frozen Patch05 cohort checkpoint.")
message("No BigQuery query will be submitted and no outcome model will be fitted.")
epibone_model_cohort <- readRDS(cohort_cache)
if (!"medication_epilepsy_route" %in% names(epibone_model_cohort)) {
  stop("Frozen cohort lacks the Patch05 medication epilepsy route.", call. = FALSE)
}

stage_a <- epibone_run_design_stage(epibone_model_cohort)
rm(epibone_model_cohort)
invisible(gc())

if (any(stage_a$gates$gate == "STOP_DESIGN_ERROR")) {
  stop("Patch07 produced a Stage A design error; do not package.", call. = FALSE)
}
rm(stage_a)
invisible(gc())
epibone_run_validation()
archive <- epibone_package_safe_results()

message("\nHOTFIX 07 COMPLETE. No query or outcome model was run.")
message("Download only the safe archive: ", archive$archive)
message("SHA256: ", archive$sha256)
