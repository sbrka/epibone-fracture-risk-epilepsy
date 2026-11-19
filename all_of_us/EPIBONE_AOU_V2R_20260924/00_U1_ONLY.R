# Fast path for the reviewer-requested U1 age and sex descriptives only.
# Run interactively inside the All of Us Controlled Tier Workbench.

epibone_entry_file <- normalizePath(sys.frame(1)$ofile, mustWork = TRUE)
epibone_entry_root <- dirname(epibone_entry_file)
Sys.setenv(AOU_EPIBONE_ROOT = epibone_entry_root)

source(file.path(epibone_entry_root, "R", "00_setup.R"), local = FALSE)
epibone_initialize()
for (file in c("01_definitions.R", "03_build_sql.R", "04_design.R",
               "05_outcomes.R", "07_v2r_modules.R", "06_validate.R")) {
  source(file.path(epibone_entry_root, "R", file), local = FALSE)
}

artifacts <- v2r_load_v1_artifacts()
u1 <- v2r_u1_descriptives(artifacts$cohort, artifacts$designs)

required_columns <- c(
  "contrast_id", "analysis", "arm", "n_people", "weight_sum",
  "age_mean", "age_sd", "age_median", "age_q1", "age_q3",
  "women_n", "women_weighted_sum", "women_percent"
)
checks <- c(
  eight_locked_rows = nrow(u1) == 8L,
  locked_contrasts = identical(
    sort(unique(u1$contrast_id)),
    sort(c("V1_EPI_VS_MIG_PRIMARY", "V1_EPI_VS_SYN_PRIMARY"))
  ),
  both_estimands = identical(
    sort(unique(u1$analysis)), sort(c("overlap_weighted", "unweighted"))
  ),
  required_columns = all(required_columns %in% names(u1)),
  finite_age_summaries = all(is.finite(unlist(u1[c(
    "age_mean", "age_sd", "age_median", "age_q1", "age_q3"
  )]))),
  valid_women_percent = all(
    is.finite(u1$women_percent) & u1$women_percent >= 0 & u1$women_percent <= 100
  )
)
if (!all(checks)) {
  stop("U1 validation failed: ", paste(names(checks)[!checks], collapse = ", "),
       call. = FALSE)
}

u1_path <- file.path(epibone_entry_root, "results", "u1_descriptives.csv")
aou_write_explicit_csv(
  u1, u1_path,
  count_cols = c("n_people", "women_n"),
  support_cols = "n_people",
  dependent_cols = c(
    "age_mean", "age_sd", "age_median", "age_q1", "age_q3",
    "women_weighted_sum", "women_percent"
  )
)

validation <- data.frame(
  check = names(checks), passed = unname(checks), detail = "",
  stringsAsFactors = FALSE
)
aou_write_explicit_csv(
  validation,
  file.path(epibone_entry_root, "results", "validation", "u1_validation_report.csv")
)
epibone_write_manifest("v2r_u1", c(
  "u1_descriptives.csv", "validation/u1_validation_report.csv"
))

message("\nV2R U1 COMPLETE. Review results/u1_descriptives.csv before export.")
message("After review, source('EPIBONE_AOU_V2R_20260924/02_PACKAGE_SAFE_RESULTS.R').")
