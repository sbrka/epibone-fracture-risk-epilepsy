# Standalone terminology-corrected V2R-U2 counts-only runner.
# Upload this one file to the All of Us RStudio home directory and source it.
# It reuses the existing EPIBONE_AOU_V2R_20260924 package for locked cohort and
# disclosure functions, but overrides U2 terminology and cache identity in memory.
# It does not query U3 events or fit any U2/U3 model.

u2_patch_file <- normalizePath(sys.frame(1)$ofile, mustWork = TRUE)
u2_home <- dirname(u2_patch_file)
u2_root_candidates <- unique(c(
  file.path(u2_home, "EPIBONE_AOU_V2R_20260924"),
  file.path(path.expand("~"), "EPIBONE_AOU_V2R_20260924")
))
u2_root_candidates <- u2_root_candidates[dir.exists(u2_root_candidates)]
if (!length(u2_root_candidates)) {
  stop("Cannot find ~/EPIBONE_AOU_V2R_20260924.", call. = FALSE)
}
u2_root <- normalizePath(u2_root_candidates[1], mustWork = TRUE)
Sys.setenv(AOU_EPIBONE_ROOT = u2_root)

source(file.path(u2_root, "R", "00_setup.R"), local = FALSE)
epibone_initialize()
for (file in c("01_definitions.R", "03_build_sql.R", "04_design.R",
               "05_outcomes.R", "07_v2r_modules.R", "06_validate.R")) {
  source(file.path(u2_root, "R", file), local = FALSE)
}

v2r_candidate_loincs <- function() {
  data.frame(
    loinc_code = c("38263-0", "38267-1", "38264-8", "24701-5", "24966-4"),
    measure = c("femur_t_score", "lumbar_spine_t_score", "hip_t_score",
                "femur_bmd", "lumbar_spine_bmd"),
    class = c(rep("T_SCORE", 3), rep("BMD", 2)),
    stringsAsFactors = FALSE
  )
}

v2r_loinc_audit_sql <- function() {
  cand <- v2r_candidate_loincs()
  structs <- paste(vapply(seq_len(nrow(cand)), function(i) paste0(
    "STRUCT(", epibone_sql_string(cand$loinc_code[i]), " AS loinc_code, ",
    epibone_sql_string(cand$measure[i]), " AS measure, ",
    epibone_sql_string(cand$class[i]), " AS measure_class)"
  ), character(1)), collapse = ",\n    ")
  paste0(
    "WITH candidates AS (SELECT * FROM UNNEST([\n    ", structs, "\n  ])),\n",
    "resolved AS (SELECT q.*, c.concept_id, c.concept_name, c.standard_concept, ",
    "c.invalid_reason FROM candidates q LEFT JOIN ", epibone_table("concept"),
    " c ON c.vocabulary_id='LOINC' AND c.concept_code=q.loinc_code),\n",
    "usage AS (SELECT r.loinc_code, r.concept_id,\n",
    " COUNT(DISTINCT m.measurement_id) AS n_records,\n",
    " COUNT(DISTINCT m.person_id) AS n_people,\n",
    " COUNT(DISTINCT IF(m.value_as_number IS NOT NULL,m.measurement_id,NULL)) AS n_numeric_values\n",
    " FROM resolved r LEFT JOIN ", epibone_table("measurement"), " m\n",
    " ON r.concept_id IN (m.measurement_concept_id,m.measurement_source_concept_id)\n",
    " GROUP BY r.loinc_code,r.concept_id)\n",
    "SELECT r.loinc_code,r.measure,r.measure_class,r.concept_id,r.concept_name,\n",
    " r.standard_concept,r.invalid_reason,COALESCE(u.n_records,0) AS n_records,\n",
    " COALESCE(u.n_people,0) AS n_people,COALESCE(u.n_numeric_values,0) AS n_numeric_values,\n",
    " CASE WHEN r.concept_id IS NULL OR r.invalid_reason IS NOT NULL THEN 'NOT_EXECUTABLE'\n",
    "      WHEN r.measure_class='T_SCORE' AND NOT REGEXP_CONTAINS(LOWER(r.concept_name), r't-score') THEN 'SEMANTIC_MISMATCH'\n",
    "      WHEN r.measure_class='BMD' AND NOT REGEXP_CONTAINS(LOWER(r.concept_name), r'mass/area') THEN 'SEMANTIC_MISMATCH'\n",
    "      WHEN COALESCE(u.n_numeric_values,0)=0 THEN 'NO_NUMERIC_VALUES'\n",
    "      ELSE 'EXECUTABLE_VERIFIED' END AS disposition\n",
    "FROM resolved r LEFT JOIN usage u USING(loinc_code,concept_id) ORDER BY r.loinc_code,r.concept_id"
  )
}

v2r_fetch_corrected_u2_measurements <- function(anchors) {
  signature <- digest::digest(list(
    cdr = Sys.getenv("WORKSPACE_CDR"),
    person = sort(as.character(anchors$person_id)),
    index = sort(as.character(anchors$index_date)),
    loinc_codes = sort(v2r_candidate_loincs()$loinc_code),
    patch = "U2_LOINC_AMEND_20260925"
  ), algo = "sha256")
  path <- file.path(
    u2_root, "private_enclave_only",
    paste0("v2r_u2_corrected_measurements_", signature, ".rds")
  )
  if (file.exists(path)) {
    message("Reusing corrected-code enclave-only U2 measurement checkpoint.")
    return(readRDS(path))
  }
  measurements <- v2r_query_chunks(anchors$person_id, v2r_measurement_sql)
  if (nrow(measurements)) {
    measurements$measurement_date <- as.Date(measurements$measurement_date)
  }
  out <- list(measurements = measurements, signature = signature)
  aou_write_private_rds(out, path)
  out
}

v2r_u2_feasibility <- function(anchors, enrichment) {
  d <- v2r_measurement_frame(anchors, enrichment)
  if (nrow(d)) {
    usable <- d[!is.na(d$period) & is.finite(d$value_as_number), , drop = FALSE]
  } else {
    usable <- data.frame(
      person_id = character(), loinc_code = character(), period = character(),
      analysis_arm = character(), class = character(), value_as_number = numeric(),
      stringsAsFactors = FALSE
    )
  }

  grid <- expand.grid(
    loinc_code = v2r_candidate_loincs()$loinc_code,
    period = c("baseline_-1095_-1", "followup_1_1095"),
    arm = c("migraine", "epilepsy"), stringsAsFactors = FALSE
  )
  if (nrow(usable)) {
    count <- stats::aggregate(
      usable$person_id,
      by = list(loinc_code = usable$loinc_code, period = usable$period,
                arm = usable$analysis_arm),
      FUN = function(x) length(unique(x))
    )
    names(count)[4] <- "n_people_with_numeric_value"
    grid <- merge(grid, count, all.x = TRUE,
                  by = c("loinc_code", "period", "arm"), sort = FALSE)
  } else {
    grid$n_people_with_numeric_value <- NA_real_
  }
  grid$n_people_with_numeric_value[is.na(grid$n_people_with_numeric_value)] <- 0
  grid <- merge(grid, v2r_candidate_loincs(), by = "loinc_code", all.x = TRUE)

  if (nrow(usable)) {
    combined <- stats::aggregate(
      usable$person_id,
      by = list(class = usable$class, period = usable$period,
                arm = usable$analysis_arm),
      FUN = function(x) length(unique(x))
    )
    names(combined)[4] <- "n_people_with_numeric_value"
    combined$loinc_code <- paste0("ANY_", combined$class)
    combined$measure <- ifelse(combined$class == "T_SCORE",
                               "any_candidate_t_score", "any_candidate_bmd")
    combined <- combined[, names(grid), drop = FALSE]
    grid <- rbind(grid, combined)
  }

  baseline <- usable[
    usable$period == "baseline_-1095_-1" & usable$class == "T_SCORE" &
      usable$value_as_number >= -10 & usable$value_as_number <= 10,
    , drop = FALSE
  ]
  n_gate <- vapply(c("migraine", "epilepsy"), function(a) {
    length(unique(baseline$person_id[baseline$analysis_arm == a]))
  }, numeric(1))
  gate <- data.frame(
    module = "U2_BASELINE_T_SCORE", arm = names(n_gate),
    n_people = unname(n_gate), threshold = c(100L, 200L),
    gate = if (n_gate["migraine"] >= 100 && n_gate["epilepsy"] >= 200) {
      "PROCEED"
    } else {
      "COUNTS_ONLY_GATE_FAILED"
    },
    stringsAsFactors = FALSE
  )
  list(availability = grid, gate = gate, baseline = baseline)
}

u2_artifacts <- v2r_load_v1_artifacts()
u2_audit <- as.data.frame(epibone_bq(v2r_loinc_audit_sql()))
u2_anchors <- v2r_primary_anchors(u2_artifacts$cohort, u2_artifacts$designs)
u2_enrichment <- v2r_fetch_corrected_u2_measurements(u2_anchors)
u2_result <- v2r_u2_feasibility(u2_anchors, u2_enrichment)

u2_expected_loincs <- v2r_candidate_loincs()$loinc_code
u2_acceptable_dispositions <- c("EXECUTABLE_VERIFIED", "NO_NUMERIC_VALUES")
u2_expected_thresholds <- c(migraine = 100, epilepsy = 200)
u2_observed_thresholds <- setNames(u2_result$gate$threshold, u2_result$gate$arm)
u2_observed_counts <- setNames(u2_result$gate$n_people, u2_result$gate$arm)
u2_gate_should_proceed <- setequal(names(u2_observed_counts),
                                   names(u2_expected_thresholds)) &&
  all(u2_observed_counts[names(u2_expected_thresholds)] >= u2_expected_thresholds)
u2_checks <- c(
  corrected_candidate_loincs_present =
    setequal(unique(u2_audit$loinc_code), u2_expected_loincs),
  all_corrected_loincs_resolve_semantically = nrow(u2_audit) == 5L &&
    all(!is.na(u2_audit$disposition) &
          u2_audit$disposition %in% u2_acceptable_dispositions),
  no_semantic_mismatch = !any(u2_audit$disposition == "SEMANTIC_MISMATCH"),
  availability_columns = all(c(
    "loinc_code", "period", "arm", "n_people_with_numeric_value",
    "measure", "class"
  ) %in% names(u2_result$availability)),
  nonnegative_availability_counts = all(
    is.finite(u2_result$availability$n_people_with_numeric_value) &
      u2_result$availability$n_people_with_numeric_value >= 0
  ),
  locked_gate_arms = setequal(u2_result$gate$arm,
                              names(u2_expected_thresholds)),
  locked_gate_thresholds = identical(
    as.numeric(u2_observed_thresholds[names(u2_expected_thresholds)]),
    as.numeric(u2_expected_thresholds)
  ),
  gate_matches_counts = all(u2_result$gate$gate == if (u2_gate_should_proceed) {
    "PROCEED"
  } else {
    "COUNTS_ONLY_GATE_FAILED"
  }),
  u3_not_queried = TRUE
)
if (!all(u2_checks)) {
  stop("Corrected U2 validation failed: ",
       paste(names(u2_checks)[!u2_checks], collapse = ", "), call. = FALSE)
}

aou_write_explicit_csv(
  u2_audit,
  file.path(u2_root, "results", "feasibility", "u2_loinc_vocabulary_audit.csv"),
  count_cols = c("n_records", "n_people", "n_numeric_values")
)
aou_write_explicit_csv(
  u2_result$availability,
  file.path(u2_root, "results", "feasibility", "u2_measurement_counts.csv"),
  count_cols = "n_people_with_numeric_value"
)
aou_write_explicit_csv(
  u2_result$gate,
  file.path(u2_root, "results", "feasibility", "u2_model_gate.csv"),
  count_cols = "n_people"
)
u2_validation <- data.frame(
  check = names(u2_checks), passed = unname(u2_checks), detail = "",
  stringsAsFactors = FALSE
)
aou_write_explicit_csv(
  u2_validation,
  file.path(u2_root, "results", "validation", "u2_validation_report.csv")
)
u2_outputs <- c(
  "feasibility/u2_loinc_vocabulary_audit.csv",
  "feasibility/u2_measurement_counts.csv",
  "feasibility/u2_model_gate.csv",
  "validation/u2_validation_report.csv"
)
u2_manifest <- data.frame(
  stage = "v2r_u2_corrected",
  run_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  cdr = Sys.getenv("WORKSPACE_CDR"),
  billing_project = Sys.getenv("GOOGLE_PROJECT"),
  code_version = "EPIBONE_AOU_V2R_U2_LOINC_AMEND_20260925",
  output = u2_outputs,
  stringsAsFactors = FALSE
)
aou_write_explicit_csv(
  u2_manifest,
  file.path(u2_root, "results", "v2r_u2", "manifest_v2r_u2.csv")
)

v2r_package_corrected_u2_results <- function() {
  base_validation <- v2r_validate_static_lock()
  corrected_validation <- data.frame(
    check = c("corrected_u2_loincs", "u2_semantic_guard",
              "corrected_u2_validation_passed", "u3_absent_from_results"),
    passed = c(
      TRUE,
      TRUE,
      all(u2_checks),
      !any(grepl("u3", list.files(file.path(u2_root, "results"), recursive = TRUE),
                 ignore.case = TRUE))
    ),
    detail = "",
    stringsAsFactors = FALSE
  )
  safe_tree <- v2r_validate_safe_tree(file.path(u2_root, "results"))
  report <- rbind(base_validation, corrected_validation, safe_tree)
  aou_write_explicit_csv(
    report,
    file.path(u2_root, "results", "validation", "01_validation_report.csv")
  )
  if (!all(report$passed)) {
    stop("Corrected safe-package validation failed: ",
         paste(report$check[!report$passed], collapse = ", "), call. = FALSE)
  }

  files <- list.files(file.path(u2_root, "results"), recursive = TRUE,
                      full.names = TRUE, include.dirs = FALSE)
  files <- files[!grepl(
    "private_enclave_only|[.](rds|rda|rdata|parquet|feather)$",
    files, ignore.case = TRUE
  )]
  files <- files[basename(files) != "SAFE_EXPORT_MANIFEST.csv"]
  rel <- sub(paste0("^", gsub("([.()+^$|\\\\])", "\\\\\\1", u2_root), "/?"),
             "", files)
  hashes <- vapply(files, digest::digest, character(1), algo = "sha256", file = TRUE)
  safe_manifest <- data.frame(
    file = rel, sha256 = unname(hashes), stringsAsFactors = FALSE
  )
  safe_manifest_path <- file.path(u2_root, "results", "SAFE_EXPORT_MANIFEST.csv")
  aou_write_explicit_csv(safe_manifest, safe_manifest_path)
  rel <- unique(c(rel, "results/SAFE_EXPORT_MANIFEST.csv"))

  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
  archive <- file.path(
    u2_root,
    paste0("EPIBONE_AOU_V2R_CORRECTED_SAFE_RESULTS_", stamp, ".zip")
  )
  old <- setwd(u2_root); on.exit(setwd(old), add = TRUE)
  utils::zip(archive, rel, flags = "-q")
  checksum <- digest::digest(archive, algo = "sha256", file = TRUE)
  writeLines(paste(checksum, basename(archive)), paste0(archive, ".sha256"))
  message("Corrected safe archive created: ", archive, "\nSHA256: ", checksum)
  invisible(list(archive = archive, sha256 = checksum))
}

message("\nCORRECTED V2R U2 COUNTS-ONLY CHECK COMPLETE.")
message("Review the three results/feasibility/u2_*.csv files.")
message("This standalone run did not query U3 or fit a model.")
message("After review, run: v2r_package_corrected_u2_results()")
