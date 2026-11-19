epibone_validate_static_lock <- function() {
  files <- c(
    file.path(EPIBONE_ROOT, "00_aou_ducc_guard.R"),
    file.path(EPIBONE_ROOT, "R", c(
      "01_definitions.R", "03_build_sql.R", "04_design.R", "05_outcomes.R"
    ))
  )
  stopifnot(all(file.exists(files)))
  text <- setNames(lapply(files, function(path) paste(readLines(path, warn = FALSE),
                                                    collapse = "\n")), basename(files))
  cohort_sql <- epibone_model_cohort_sql()
  condition_ctes <- epibone_condition_map_ctes(
    requests = epibone_model_condition_requests(), prefix = "cond"
  )
  checks <- c(
    explicit_export_guard = grepl("aou_prepare_explicit_export", text[[1]], fixed = TRUE),
    private_cache_only = grepl("private_enclave_only", text[[3]], fixed = TRUE),
    single_cohort_builder = lengths(regmatches(text[[3]], gregexpr(
      "Submitting the single minimum-necessary", text[[3]], fixed = TRUE))) == 1L,
    direct_condition_source_join = grepl(
      "m.source_concept_id=co.condition_source_concept_id", cohort_sql, fixed = TRUE
    ),
    no_standard_condition_occurrence_join = !grepl(
      "m.concept_id=co.condition_concept_id", cohort_sql, fixed = TRUE
    ),
    no_snomed_condition_expansion = !grepl(
      "concept_ancestor", condition_ctes, fixed = TRUE
    ),
    icd9_and_icd10_source_registries = all(
      c("ICD9CM", "ICD10CM") %in% epibone_model_condition_requests()$vocabulary_id
    ),
    medication_broad_direct_or_supported_r56 = all(c(
      grepl("c.n_epi_dates>=1 OR c.n_r56_dates_180>=1", cohort_sql, fixed = TRUE),
      grepl(
        "ce.set_name='convulsion' AND ce.event_date BETWEEN DATE_SUB(x.index_date,INTERVAL 180 DAY) AND x.index_date",
        cohort_sql, fixed = TRUE
      ),
      grepl("exploratory_broad30", cohort_sql, fixed = TRUE)
    )),
    calendar_count_only = grepl("epibone_calendar_window", text[[4]], fixed = TRUE),
    matching_uses_logit_ps_caliper = all(c(
      grepl("exposure ~ logit_ps", text[[4]], fixed = TRUE),
      grepl("caliper = 0.2", text[[4]], fixed = TRUE)
    )),
    matching_indices_from_fit_weights = grepl(
      "index <- unname(which(fit$weights > 0))", text[[4]], fixed = TRUE
    ),
    medication_route_in_propensity_model = grepl(
      "medication_epilepsy_route_r56_supported", text[[4]], fixed = TRUE
    ),
    outcome_review_gate = grepl("AOU_EPIBONE_DESIGN_REVIEWED", text[[5]], fixed = TRUE),
    event_gate = grepl("all(support3$events > 20)", text[[5]], fixed = TRUE),
    death_gate = grepl("all(support3$deaths > 20)", text[[5]], fixed = TRUE),
    hard_stop_lcm_lev = "LCM_VS_LEV" %in% epibone_hard_stops(),
    no_lcm_lev_contrast = !"LCM_LEV" %in% epibone_contrast_plan()$contrast_id
  )
  data.frame(check = names(checks), passed = as.logical(checks),
             stringsAsFactors = FALSE)
}

epibone_validate_stage_a_audits <- function() {
  design_dir <- file.path(EPIBONE_ROOT, "results", "design")
  vocabulary_dir <- file.path(EPIBONE_ROOT, "results", "model_vocabulary")
  gate_path <- file.path(design_dir, "01_design_gates.csv")
  balance_path <- file.path(design_dir, "02_weighted_balance.csv")
  matching_path <- file.path(design_dir, "04_matching_sensitivity_design.csv")
  route_path <- file.path(design_dir, "08_medication_epilepsy_phenotype_routes.csv")
  coverage_path <- file.path(
    vocabulary_dir, "05_condition_source_pattern_coverage.csv"
  )
  exception_path <- file.path(vocabulary_dir, "02_mapping_exceptions.csv")

  gates <- if (file.exists(gate_path)) utils::read.csv(
    gate_path, check.names = FALSE, na.strings = "SUPPRESSED"
  ) else NULL
  balance <- if (file.exists(balance_path)) utils::read.csv(
    balance_path, check.names = FALSE, na.strings = "SUPPRESSED"
  ) else NULL
  matching <- if (file.exists(matching_path)) utils::read.csv(
    matching_path, check.names = FALSE, na.strings = "SUPPRESSED"
  ) else NULL
  coverage <- if (file.exists(coverage_path)) utils::read.csv(
    coverage_path, check.names = FALSE, na.strings = "SUPPRESSED"
  ) else NULL
  exceptions <- if (file.exists(exception_path)) utils::read.csv(
    exception_path, check.names = FALSE, na.strings = "SUPPRESSED"
  ) else NULL

  expected_matching <- if (!is.null(gates)) gates$contrast_id[
    gates$role == "co_primary" & gates$gate == "PROCEED"
  ] else character()
  matching_balanced <- FALSE
  matching_detail <- "matching file or status columns missing"
  if (!is.null(matching) &&
      all(c("contrast_id", "matching_status", "matching_note", "maximum_asmd") %in%
          names(matching))) {
    rows <- match(expected_matching, matching$contrast_id)
    present <- !anyNA(rows)
    statuses <- if (present) as.character(matching$matching_status[rows]) else character()
    maximum_asmd <- if (present) suppressWarnings(as.numeric(
      matching$maximum_asmd[rows]
    )) else numeric()
    matching_balanced <- length(expected_matching) > 0 && present &&
      all(statuses == "MATCHED_BALANCED") && all(is.finite(maximum_asmd)) &&
      all(maximum_asmd <= 0.10)
    matching_detail <- paste0(
      "expected=", paste(expected_matching, collapse = "|"),
      "; status=", paste(statuses, collapse = "|"),
      "; maximum_asmd=", paste(signif(maximum_asmd, 4), collapse = "|")
    )
  }
  medication_ids <- epibone_contrast_plan()$contrast_id[
    epibone_contrast_plan()$module %in% c("V3", "V4")
  ]
  proceeding_medication <- if (!is.null(gates)) gates$contrast_id[
    gates$contrast_id %in% medication_ids & gates$gate == "PROCEED"
  ] else character()
  route_covariate <- "medication_epilepsy_route_r56_supported"
  route_adjusted <- !is.null(balance) &&
    all(c("contrast_id", "covariate") %in% names(balance)) &&
    length(proceeding_medication) > 0 && all(
      proceeding_medication %in% balance$contrast_id[balance$covariate == route_covariate]
    )
  allowed_coverage <- c(
    "EXECUTABLE_DIRECT_SOURCE",
    "KNOWN_CDR_GAP_UNSPECIFIED_STROKE_UNDERASCERTAINED",
    "KNOWN_CDR_GAP_NO_SOURCE_CONCEPT"
  )
  i64_documented <- !is.null(exceptions) && all(
    c("domain", "set_name", "requested_value") %in% names(exceptions)
  ) && any(
    exceptions$domain == "condition" & exceptions$set_name == "stroke_prior" &
      exceptions$requested_value == "I64%"
  )

  data.frame(
    check = c(
      "condition_source_pattern_coverage_audited",
      "i64_registry_gap_documented",
      "medication_epilepsy_route_counts_created",
      "proceeding_medication_route_adjusted",
      "proceeding_primary_matching_balanced"
    ),
    passed = c(
      !is.null(coverage) && nrow(coverage) > 0 &&
        all(coverage$status %in% allowed_coverage),
      i64_documented,
      file.exists(route_path),
      route_adjusted,
      matching_balanced
    ),
    detail = c(
      if (is.null(coverage)) "coverage file missing" else paste(
        "rows=", nrow(coverage), sep = ""
      ),
      if (i64_documented) "" else "I64 omission is not documented",
      if (file.exists(route_path)) "" else "route-count file missing",
      paste0(
        "expected=", paste(proceeding_medication, collapse = "|"),
        "; covariate=", route_covariate
      ),
      matching_detail
    ),
    stringsAsFactors = FALSE
  )
}

epibone_validate_safe_tree <- function(results_dir = file.path(EPIBONE_ROOT, "results")) {
  if (!dir.exists(results_dir)) {
    return(data.frame(check = "results_directory_exists", passed = FALSE,
                      detail = results_dir, stringsAsFactors = FALSE))
  }
  all_files <- list.files(results_dir, recursive = TRUE, full.names = TRUE,
                          include.dirs = FALSE)
  unsafe_extensions <- all_files[grepl("\\.(rds|rda|rdata|parquet|feather)$", all_files,
                                      ignore.case = TRUE)]
  private_paths <- all_files[grepl("private_enclave_only", all_files, fixed = TRUE)]
  csvs <- all_files[grepl("\\.csv$", all_files, ignore.case = TRUE)]
  identifier_columns <- character()
  small_cells <- character()
  for (path in csvs) {
    x <- tryCatch(utils::read.csv(path, check.names = FALSE,
                                  na.strings = "SUPPRESSED"), error = function(e) NULL)
    if (is.null(x)) next
    forbidden <- names(x)[grepl(
      "(^|_)(person|participant|research|sample|visit|encounter)_?id$|_(date|datetime|timestamp)$",
      tolower(names(x)), perl = TRUE
    )]
    if (length(forbidden)) identifier_columns <- c(
      identifier_columns, paste(basename(path), forbidden, sep = ":")
    )
    count_cols <- names(x)[grepl(
      paste0(
        "^(n_people|n_records|.*events.*|.*deaths.*|n_(comparator|exposure|0|1)|",
        "n_.*_(comparator|exposure)|raw_positive_n_(comparator|exposure)|",
        ".*people.*|.*ties_removed)$"
      ),
      names(x), ignore.case = TRUE, perl = TRUE
    )]
    for (name in count_cols) {
      value <- suppressWarnings(as.numeric(x[[name]]))
      if (any(is.finite(value) & value >= 1 & value <= 20)) {
        small_cells <- c(small_cells, paste(basename(path), name, sep = ":"))
      }
    }
  }
  data.frame(
    check = c("no_private_formats_in_results", "no_private_paths_in_results",
              "no_identifier_or_date_columns", "no_unsuppressed_small_participant_cells"),
    passed = c(!length(unsafe_extensions), !length(private_paths),
               !length(identifier_columns), !length(small_cells)),
    detail = c(paste(basename(unsafe_extensions), collapse = ";"),
               paste(private_paths, collapse = ";"),
               paste(identifier_columns, collapse = ";"),
               paste(small_cells, collapse = ";")),
    stringsAsFactors = FALSE
  )
}

epibone_run_validation <- function() {
  static <- epibone_validate_static_lock()
  static$detail <- ""
  stage_a <- epibone_validate_stage_a_audits()
  safe <- epibone_validate_safe_tree()
  report <- rbind(static[, c("check", "passed", "detail")], stage_a, safe)
  outdir <- file.path(EPIBONE_ROOT, "results", "validation")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  aou_write_explicit_csv(report, file.path(outdir, "01_validation_report.csv"))
  epibone_write_manifest("validation", "01_validation_report.csv")
  if (!all(report$passed)) {
    stop("Validation failed: ", paste(report$check[!report$passed], collapse = ", "),
         call. = FALSE)
  }
  message("Static lock and safe-results validation passed.")
  invisible(report)
}

epibone_package_safe_results <- function() {
  epibone_run_validation()
  files <- list.files(file.path(EPIBONE_ROOT, "results"), recursive = TRUE,
                      full.names = TRUE, include.dirs = FALSE)
  files <- files[!grepl("private_enclave_only|\\.(rds|rda|rdata|parquet|feather)$",
                       files, ignore.case = TRUE)]
  files <- files[basename(files) != "SAFE_EXPORT_MANIFEST.csv"]
  legacy_standard_map <- file.path(
    EPIBONE_ROOT, "results", "checkpoint_repair", "01_condition_vocabulary_map.csv"
  )
  files <- setdiff(files, legacy_standard_map)
  rel <- sub(paste0("^", gsub("([.()+^$|\\\\])", "\\\\\\1", EPIBONE_ROOT), "/?"),
             "", files)
  hashes <- vapply(files, digest::digest, character(1), algo = "sha256", file = TRUE)
  manifest <- data.frame(file = rel, sha256 = unname(hashes), stringsAsFactors = FALSE)
  manifest_path <- file.path(EPIBONE_ROOT, "results", "SAFE_EXPORT_MANIFEST.csv")
  aou_write_explicit_csv(manifest, manifest_path)
  files <- unique(c(files, manifest_path))
  rel <- sub(paste0("^", gsub("([.()+^$|\\\\])", "\\\\\\1", EPIBONE_ROOT), "/?"),
             "", files)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
  archive <- file.path(EPIBONE_ROOT, paste0("EPIBONE_AOU_SAFE_RESULTS_", stamp, ".zip"))
  if (file.exists(archive)) stop("Refusing to overwrite archive: ", archive, call. = FALSE)
  old <- setwd(EPIBONE_ROOT); on.exit(setwd(old), add = TRUE)
  utils::zip(zipfile = archive, files = rel, flags = "-q")
  checksum <- digest::digest(archive, algo = "sha256", file = TRUE)
  writeLines(paste(checksum, basename(archive)), paste0(archive, ".sha256"))
  message("Safe archive created: ", archive)
  message("SHA256: ", checksum)
  invisible(list(archive = archive, sha256 = checksum))
}
