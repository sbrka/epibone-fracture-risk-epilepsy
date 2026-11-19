v2r_validate_static_lock <- function() {
  module <- paste(readLines(file.path(EPIBONE_ROOT, "R", "07_v2r_modules.R"),
                            warn = FALSE), collapse = "\n")
  stage_b <- paste(readLines(file.path(EPIBONE_ROOT, "01_STAGE_B_OUTCOMES.R"),
                             warn = FALSE), collapse = "\n")
  checks <- c(
    exact_review_gate = grepl("AOU_EPIBONE_V2R_REVIEWED", module, fixed = TRUE) &&
      grepl("v2r_run_stage_b", stage_b, fixed = TRUE),
    direct_condition_source = grepl(
      "condition_source_concept_id", module, fixed = TRUE),
    direct_procedure_source = grepl(
      "procedure_source_concept_id", module, fixed = TRUE),
    no_snomed_expansion = !grepl("SNOMED", module, fixed = TRUE) &&
      !grepl("Maps to", module, fixed = TRUE),
    u2_thresholds = grepl("n_gate[\"migraine\"] >= 100", module, fixed = TRUE) &&
      grepl("n_gate[\"epilepsy\"] >= 200", module, fixed = TRUE),
    corrected_u2_loincs = all(vapply(
      c("38263-0", "38267-1", "38264-8", "24701-5", "24966-4"),
      grepl, logical(1), x = module, fixed = TRUE
    )) && !any(vapply(
      c("38263-6", "38267-7", "38269-3"),
      grepl, logical(1), x = module, fixed = TRUE
    )),
    u2_semantic_guard = grepl("SEMANTIC_MISMATCH", module, fixed = TRUE),
    u3_event_gate = grepl("events_broad_fracture >= 21", module, fixed = TRUE),
    u4_absent = !grepl("V2R-U4|event-time rates|injury-trigger", module,
                       ignore.case = TRUE),
    suppression_guard = exists("aou_prepare_explicit_export", mode = "function")
  )
  data.frame(check = names(checks), passed = as.logical(checks), detail = "",
             stringsAsFactors = FALSE)
}

v2r_validate_safe_tree <- function(results_dir = file.path(EPIBONE_ROOT, "results")) {
  if (!dir.exists(results_dir)) return(data.frame(
    check = "results_directory_exists", passed = TRUE,
    detail = "No results yet; source validation only.", stringsAsFactors = FALSE))
  files <- list.files(results_dir, recursive = TRUE, full.names = TRUE,
                      include.dirs = FALSE)
  private <- files[grepl("private_enclave_only", files, fixed = TRUE)]
  unsafe <- files[grepl("[.](rds|rda|rdata|parquet|feather)$", files,
                        ignore.case = TRUE)]
  forbidden <- character(); small <- character()
  for (path in files[grepl("[.]csv$", files, ignore.case = TRUE)]) {
    x <- tryCatch(utils::read.csv(path, na.strings = "SUPPRESSED",
                                  check.names = FALSE), error = function(e) NULL)
    if (is.null(x)) next
    bad <- names(x)[grepl(
      "(^|_)(person|participant|research|sample|visit|encounter)_?id$|(^|_)(date|datetime|timestamp)$|_(date|datetime|timestamp)$",
      tolower(names(x)), perl = TRUE)]
    if (length(bad)) forbidden <- c(forbidden, paste(basename(path), bad, sep = ":"))
    count_cols <- names(x)[grepl(
      "(^n_people$|^women_n$|^n_(migraine|epilepsy)$|^events_|^deaths_|^n_records$|^n_numeric_values$|^n_people_with_numeric_value$)",
      names(x), ignore.case = TRUE, perl = TRUE)]
    for (name in count_cols) {
      value <- suppressWarnings(as.numeric(x[[name]]))
      if (any(is.finite(value) & value >= 1 & value <= 20))
        small <- c(small, paste(basename(path), name, sep = ":"))
    }
  }
  data.frame(
    check = c("no_private_paths_in_results", "no_private_formats_in_results",
              "no_identifier_or_date_columns", "no_unsuppressed_small_cells"),
    passed = c(!length(private), !length(unsafe), !length(forbidden), !length(small)),
    detail = c(paste(private, collapse = ";"), paste(unsafe, collapse = ";"),
               paste(forbidden, collapse = ";"), paste(small, collapse = ";")),
    stringsAsFactors = FALSE
  )
}

v2r_run_validation <- function() {
  report <- rbind(v2r_validate_static_lock(), v2r_validate_safe_tree())
  out <- file.path(EPIBONE_ROOT, "results", "validation", "01_validation_report.csv")
  aou_write_explicit_csv(report, out)
  if (!all(report$passed)) stop("V2R validation failed: ",
                                paste(report$check[!report$passed], collapse = ", "),
                                call. = FALSE)
  invisible(report)
}

v2r_package_safe_results <- function() {
  v2r_run_validation()
  files <- list.files(file.path(EPIBONE_ROOT, "results"), recursive = TRUE,
                      full.names = TRUE, include.dirs = FALSE)
  files <- files[!grepl("private_enclave_only|[.](rds|rda|rdata|parquet|feather)$",
                       files, ignore.case = TRUE)]
  files <- files[basename(files) != "SAFE_EXPORT_MANIFEST.csv"]
  rel <- sub(paste0("^", gsub("([.()+^$|\\\\])", "\\\\\\1", EPIBONE_ROOT), "/?"),
             "", files)
  hashes <- vapply(files, digest::digest, character(1), algo = "sha256", file = TRUE)
  manifest <- data.frame(file = rel, sha256 = unname(hashes), stringsAsFactors = FALSE)
  manifest_path <- file.path(EPIBONE_ROOT, "results", "SAFE_EXPORT_MANIFEST.csv")
  aou_write_explicit_csv(manifest, manifest_path)
  rel <- unique(c(rel, "results/SAFE_EXPORT_MANIFEST.csv"))
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
  archive <- file.path(EPIBONE_ROOT, paste0("EPIBONE_AOU_V2R_SAFE_RESULTS_", stamp, ".zip"))
  old <- setwd(EPIBONE_ROOT); on.exit(setwd(old), add = TRUE)
  utils::zip(archive, rel, flags = "-q")
  checksum <- digest::digest(archive, algo = "sha256", file = TRUE)
  writeLines(paste(checksum, basename(archive)), paste0(archive, ".sha256"))
  message("Safe archive created: ", archive, "\nSHA256: ", checksum)
  invisible(list(archive = archive, sha256 = checksum))
}
