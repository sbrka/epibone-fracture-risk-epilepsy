epibone_find_prior_checkpoint <- function(folder_name, file_name) {
  configured <- Sys.getenv(
    if (grepl("concept", file_name, fixed = TRUE))
      "AOU_EPIBONE_PRIOR_AUDIT_ROOT" else "AOU_EPIBONE_PRIOR_FEASIBILITY_ROOT",
    unset = ""
  )
  candidates <- unique(c(
    if (nzchar(configured)) configured else character(),
    file.path(dirname(EPIBONE_ROOT), folder_name),
    file.path(getwd(), folder_name)
  ))
  paths <- file.path(candidates, "private_enclave_only", file_name)
  paths <- paths[file.exists(paths)]
  if (length(paths) != 1L) {
    stop(
      "Expected exactly one prior enclave checkpoint named ", file_name,
      ". Set the corresponding AOU_EPIBONE_PRIOR_*_ROOT variable. Found: ",
      paste(paths, collapse = "; "), call. = FALSE
    )
  }
  normalizePath(paths, mustWork = TRUE)
}

epibone_num <- function(x) suppressWarnings(as.numeric(x))

epibone_checkpoint_repair <- function() {
  aou_ducc_preflight()
  audit_path <- epibone_find_prior_checkpoint(
    "EPIBONE_AOU_ENCLAVE_20260822", "concept_audit_raw.rds"
  )
  feasibility_path <- epibone_find_prior_checkpoint(
    "EPIBONE_AOU_ENCLAVE_20260822_v2", "feasibility_raw.rds"
  )
  audit <- readRDS(audit_path)
  feasibility <- readRDS(feasibility_path)
  stopifnot(all(c("conditions", "drugs", "routes") %in% names(audit)))
  stopifnot(all(c("v1", "v2", "v3", "decisions", "pair_gates") %in% names(feasibility)))

  outdir <- file.path(EPIBONE_ROOT, "results", "checkpoint_repair")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  condition_vocab <- unique(as.data.frame(audit$conditions)[, c(
    "set_name", "source_code", "source_name", "source_concept_id",
    "standard_concept_id", "standard_name", "n_descendant_concepts"
  )])
  condition_vocab <- condition_vocab[order(
    condition_vocab$set_name, condition_vocab$source_code,
    condition_vocab$standard_concept_id
  ), , drop = FALSE]

  drug_vocab <- unique(as.data.frame(audit$drugs)[, c(
    "abbreviation", "group_name", "role", "ingredient_concept_id",
    "ingredient_concept_name", "n_descendant_concepts"
  )])
  drug_vocab <- drug_vocab[order(drug_vocab$abbreviation), , drop = FALSE]

  route <- as.data.frame(audit$routes)
  route$n_records <- epibone_num(route$n_records)
  route_class <- stats::aggregate(
    n_records ~ abbreviation + group_name + route_class,
    data = route, FUN = sum, na.rm = TRUE
  )
  totals <- stats::aggregate(n_records ~ abbreviation, route_class, sum, na.rm = TRUE)
  names(totals)[2] <- "n_total_records"
  route_class <- merge(route_class, totals, by = "abbreviation", all.x = TRUE)
  route_class$record_percent <- 100 * route_class$n_records / route_class$n_total_records
  unsafe_route_drug <- ave(
    route_class$n_records >= 1 & route_class$n_records <= AOU_MAX_SUPPRESSED_COUNT,
    route_class$abbreviation, FUN = any
  )
  route_class$n_total_records[unsafe_route_drug] <- NA_real_
  route_class$record_percent[unsafe_route_drug] <- NA_real_
  route_class <- route_class[order(route_class$abbreviation, route_class$route_class), ]

  v1 <- as.data.frame(feasibility$v1)
  strict_support <- v1[v1$stage == "07_event_support_two_code_730obs", c(
    "module", "arm", "stage", "n_people", "n_broad_events_1y",
    "n_broad_events_3y", "n_mof_events_3y", "n_osteoporosis_events_3y",
    "n_followup_3y"
  ), drop = FALSE]

  v3 <- as.data.frame(feasibility$v3)
  calendar <- v3[v3$row_type == "calendar", , drop = FALSE]
  calendar$index_year <- epibone_num(calendar$label)
  calendar$band_start <- 5L * floor(calendar$index_year / 5L)
  sum_cols <- c(
    "n_people", "n_two_code_epilepsy", "n_nonacute_index",
    "n_broad_events_1y", "n_broad_events_3y", "n_mof_events_3y",
    "n_osteoporosis_events_3y", "n_followup_3y"
  )
  for (name in sum_cols) calendar[[name]] <- epibone_num(calendar[[name]])
  calendar_bands <- stats::aggregate(
    calendar[, sum_cols, drop = FALSE],
    by = list(group_name = calendar$group_name, band_start = calendar$band_start),
    FUN = sum, na.rm = TRUE
  )
  calendar_bands$band_end <- calendar_bands$band_start + 4L
  calendar_bands <- calendar_bands[, c("group_name", "band_start", "band_end", sum_cols)]
  calendar_bands <- calendar_bands[order(calendar_bands$group_name,
                                         calendar_bands$band_start), ]

  module_gates <- as.data.frame(feasibility$decisions)
  pair_gates <- as.data.frame(feasibility$pair_gates)

  files <- c(
    condition_vocab = file.path(outdir, "01_condition_vocabulary_map.csv"),
    drug_vocab = file.path(outdir, "02_drug_vocabulary_map.csv"),
    route = file.path(outdir, "03_route_class_record_completeness.csv"),
    strict = file.path(outdir, "04_v1_strict_fracture_support.csv"),
    calendar = file.path(outdir, "05_v3_calendar_5year_bands.csv"),
    modules = file.path(outdir, "06_module_gates_readable.csv"),
    pairs = file.path(outdir, "07_pair_gates_readable.csv")
  )
  aou_write_explicit_csv(condition_vocab, files[["condition_vocab"]])
  aou_write_explicit_csv(drug_vocab, files[["drug_vocab"]])
  aou_write_explicit_csv(
    route_class, files[["route"]],
    count_cols = c("n_records", "n_total_records"),
    support_cols = "n_records", dependent_cols = "record_percent"
  )
  aou_write_explicit_csv(
    strict_support, files[["strict"]], count_cols = setdiff(names(strict_support), c(
      "module", "arm", "stage"
    ))
  )
  aou_write_explicit_csv(
    calendar_bands, files[["calendar"]], count_cols = sum_cols
  )
  aou_write_explicit_csv(
    module_gates, files[["modules"]],
    count_cols = c("minimum_people", "minimum_broad_events_3y")
  )
  aou_write_explicit_csv(
    pair_gates, files[["pairs"]],
    count_cols = c("trinetx_matched_per_arm", "n_a", "n_b", "events_3y_a", "events_3y_b")
  )
  epibone_write_manifest("checkpoint_repair", basename(files))
  message("Checkpoint-only audit repair complete; no BigQuery job was submitted.")
  invisible(list(
    condition_vocab = condition_vocab, drug_vocab = drug_vocab,
    route_class = route_class, strict_support = strict_support,
    calendar_bands = calendar_bands, module_gates = module_gates,
    pair_gates = pair_gates
  ))
}
