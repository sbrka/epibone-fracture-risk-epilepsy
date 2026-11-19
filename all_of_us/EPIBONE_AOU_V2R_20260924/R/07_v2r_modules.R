# EPI-BONE reviewer-response modules U1-U3.
# Participant identifiers, dates, measurement values, weights and model objects
# remain in private_enclave_only/. Only disclosure-checked aggregates are written
# under results/.

v2r_candidate_loincs <- function() {
  data.frame(
    loinc_code = c("38263-0", "38267-1", "38264-8", "24701-5", "24966-4"),
    measure = c("femur_t_score", "lumbar_spine_t_score",
                "hip_t_score", "femur_bmd", "lumbar_spine_bmd"),
    class = c(rep("T_SCORE", 3), rep("BMD", 2)),
    stringsAsFactors = FALSE
  )
}

v2r_v1_root <- function() {
  configured <- Sys.getenv("AOU_EPIBONE_V1_ROOT", unset = "")
  path <- if (nzchar(configured)) configured else
    file.path(dirname(EPIBONE_ROOT), "EPIBONE_AOU_MODELLING_20260823_v1")
  normalizePath(path, mustWork = TRUE)
}

v2r_latest_file <- function(path, pattern) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (!length(files)) return(NA_character_)
  files[order(file.info(files)$mtime, decreasing = TRUE)][1]
}

v2r_load_v1_artifacts <- function() {
  root <- v2r_v1_root()
  private <- file.path(root, "private_enclave_only")
  cohort_path <- v2r_latest_file(private, "^epibone_model_cohort_.*[.]rds$")
  design_path <- file.path(private, "accepted_designs.rds")
  if (is.na(cohort_path)) {
    message("No sibling V1 cohort checkpoint found; building one minimum-necessary cohort cache.")
    cohort <- epibone_build_or_resume_cohort()
  } else {
    message("Reusing V1 enclave-only cohort checkpoint: ", basename(cohort_path))
    cohort <- readRDS(cohort_path)
  }
  if (!file.exists(design_path)) {
    own_design <- file.path(EPIBONE_ROOT, "private_enclave_only", "accepted_designs.rds")
    if (!file.exists(own_design)) epibone_run_design_stage(cohort)
    design_path <- own_design
  }
  designs <- readRDS(design_path)
  needed <- c("V1_EPI_VS_MIG_PRIMARY", "V1_EPI_VS_SYN_PRIMARY")
  missing <- needed[vapply(needed, function(x) is.null(designs[[x]]), logical(1))]
  if (length(missing)) stop("Required accepted V1 design is absent: ",
                            paste(missing, collapse = ", "), call. = FALSE)
  list(cohort = cohort, designs = designs, v1_root = root,
       cohort_path = cohort_path, design_path = design_path)
}

v2r_design_keys <- function(designs, contrast_id) {
  entry <- designs[[contrast_id]]
  keys <- if (is.list(entry) && !is.data.frame(entry)) entry$overlap else entry
  if (is.null(keys) || !nrow(keys)) stop("No overlap design keys for ", contrast_id,
                                        call. = FALSE)
  keys
}

v2r_weighted_quantile <- function(x, w, probs = c(.25, .5, .75)) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cdf <- (cumsum(w) - .5 * w) / sum(w)
  stats::approx(c(0, cdf, 1), c(x[1], x, x[length(x)]),
                xout = probs, ties = "ordered", rule = 2)$y
}

v2r_weighted_sd <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  if (length(x) < 2 || sum(w) <= 0) return(NA_real_)
  mu <- sum(w * x) / sum(w)
  sqrt(sum(w * (x - mu)^2) / sum(w))
}

v2r_u1_rows <- function(d, contrast_id, weighted = FALSE) {
  do.call(rbind, lapply(c(0L, 1L), function(a) {
    x <- d[d$exposure == a, , drop = FALSE]
    w <- if (weighted) x$overlap_weight else rep(1, nrow(x))
    q <- v2r_weighted_quantile(as.numeric(x$age), w)
    woman <- grepl("female|woman", tolower(as.character(x$sex)))
    data.frame(
      contrast_id = contrast_id,
      analysis = if (weighted) "overlap_weighted" else "unweighted",
      arm = if (a == 1L) "epilepsy" else unique(x$analysis_arm)[1],
      n_people = nrow(x),
      weight_sum = if (weighted) sum(w) else NA_real_,
      age_mean = weighted.mean(as.numeric(x$age), w, na.rm = TRUE),
      age_sd = v2r_weighted_sd(as.numeric(x$age), w),
      age_median = q[2], age_q1 = q[1], age_q3 = q[3],
      women_n = if (weighted) NA_real_ else sum(woman, na.rm = TRUE),
      women_weighted_sum = if (weighted) sum(w * woman, na.rm = TRUE) else NA_real_,
      women_percent = 100 * sum(w * woman, na.rm = TRUE) / sum(w),
      stringsAsFactors = FALSE
    )
  }))
}

v2r_u1_descriptives <- function(cohort, designs) {
  rows <- list()
  ids <- c("V1_EPI_VS_MIG_PRIMARY", "V1_EPI_VS_SYN_PRIMARY")
  for (id in ids) {
    keys <- v2r_design_keys(designs, id)
    by <- intersect(c("module", "base_scenario", "person_id", "arm", "index_date"),
                    names(keys))
    source <- unique(cohort[, unique(c(by, "age", "sex")), drop = FALSE])
    d <- merge(keys, source, by = by, all.x = TRUE, sort = FALSE)
    if (anyNA(d$age) || anyNA(d$sex)) stop("U1 merge left missing age/sex for ", id,
                                          call. = FALSE)
    rows[[length(rows) + 1L]] <- v2r_u1_rows(d, id, FALSE)
    rows[[length(rows) + 1L]] <- v2r_u1_rows(d, id, TRUE)
  }
  do.call(rbind, rows)
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

v2r_ids_sql <- function(ids) {
  ids <- unique(as.character(ids))
  if (any(!grepl("^[0-9]+$", ids))) stop("Non-numeric person identifier.", call. = FALSE)
  paste(ids, collapse = ",")
}

v2r_measurement_sql <- function(ids) {
  codes <- paste(vapply(v2r_candidate_loincs()$loinc_code, epibone_sql_string,
                        character(1)), collapse = ",")
  paste0(
    "WITH resolved AS (SELECT concept_id,concept_code,concept_name FROM ",
    epibone_table("concept"), " WHERE vocabulary_id='LOINC' AND concept_code IN (", codes,
    ") AND invalid_reason IS NULL)\n",
    "SELECT DISTINCT m.person_id,m.measurement_id,m.measurement_date,",
    "r.concept_code AS loinc_code,r.concept_name,m.value_as_number,",
    "COALESCE(u.concept_name,'') AS unit_name\n",
    "FROM ", epibone_table("measurement"), " m JOIN resolved r ON r.concept_id IN ",
    "(m.measurement_concept_id,m.measurement_source_concept_id)\n",
    "LEFT JOIN ", epibone_table("concept"), " u ON u.concept_id=m.unit_concept_id\n",
    "WHERE m.person_id IN UNNEST([", v2r_ids_sql(ids), "])"
  )
}

v2r_condition_requests <- function() {
  base <- unique(rbind(epibone_condition_requests(),
                       epibone_icd9_condition_requests()))
  broad <- base[base$set_name == "broad_fracture", ]
  falls <- base[base$set_name %in% c("falls_00_09", "falls_10_19"), ]
  rbind(
    transform(broad, set_name = "broad_fracture"),
    transform(falls, set_name = "falls"),
    data.frame(set_name = c("bone_history", "bone_history", "bone_history",
                            "bone_history", "m81_osteoporosis", "m81_osteoporosis",
                            "m858_osteopenia", "m858_osteopenia"),
               pattern = c("M80%", "M81%", "M85%", "733.0%",
                           "M81%", "733.0%", "M85.8%", "733.9%"),
               vocabulary_id = c(rep("ICD10CM", 3), "ICD9CM", "ICD10CM",
                                 "ICD9CM", "ICD10CM", "ICD9CM"),
               stringsAsFactors = FALSE)
  )
}

v2r_event_sql <- function(ids) {
  cond <- epibone_condition_map_ctes(v2r_condition_requests(), prefix = "v2rcond")
  paste0(
    "WITH ", cond, ",\n",
    "procedures AS (SELECT DISTINCT p.person_id,p.procedure_occurrence_id AS event_id,\n",
    " p.procedure_date AS event_date,CONCAT('dxa_',c.concept_code) AS set_name\n",
    " FROM ", epibone_table("procedure_occurrence"), " p JOIN ",
    epibone_table("concept"), " c ON c.concept_id=p.procedure_source_concept_id\n",
    " WHERE p.person_id IN UNNEST([", v2r_ids_sql(ids), "])\n",
    " AND c.vocabulary_id IN ('CPT4','HCPCS') AND c.concept_code IN ",
    "('77080','77081','77085','77086')),\n",
    "conditions AS (SELECT DISTINCT co.person_id,co.condition_occurrence_id AS event_id,\n",
    " co.condition_start_date AS event_date,m.set_name\n",
    " FROM ", epibone_table("condition_occurrence"), " co\n",
    " JOIN v2rcond_map m ON m.source_concept_id=co.condition_source_concept_id\n",
    " WHERE co.person_id IN UNNEST([", v2r_ids_sql(ids), "]))\n",
    "SELECT person_id,event_id,event_date,set_name,'procedure_source_concept_id' AS source FROM procedures\n",
    "UNION ALL SELECT person_id,event_id,event_date,set_name,",
    "'condition_source_concept_id' AS source FROM conditions"
  )
}

v2r_query_chunks <- function(ids, builder, chunk_size = 3000L) {
  ids <- unique(ids)
  groups <- split(ids, ceiling(seq_along(ids) / chunk_size))
  out <- lapply(seq_along(groups), function(i) {
    message("V2R enclave enrichment query chunk ", i, "/", length(groups), "...")
    as.data.frame(epibone_bq(builder(groups[[i]])))
  })
  if (!length(out)) return(data.frame())
  do.call(rbind, out)
}

v2r_primary_anchors <- function(cohort, designs) {
  keys <- v2r_design_keys(designs, "V1_EPI_VS_MIG_PRIMARY")
  by <- intersect(c("module", "base_scenario", "person_id", "arm", "index_date",
                    "landmark_date", "obs_end"), names(keys))
  extra <- unique(cohort[, unique(c(by, "death_date", "age", "sex")), drop = FALSE])
  out <- merge(keys, extra, by = by, all.x = TRUE, sort = FALSE)
  if (anyDuplicated(out$person_id)) stop("Primary V1 design is not one row per person.",
                                        call. = FALSE)
  out
}

v2r_fetch_enrichment <- function(anchors) {
  private <- file.path(EPIBONE_ROOT, "private_enclave_only")
  signature <- digest::digest(list(
    cdr = Sys.getenv("WORKSPACE_CDR"),
    person = sort(as.character(anchors$person_id)),
    index = sort(as.character(anchors$index_date)),
    loinc_codes = sort(v2r_candidate_loincs()$loinc_code)
  ), algo = "sha256")
  path <- file.path(private, paste0("v2r_u2_u3_enrichment_", signature, ".rds"))
  if (file.exists(path)) {
    message("Reusing enclave-only U2/U3 enrichment checkpoint.")
    return(readRDS(path))
  }
  measurements <- v2r_query_chunks(anchors$person_id, v2r_measurement_sql)
  events <- v2r_query_chunks(anchors$person_id, v2r_event_sql)
  if (nrow(measurements)) measurements$measurement_date <- as.Date(measurements$measurement_date)
  if (nrow(events)) events$event_date <- as.Date(events$event_date)
  out <- list(measurements = measurements, events = events, signature = signature)
  aou_write_private_rds(out, path)
  out
}

# U2-only enrichment deliberately avoids the event query used by U3. Reuse the
# measurement component of a prior full checkpoint when available; otherwise
# cache only the minimum participant-level measurement extract in the enclave.
v2r_fetch_measurement_enrichment <- function(anchors) {
  private <- file.path(EPIBONE_ROOT, "private_enclave_only")
  signature <- digest::digest(list(
    cdr = Sys.getenv("WORKSPACE_CDR"),
    person = sort(as.character(anchors$person_id)),
    index = sort(as.character(anchors$index_date)),
    loinc_codes = sort(v2r_candidate_loincs()$loinc_code)
  ), algo = "sha256")

  full_path <- file.path(private, paste0("v2r_u2_u3_enrichment_", signature, ".rds"))
  if (file.exists(full_path)) {
    message("Reusing measurement component of enclave-only U2/U3 checkpoint.")
    full <- readRDS(full_path)
    return(list(measurements = full$measurements, signature = signature))
  }

  path <- file.path(private, paste0("v2r_u2_measurements_", signature, ".rds"))
  if (file.exists(path)) {
    message("Reusing enclave-only U2 measurement checkpoint.")
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

v2r_measurement_frame <- function(anchors, enrichment) {
  m <- enrichment$measurements
  if (!nrow(m)) return(data.frame())
  d <- merge(m, anchors[, c("person_id", "arm", "analysis_arm", "exposure",
                            "index_date", "overlap_weight")],
             by = "person_id", all.x = FALSE, all.y = FALSE)
  d$day <- as.integer(d$measurement_date - d$index_date)
  d$period <- ifelse(d$day >= -1095 & d$day <= -1, "baseline_-1095_-1",
                     ifelse(d$day >= 1 & d$day <= 1095, "followup_1_1095", NA))
  spec <- v2r_candidate_loincs()
  merge(d, spec, by = "loinc_code", all.x = TRUE, sort = FALSE)
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
    count <- stats::aggregate(usable$person_id,
      by = list(loinc_code = usable$loinc_code, period = usable$period,
                arm = usable$analysis_arm),
      FUN = function(x) length(unique(x)))
    names(count)[4] <- "n_people_with_numeric_value"
    grid <- merge(grid, count, all.x = TRUE,
                  by = c("loinc_code", "period", "arm"), sort = FALSE)
  } else grid$n_people_with_numeric_value <- NA_real_
  grid$n_people_with_numeric_value[is.na(grid$n_people_with_numeric_value)] <- 0
  grid <- merge(grid, v2r_candidate_loincs(), by = "loinc_code", all.x = TRUE)
  if (nrow(usable)) {
    combined <- stats::aggregate(usable$person_id,
      by = list(class = usable$class, period = usable$period,
                arm = usable$analysis_arm),
      FUN = function(x) length(unique(x)))
    names(combined)[4] <- "n_people_with_numeric_value"
    combined$loinc_code <- paste0("ANY_", combined$class)
    combined$measure <- ifelse(combined$class == "T_SCORE",
                               "any_candidate_t_score", "any_candidate_bmd")
    combined <- combined[, names(grid), drop = FALSE]
    grid <- rbind(grid, combined)
  }

  baseline <- usable[usable$period == "baseline_-1095_-1" &
                       usable$class == "T_SCORE" &
                       usable$value_as_number >= -10 & usable$value_as_number <= 10, ]
  n_gate <- vapply(c("migraine", "epilepsy"), function(a)
    length(unique(baseline$person_id[baseline$analysis_arm == a])), numeric(1))
  gate <- data.frame(
    module = "U2_BASELINE_T_SCORE", arm = names(n_gate), n_people = unname(n_gate),
    threshold = c(100L, 200L),
    gate = if (n_gate["migraine"] >= 100 && n_gate["epilepsy"] >= 200)
      "PROCEED" else "COUNTS_ONLY_GATE_FAILED", stringsAsFactors = FALSE
  )
  list(availability = grid, gate = gate, baseline = baseline)
}

v2r_u3_dataset <- function(anchors, enrichment, outcome = "broad_fracture") {
  e <- enrichment$events
  if (!nrow(e)) return(anchors[0, , drop = FALSE])
  e <- merge(e, anchors[, c("person_id", "index_date", "landmark_date")],
             by = "person_id", all.x = FALSE)
  e$day_index <- as.integer(e$event_date - e$index_date)
  dxa_people <- unique(e$person_id[grepl("^dxa_", e$set_name) &
                                      e$day_index >= -1095 & e$day_index <= -1])
  bone_people <- unique(e$person_id[e$set_name == "bone_history" & e$event_date < e$index_date])
  eligible <- anchors[anchors$person_id %in% setdiff(dxa_people, bone_people), , drop = FALSE]
  outcome_events <- e[e$set_name == outcome, c("person_id", "event_date"), drop = FALSE]
  prior_people <- unique(outcome_events$person_id[outcome_events$event_date <=
    anchors$landmark_date[match(outcome_events$person_id, anchors$person_id)]])
  eligible <- eligible[!eligible$person_id %in% prior_people, , drop = FALSE]
  post <- outcome_events[outcome_events$event_date >
    anchors$landmark_date[match(outcome_events$person_id, anchors$person_id)], , drop = FALSE]
  first_event <- if (nrow(post)) stats::aggregate(event_date ~ person_id, post, min) else
    data.frame(person_id = anchors$person_id[0], event_date = as.Date(character()))
  d <- merge(eligible, first_event, by = "person_id", all.x = TRUE, sort = FALSE)
  admin_end <- d$landmark_date + 1095L
  observed_end <- pmin(d$obs_end, admin_end, na.rm = TRUE)
  event_ok <- !is.na(d$event_date) & d$event_date <= observed_end &
    (is.na(d$death_date) | d$event_date <= d$death_date)
  death_ok <- !is.na(d$death_date) & d$death_date <= observed_end & !event_ok
  final_date <- observed_end
  final_date[event_ok] <- d$event_date[event_ok]
  final_date[death_ok] <- d$death_date[death_ok]
  d$time <- pmax(1, as.numeric(final_date - d$landmark_date))
  d$status <- ifelse(event_ok, 1L, ifelse(death_ok, 2L, 0L))
  d$event <- as.integer(d$status == 1L)
  d
}

v2r_u3_feasibility <- function(anchors, enrichment) {
  d <- v2r_u3_dataset(anchors, enrichment, "broad_fracture")
  rows <- do.call(rbind, lapply(c(0L, 1L), function(a) data.frame(
    module = "U3_DXA_RESTRICTED", arm = if (a == 1L) "epilepsy" else "migraine",
    n_people = sum(d$exposure == a), events_broad_fracture = sum(d$event[d$exposure == a]),
    stringsAsFactors = FALSE
  )))
  proceed <- all(rows$events_broad_fracture >= 21)
  rows$threshold_events_per_arm <- 21L
  rows$gate <- if (proceed) "PROCEED_EXPLORATORY_DIRECTION_CHECK" else
    "COUNTS_ONLY_GATE_FAILED"
  list(rows = rows, proceed = proceed)
}

v2r_require_review <- function() {
  if (!identical(toupper(trimws(Sys.getenv("AOU_EPIBONE_V2R_REVIEWED", unset = ""))),
                 "YES")) {
    stop("Stage B is gated. Review Stage A, then set AOU_EPIBONE_V2R_REVIEWED='YES'.",
         call. = FALSE)
  }
  invisible(TRUE)
}

v2r_robust_weighted_difference <- function(y, exposure, w) {
  keep <- is.finite(y) & exposure %in% c(0, 1) & is.finite(w) & w > 0
  y <- y[keep]; exposure <- exposure[keep]; w <- w[keep]
  X <- cbind(1, exposure)
  bread <- solve(crossprod(X, w * X))
  beta <- as.vector(bread %*% crossprod(X, w * y))
  residual <- y - as.vector(X %*% beta)
  meat <- crossprod(X, (w * residual)^2 * X) * length(y) / (length(y) - ncol(X))
  vc <- bread %*% meat %*% bread
  se <- sqrt(vc[2, 2]); z <- beta[2] / se
  c(comparator = beta[1], exposure = sum(beta), difference = beta[2], se = se,
    ci_low = beta[2] - 1.96 * se, ci_high = beta[2] + 1.96 * se,
    p_value = 2 * stats::pnorm(-abs(z)))
}

v2r_u2_models <- function(anchors, enrichment) {
  feas <- v2r_u2_feasibility(anchors, enrichment)
  if (!all(feas$gate$gate == "PROCEED")) return(data.frame(
    estimand = "baseline_t_score", gate = "NOT_ESTIMATED_U2_GATE_FAILED",
    stringsAsFactors = FALSE))
  b <- feas$baseline
  lowest <- stats::aggregate(value_as_number ~ person_id, b, min)
  names(lowest)[2] <- "lowest_t_score"
  d <- merge(anchors, lowest, by = "person_id", all.x = FALSE, sort = FALSE)
  definitions <- list(
    lowest_t_score = d$lowest_t_score,
    osteoporosis_t_le_minus_2_5 = as.numeric(d$lowest_t_score <= -2.5),
    osteopenia_t_minus_2_5_to_minus_1 = as.numeric(d$lowest_t_score > -2.5 &
                                                     d$lowest_t_score <= -1.0)
  )
  do.call(rbind, lapply(names(definitions), function(name) {
    est <- v2r_robust_weighted_difference(definitions[[name]], d$exposure,
                                          d$overlap_weight)
    data.frame(
      estimand = name, n_migraine = sum(d$exposure == 0),
      n_epilepsy = sum(d$exposure == 1), estimate_migraine = est["comparator"],
      estimate_epilepsy = est["exposure"], difference_epilepsy_minus_migraine = est["difference"],
      robust_se = est["se"], ci_low = est["ci_low"], ci_high = est["ci_high"],
      p_value = est["p_value"], gate = "ESTIMATED_OVERLAP_WEIGHTED",
      stringsAsFactors = FALSE
    )
  }))
}

v2r_u3_models <- function(anchors, enrichment) {
  gate <- v2r_u3_feasibility(anchors, enrichment)
  outcomes <- c("broad_fracture", "m81_osteoporosis", "m858_osteopenia", "falls")
  if (!gate$proceed) return(data.frame(
    outcome = outcomes, gate = "NOT_ESTIMATED_U3_BROAD_FRACTURE_GATE_FAILED",
    stringsAsFactors = FALSE))
  reps <- suppressWarnings(as.integer(Sys.getenv("AOU_EPIBONE_BOOTSTRAP_REPS", "300")))
  if (!is.finite(reps) || reps < 100) stop("Use at least 100 bootstrap repetitions.",
                                          call. = FALSE)
  rows <- lapply(outcomes, function(outcome) {
    d <- v2r_u3_dataset(anchors, enrichment, outcome)
    support <- epibone_support_counts(d)
    row <- data.frame(
      outcome = outcome, n_migraine = support$n[1], n_epilepsy = support$n[2],
      events_migraine = support$events[1], events_epilepsy = support$events[2],
      deaths_migraine = support$deaths[1], deaths_epilepsy = support$deaths[2],
      hr = NA_real_, hr_ci_low = NA_real_, hr_ci_high = NA_real_, hr_p_value = NA_real_,
      risk_migraine_3y = NA_real_, risk_epilepsy_3y = NA_real_, risk_difference_3y = NA_real_,
      risk_ratio_3y = NA_real_, subdistribution_hr = NA_real_, gate = "OUTCOME_EVENT_GATE_FAILED",
      stringsAsFactors = FALSE
    )
    if (all(support$n > 20) && all(support$events > 20)) {
      cox <- tryCatch(epibone_weighted_cox(d), error = function(e) NULL)
      absolute <- tryCatch(epibone_absolute_bootstrap(
        d, reps, epibone_outcome_seed("V2R_U3", outcome, 1095L)), error = function(e) NULL)
      if (!is.null(cox)) row[1, c("hr", "hr_ci_low", "hr_ci_high", "hr_p_value")] <-
        unname(cox[c("hr", "ci_low", "ci_high", "p_value")])
      if (!is.null(absolute)) row[1, c("risk_migraine_3y", "risk_epilepsy_3y",
                                      "risk_difference_3y", "risk_ratio_3y")] <-
        unname(absolute$point)
      if (all(support$deaths > 20)) {
        fg <- tryCatch(epibone_weighted_finegray(d), error = function(e) NULL)
        if (!is.null(fg)) row$subdistribution_hr <- fg["s_hr"]
      }
      row$gate <- "ESTIMATED_EXPLORATORY_DIRECTION_CHECK"
    }
    row
  })
  do.call(rbind, rows)
}

v2r_write_stage_a <- function(u1, audit, u2, u3) {
  aou_write_explicit_csv(u1, file.path(EPIBONE_ROOT, "results", "u1_descriptives.csv"),
    count_cols = c("n_people", "women_n"), support_cols = "n_people",
    dependent_cols = c("age_mean", "age_sd", "age_median", "age_q1", "age_q3",
                       "women_weighted_sum", "women_percent"))
  aou_write_explicit_csv(audit,
    file.path(EPIBONE_ROOT, "results", "feasibility", "u2_loinc_vocabulary_audit.csv"),
    count_cols = c("n_records", "n_people", "n_numeric_values"))
  aou_write_explicit_csv(u2$availability,
    file.path(EPIBONE_ROOT, "results", "feasibility", "u2_measurement_counts.csv"),
    count_cols = "n_people_with_numeric_value")
  aou_write_explicit_csv(u2$gate,
    file.path(EPIBONE_ROOT, "results", "feasibility", "u2_model_gate.csv"),
    count_cols = "n_people")
  aou_write_explicit_csv(u3$rows,
    file.path(EPIBONE_ROOT, "results", "feasibility", "u3_dxa_replication_gate.csv"),
    count_cols = c("n_people", "events_broad_fracture"))
}

v2r_run_stage_a <- function() {
  aou_ducc_preflight()
  artifacts <- v2r_load_v1_artifacts()
  audit <- as.data.frame(epibone_bq(v2r_loinc_audit_sql()))
  u1 <- v2r_u1_descriptives(artifacts$cohort, artifacts$designs)
  anchors <- v2r_primary_anchors(artifacts$cohort, artifacts$designs)
  enrichment <- v2r_fetch_enrichment(anchors)
  u2 <- v2r_u2_feasibility(anchors, enrichment)
  u3 <- v2r_u3_feasibility(anchors, enrichment)
  v2r_write_stage_a(u1, audit, u2, u3)
  aou_write_private_rds(list(v1_root = artifacts$v1_root,
                             enrichment_signature = enrichment$signature),
                        file.path(EPIBONE_ROOT, "private_enclave_only",
                                  "v2r_stage_a_manifest.rds"))
  epibone_write_manifest("v2r_stage_a", c(
    "u1_descriptives.csv", "feasibility/u2_loinc_vocabulary_audit.csv",
    "feasibility/u2_measurement_counts.csv", "feasibility/u2_model_gate.csv",
    "feasibility/u3_dxa_replication_gate.csv"))
  invisible(list(u1 = u1, audit = audit, u2 = u2, u3 = u3))
}

v2r_run_stage_b <- function() {
  aou_ducc_preflight(); v2r_require_review()
  artifacts <- v2r_load_v1_artifacts()
  anchors <- v2r_primary_anchors(artifacts$cohort, artifacts$designs)
  enrichment <- v2r_fetch_enrichment(anchors)
  u2 <- v2r_u2_models(anchors, enrichment)
  u3 <- v2r_u3_models(anchors, enrichment)
  aou_write_explicit_csv(u2, file.path(EPIBONE_ROOT, "results", "outcomes",
                                       "u2_bmd_models.csv"),
    count_cols = c("n_migraine", "n_epilepsy"),
    support_cols = c("n_migraine", "n_epilepsy"),
    dependent_cols = setdiff(names(u2), c("estimand", "gate", "n_migraine", "n_epilepsy")))
  aou_write_explicit_csv(u3, file.path(EPIBONE_ROOT, "results", "outcomes",
                                       "u3_dxa_restricted_models.csv"),
    count_cols = c("n_migraine", "n_epilepsy", "events_migraine", "events_epilepsy",
                   "deaths_migraine", "deaths_epilepsy"),
    support_cols = c("events_migraine", "events_epilepsy"),
    dependent_cols = setdiff(names(u3), c("outcome", "gate", "n_migraine", "n_epilepsy",
                                          "events_migraine", "events_epilepsy",
                                          "deaths_migraine", "deaths_epilepsy")))
  epibone_write_manifest("v2r_stage_b", c("outcomes/u2_bmd_models.csv",
                                           "outcomes/u3_dxa_restricted_models.csv"))
  invisible(list(u2 = u2, u3 = u3))
}
