# Outcome stage. This file is called only after explicit owner review of Stage A.

epibone_require_design_review <- function() {
  if (!identical(toupper(trimws(Sys.getenv("AOU_EPIBONE_DESIGN_REVIEWED", unset = ""))),
                 "YES")) {
    stop(
      "Outcome modelling is gated. Review results/design/, then set ",
      "AOU_EPIBONE_DESIGN_REVIEWED='YES'.", call. = FALSE
    )
  }
  invisible(TRUE)
}

epibone_merge_design_outcomes <- function(cohort, design_keys, outcome, horizon_days) {
  date_col <- paste0(outcome, "_date")
  prior_col <- paste0("prior_", outcome)
  required <- c(date_col, prior_col, "death_date")
  if (!all(required %in% names(cohort))) {
    stop("Missing frozen outcome fields: ", paste(setdiff(required, names(cohort)),
                                                   collapse = ", "), call. = FALSE)
  }
  by <- c("module", "base_scenario", "person_id", "arm", "index_date",
          "landmark_date", "obs_end")
  select <- unique(c(by, date_col, prior_col, "death_date"))
  d <- merge(design_keys, cohort[, select, drop = FALSE], by = by, all.x = TRUE,
             sort = FALSE)
  d <- d[is.na(d[[prior_col]]) | d[[prior_col]] == 0, , drop = FALSE]
  admin_end <- d$landmark_date + as.integer(horizon_days)
  observed_end <- pmin(d$obs_end, admin_end, na.rm = TRUE)
  event_date <- d[[date_col]]
  event_ok <- !is.na(event_date) & event_date <= observed_end &
    (is.na(d$death_date) | event_date <= d$death_date)
  death_ok <- !is.na(d$death_date) & d$death_date <= observed_end & !event_ok
  final_date <- observed_end
  final_date[event_ok] <- event_date[event_ok]
  final_date[death_ok] <- d$death_date[death_ok]
  d$time <- pmax(1, as.numeric(final_date - d$landmark_date))
  d$status <- ifelse(event_ok, 1L, ifelse(death_ok, 2L, 0L))
  d$event <- as.integer(d$status == 1L)
  d
}

epibone_support_counts <- function(d) {
  n <- vapply(c(0, 1), function(a) sum(d$exposure == a), numeric(1))
  events <- vapply(c(0, 1), function(a) sum(d$event[d$exposure == a]), numeric(1))
  deaths <- vapply(c(0, 1), function(a) sum(d$status[d$exposure == a] == 2), numeric(1))
  list(n = n, events = events, deaths = deaths)
}

epibone_weighted_cox <- function(d) {
  fit <- survival::coxph(
    survival::Surv(time, event) ~ exposure + survival::cluster(person_id),
    data = d, weights = overlap_weight, robust = TRUE, ties = "efron"
  )
  beta <- unname(stats::coef(fit)["exposure"])
  se <- sqrt(unname(stats::vcov(fit)["exposure", "exposure"]))
  z <- beta / se
  c(log_hr = beta, se = se, hr = exp(beta), ci_low = exp(beta - 1.96 * se),
    ci_high = exp(beta + 1.96 * se), p_value = 2 * stats::pnorm(-abs(z)))
}

epibone_weighted_cif <- function(d) {
  if (!nrow(d)) return(NA_real_)
  times <- sort(unique(d$time[d$status %in% c(1L, 2L)]))
  survival_probability <- 1
  cif <- 0
  for (time in times) {
    risk <- sum(d$overlap_weight[d$time >= time])
    if (!is.finite(risk) || risk <= 0) next
    event_weight <- sum(d$overlap_weight[d$time == time & d$status == 1L])
    all_failure_weight <- sum(d$overlap_weight[d$time == time & d$status %in% c(1L, 2L)])
    cif <- cif + survival_probability * event_weight / risk
    survival_probability <- survival_probability * (1 - all_failure_weight / risk)
  }
  max(0, min(1, cif))
}

epibone_absolute_bootstrap <- function(d, reps, seed) {
  estimate_arm <- function(a, data = d) epibone_weighted_cif(data[data$exposure == a, ])
  risk0 <- estimate_arm(0)
  risk1 <- estimate_arm(1)
  point <- c(risk_comparator = risk0, risk_exposure = risk1,
             risk_difference = risk1 - risk0,
             risk_ratio = if (risk0 > 0) risk1 / risk0 else NA_real_)
  set.seed(seed)
  boot <- matrix(NA_real_, nrow = reps, ncol = 4,
                 dimnames = list(NULL, names(point)))
  indices <- split(seq_len(nrow(d)), d$exposure)
  for (b in seq_len(reps)) {
    draw <- unlist(lapply(indices, function(idx) sample(idx, length(idx), replace = TRUE)),
                   use.names = FALSE)
    db <- d[draw, , drop = FALSE]
    r0 <- epibone_weighted_cif(db[db$exposure == 0, ])
    r1 <- epibone_weighted_cif(db[db$exposure == 1, ])
    boot[b, ] <- c(r0, r1, r1 - r0, if (r0 > 0) r1 / r0 else NA_real_)
  }
  ci <- t(apply(boot, 2, stats::quantile, probs = c(0.025, 0.975),
                na.rm = TRUE, names = FALSE))
  list(point = point, low = ci[, 1], high = ci[, 2])
}

epibone_weighted_finegray <- function(d) {
  d$status_factor <- factor(d$status, levels = c(0, 1, 2),
                            labels = c("censor", "fracture", "death"))
  fg <- survival::finegray(
    survival::Surv(time, status_factor) ~ exposure + overlap_weight + person_id,
    data = d, etype = "fracture"
  )
  fit <- survival::coxph(
    survival::Surv(fgstart, fgstop, fgstatus) ~ exposure + survival::cluster(person_id),
    data = fg, weights = overlap_weight * fgwt, robust = TRUE, ties = "efron"
  )
  beta <- unname(stats::coef(fit)["exposure"])
  se <- sqrt(unname(stats::vcov(fit)["exposure", "exposure"]))
  z <- beta / se
  c(log_s_hr = beta, se = se, s_hr = exp(beta), ci_low = exp(beta - 1.96 * se),
    ci_high = exp(beta + 1.96 * se), p_value = 2 * stats::pnorm(-abs(z)))
}

epibone_evalue <- function(hr, ci_low, ci_high) {
  convert <- function(x) {
    rr <- if (x < 1) 1 / x else x
    rr + sqrt(rr * (rr - 1))
  }
  point <- convert(hr)
  limit <- if (ci_low <= 1 && ci_high >= 1) 1 else if (hr >= 1) ci_low else ci_high
  c(e_value_point = point, e_value_ci = convert(limit))
}

epibone_outcome_seed <- function(contrast_id, outcome, horizon) {
  hex <- substr(digest::digest(paste(contrast_id, outcome, horizon), algo = "sha256"), 1, 7)
  as.integer(strtoi(hex, base = 16L))
}

epibone_analyse_outcome <- function(cohort, keys, contrast_id, outcome, family,
                                    bootstrap_reps) {
  d3 <- epibone_merge_design_outcomes(cohort, keys, outcome, 1095L)
  support3 <- epibone_support_counts(d3)
  cox_row <- data.frame(
    contrast_id = contrast_id, outcome = outcome, family = family,
    n_comparator = support3$n[1], n_exposure = support3$n[2],
    events_comparator = support3$events[1], events_exposure = support3$events[2],
    log_hr = NA_real_, robust_se = NA_real_, hr = NA_real_, ci_low = NA_real_,
    ci_high = NA_real_, p_value = NA_real_, e_value_point = NA_real_,
    e_value_ci = NA_real_, gate = "NOT_ESTIMABLE_EVENT_GATE",
    stringsAsFactors = FALSE
  )
  if (all(support3$n > 20) && all(support3$events > 20)) {
    estimate <- tryCatch(epibone_weighted_cox(d3), error = function(e) NULL)
    if (!is.null(estimate) && all(is.finite(estimate))) {
      evalue <- epibone_evalue(estimate["hr"], estimate["ci_low"], estimate["ci_high"])
      cox_row[1, c("log_hr", "robust_se", "hr", "ci_low", "ci_high", "p_value")] <-
        unname(estimate[c("log_hr", "se", "hr", "ci_low", "ci_high", "p_value")])
      cox_row$e_value_point <- evalue[1]
      cox_row$e_value_ci <- evalue[2]
      cox_row$gate <- "ESTIMATED_CAUSE_SPECIFIC_COX"
    } else {
      cox_row$gate <- "NOT_ESTIMABLE_MODEL_FAILURE"
    }
  }

  absolute <- NULL
  if (family != "negative_control") {
    absolute <- do.call(rbind, lapply(c(`1_year` = 365L, `3_year` = 1095L), function(horizon) {
      dh <- epibone_merge_design_outcomes(cohort, keys, outcome, horizon)
      support <- epibone_support_counts(dh)
      row <- data.frame(
        contrast_id = contrast_id, outcome = outcome, family = family,
        horizon_days = horizon, n_comparator = support$n[1], n_exposure = support$n[2],
        events_comparator = support$events[1], events_exposure = support$events[2],
        risk_comparator = NA_real_, risk_comparator_low = NA_real_,
        risk_comparator_high = NA_real_, risk_exposure = NA_real_,
        risk_exposure_low = NA_real_, risk_exposure_high = NA_real_,
        risk_difference = NA_real_, risk_difference_low = NA_real_,
        risk_difference_high = NA_real_, risk_ratio = NA_real_,
        risk_ratio_low = NA_real_, risk_ratio_high = NA_real_,
        bootstrap_reps = bootstrap_reps, gate = "NOT_ESTIMABLE_EVENT_GATE",
        stringsAsFactors = FALSE
      )
      if (all(support$n > 20) && all(support$events > 20)) {
        est <- tryCatch(epibone_absolute_bootstrap(
          dh, bootstrap_reps, epibone_outcome_seed(contrast_id, outcome, horizon)
        ), error = function(e) NULL)
        if (!is.null(est)) {
          row[1, c("risk_comparator", "risk_exposure", "risk_difference", "risk_ratio")] <-
            unname(est$point)
          row[1, c("risk_comparator_low", "risk_exposure_low", "risk_difference_low",
                   "risk_ratio_low")] <- unname(est$low)
          row[1, c("risk_comparator_high", "risk_exposure_high", "risk_difference_high",
                   "risk_ratio_high")] <- unname(est$high)
          row$gate <- "ESTIMATED_WEIGHTED_AALEN_JOHANSEN"
        } else {
          row$gate <- "NOT_ESTIMABLE_MODEL_FAILURE"
        }
      }
      row
    }))
  }

  finegray <- data.frame(
    contrast_id = contrast_id, outcome = outcome, family = family,
    n_comparator = support3$n[1], n_exposure = support3$n[2],
    events_comparator = support3$events[1], events_exposure = support3$events[2],
    deaths_comparator = support3$deaths[1], deaths_exposure = support3$deaths[2],
    log_s_hr = NA_real_, robust_se = NA_real_, s_hr = NA_real_, ci_low = NA_real_,
    ci_high = NA_real_, p_value = NA_real_, gate = "NOT_ESTIMABLE_DEATH_GATE",
    stringsAsFactors = FALSE
  )
  if (all(support3$n > 20) && all(support3$events > 20) && all(support3$deaths > 20)) {
    estimate <- tryCatch(epibone_weighted_finegray(d3), error = function(e) NULL)
    if (!is.null(estimate) && all(is.finite(estimate))) {
      finegray[1, c("log_s_hr", "robust_se", "s_hr", "ci_low", "ci_high", "p_value")] <-
        unname(estimate[c("log_s_hr", "se", "s_hr", "ci_low", "ci_high", "p_value")])
      finegray$gate <- "ESTIMATED_FINE_GRAY"
    } else {
      finegray$gate <- "NOT_ESTIMABLE_MODEL_FAILURE"
    }
  }
  list(cox = cox_row, absolute = absolute, finegray = finegray)
}

epibone_apply_multiplicity <- function(cox) {
  cox$p_holm_coprimary <- NA_real_
  co_primary <- cox$contrast_id == "V1_EPI_VS_MIG_PRIMARY" &
    cox$outcome == "broad_fracture" & is.finite(cox$p_value)
  if (any(co_primary)) cox$p_holm_coprimary[co_primary] <- stats::p.adjust(
    cox$p_value[co_primary], method = "holm"
  )
  cox$p_bh_secondary_bone <- NA_real_
  for (contrast in unique(cox$contrast_id)) {
    use <- cox$contrast_id == contrast & cox$family == "secondary_bone" &
      is.finite(cox$p_value)
    if (any(use)) cox$p_bh_secondary_bone[use] <- stats::p.adjust(
      cox$p_value[use], method = "BH"
    )
  }
  cox
}

epibone_run_outcome_stage <- function(cohort = epibone_build_or_resume_cohort()) {
  aou_ducc_preflight()
  epibone_require_design_review()
  design_path <- file.path(EPIBONE_ROOT, "private_enclave_only", "accepted_designs.rds")
  if (!file.exists(design_path)) stop("Run Stage A design first.", call. = FALSE)
  designs <- readRDS(design_path)
  outcomes <- epibone_outcome_sets()
  reps <- suppressWarnings(as.integer(Sys.getenv("AOU_EPIBONE_BOOTSTRAP_REPS", "300")))
  if (!is.finite(reps) || reps < 100) stop("Use at least 100 bootstrap repetitions.", call. = FALSE)
  fitted <- list()
  counter <- 0L
  for (contrast_id in names(designs)) {
    design_entry <- designs[[contrast_id]]
    if (is.null(design_entry)) next
    keys <- if (is.list(design_entry) && !is.data.frame(design_entry)) {
      design_entry$overlap
    } else design_entry
    for (i in seq_len(nrow(outcomes))) {
      counter <- counter + 1L
      message("Outcome model ", contrast_id, ": ", outcomes$outcome[i])
      fitted[[counter]] <- epibone_analyse_outcome(
        cohort, keys, contrast_id, outcomes$outcome[i], outcomes$family[i], reps
      )
    }
  }
  if (!length(fitted)) stop("No design passed Stage A; no outcome was fitted.", call. = FALSE)
  cox <- do.call(rbind, lapply(fitted, `[[`, "cox"))
  absolute <- do.call(rbind, lapply(fitted, `[[`, "absolute"))
  finegray <- do.call(rbind, lapply(fitted, `[[`, "finegray"))
  cox <- epibone_apply_multiplicity(cox)

  matching <- do.call(rbind, lapply(names(designs), function(contrast_id) {
    entry <- designs[[contrast_id]]
    if (is.null(entry) || is.null(entry$matching)) return(NULL)
    d <- epibone_merge_design_outcomes(cohort, entry$matching, "broad_fracture", 1095L)
    support <- epibone_support_counts(d)
    row <- data.frame(
      contrast_id = contrast_id, outcome = "broad_fracture",
      n_comparator = support$n[1], n_exposure = support$n[2],
      events_comparator = support$events[1], events_exposure = support$events[2],
      hr = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p_value = NA_real_,
      gate = "NOT_ESTIMABLE_EVENT_GATE", stringsAsFactors = FALSE
    )
    if (all(support$n > 20) && all(support$events > 20)) {
      estimate <- tryCatch(epibone_weighted_cox(d), error = function(e) NULL)
      if (!is.null(estimate)) {
        row[1, c("hr", "ci_low", "ci_high", "p_value")] <-
          unname(estimate[c("hr", "ci_low", "ci_high", "p_value")])
        row$gate <- "ESTIMATED_1TO1_MATCHING_SENSITIVITY"
      } else row$gate <- "NOT_ESTIMABLE_MODEL_FAILURE"
    }
    row
  }))
  if (is.null(matching)) matching <- data.frame(contrast_id = character())

  outdir <- file.path(EPIBONE_ROOT, "results", "outcomes")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  aou_write_explicit_csv(
    cox, file.path(outdir, "01_cause_specific_cox.csv"),
    count_cols = c("n_comparator", "n_exposure", "events_comparator", "events_exposure"),
    support_cols = c("events_comparator", "events_exposure"),
    dependent_cols = c("log_hr", "robust_se", "hr", "ci_low", "ci_high", "p_value",
                       "e_value_point", "e_value_ci", "p_holm_coprimary",
                       "p_bh_secondary_bone")
  )
  aou_write_explicit_csv(
    absolute, file.path(outdir, "02_absolute_risks.csv"),
    count_cols = c("n_comparator", "n_exposure", "events_comparator", "events_exposure"),
    support_cols = c("events_comparator", "events_exposure"),
    dependent_cols = grep("^(risk_|bootstrap)", names(absolute), value = TRUE)
  )
  aou_write_explicit_csv(
    finegray, file.path(outdir, "03_fine_gray.csv"),
    count_cols = c("n_comparator", "n_exposure", "events_comparator", "events_exposure",
                   "deaths_comparator", "deaths_exposure"),
    support_cols = c("events_comparator", "events_exposure", "deaths_comparator",
                     "deaths_exposure"),
    dependent_cols = c("log_s_hr", "robust_se", "s_hr", "ci_low", "ci_high", "p_value")
  )
  nc <- cox[cox$family == "negative_control", c(
    "contrast_id", "outcome", "events_comparator", "events_exposure",
    "log_hr", "robust_se", "hr", "ci_low", "ci_high", "p_value", "gate"
  ), drop = FALSE]
  aou_write_explicit_csv(
    nc, file.path(outdir, "04_negative_controls.csv"),
    count_cols = c("events_comparator", "events_exposure"),
    support_cols = c("events_comparator", "events_exposure"),
    dependent_cols = c("log_hr", "robust_se", "hr", "ci_low", "ci_high", "p_value")
  )
  aou_write_explicit_csv(
    matching, file.path(outdir, "05_matching_broad_fracture.csv"),
    count_cols = intersect(c("n_comparator", "n_exposure", "events_comparator",
                             "events_exposure"), names(matching)),
    support_cols = intersect(c("events_comparator", "events_exposure"), names(matching)),
    dependent_cols = intersect(c("hr", "ci_low", "ci_high", "p_value"), names(matching))
  )
  interpretation <- data.frame(
    item = c("estimand", "primary_relative_model", "absolute_model", "competing_risk",
             "multiplicity", "negative_controls", "cross_database_synthesis"),
    rule = c(
      "Contrast-specific overlap population; V2 is associational, not a drug effect.",
      "Three-year cause-specific Cox model with overlap weights and robust variance.",
      "Weighted Aalen-Johansen cumulative incidence at 1 and 3 years with stratified bootstrap.",
      "Fine-Gray is secondary and only reported when each arm has more than 20 deaths.",
      "The epilepsy-versus-migraine broad-fracture contrast is the sole AoU primary test; BH is applied within secondary bone outcomes by contrast.",
      "Qualitative bias diagnostic only; no empirical calibration claim.",
      "AoU validates direction/transportability only; it is not pooled with overlapping TriNetX analyses."
    ), stringsAsFactors = FALSE
  )
  aou_write_explicit_csv(interpretation, file.path(outdir, "06_interpretation_rules.csv"))
  epibone_write_manifest("outcomes", c(
    "01_cause_specific_cox.csv", "02_absolute_risks.csv", "03_fine_gray.csv",
    "04_negative_controls.csv", "05_matching_broad_fracture.csv",
    "06_interpretation_rules.csv"
  ))
  message("Stage B complete. Only disclosure-guarded aggregate CSVs are in results/outcomes/.")
  invisible(list(cox = cox, absolute = absolute, finegray = finegray))
}
