# Stage B modelling and disclosure-safe packaging for standalone S32 v1.0.
# Patch level: S32V1_STAGE_B_PATCH_04 (frozen 50-cell multiplicity family).
# This file is sourced only by the guarded resume entry point.

S32V1_MODEL_PATCH <- "S32V1_STAGE_B_PATCH_04"
S32V1_FORCED_COVARIATES <- c("age_index", "sex", "index_year")

s32v1_make_frame <- function(D, EVENTS, endpoint_name) {
  z <- copy(as.data.table(D))
  e <- copy(EVENTS[endpoint == endpoint_name])
  if (!nrow(e)) return(NULL)
  e <- merge(e[, .(person_id, dt)], z[, .(person_id, lm, op_end)], by = "person_id")
  prevalent <- unique(e[dt <= lm, person_id])
  z <- z[!person_id %in% prevalent]
  if (!nrow(z)) return(NULL)
  first_post <- e[
    person_id %in% z$person_id & dt > lm & dt <= op_end,
    .(outcome_date = min(dt)), by = person_id
  ]
  z[, outcome_date := as.Date(NA)]
  if (nrow(first_post)) z[first_post, on = "person_id", outcome_date := i.outcome_date]
  z[, censor_date := as.Date(op_end)]
  z[, event := as.integer(
    !is.na(outcome_date) & outcome_date <= censor_date &
      (is.na(death_date) | outcome_date <= death_date)
  )]
  z[, competing_death := as.integer(
    !is.na(death_date) & death_date <= censor_date &
      (is.na(outcome_date) | death_date < outcome_date)
  )]
  z[, exit_date := censor_date]
  z[event == 1L, exit_date := outcome_date]
  z[competing_death == 1L, exit_date := death_date]
  z[, `:=`(
    fu = as.numeric(exit_date - lm),
    fstatus = fifelse(event == 1L, 1L, fifelse(competing_death == 1L, 2L, 0L))
  )]
  z[is.finite(fu) & fu > 0]
}

s32v1_covariate_df <- function(x) {
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    max(1L, length(unique(na.omit(x))) - 1L)
  } else 1L
}

s32v1_pick_covariates <- function(z, n_events, reserved_df = 1L) {
  available <- S32V1_FIXED_COVARIATE_PRIORITY[vapply(
    S32V1_FIXED_COVARIATE_PRIORITY,
    function(name) name %in% names(z) && mean(!is.na(z[[name]])) >= .80 &&
      length(unique(na.omit(z[[name]]))) > 1L,
    logical(1)
  )]
  forced <- intersect(c("age_index", "sex", "index_year"), available)
  if (!all(c("age_index", "sex", "index_year") %in% forced)) {
    return(list(ok = FALSE, reason = "age, sex, or index year unavailable/constant"))
  }
  allowed <- floor(n_events / 10) - reserved_df
  forced_df <- sum(vapply(forced, function(name) s32v1_covariate_df(z[[name]]), integer(1)))
  if (allowed < forced_df) {
    return(list(ok = FALSE, reason = "event count cannot support the forced age/sex/year core"))
  }
  chosen <- forced; used <- forced_df
  for (name in setdiff(available, forced)) {
    df <- s32v1_covariate_df(z[[name]])
    if (used + df <= allowed) {
      chosen <- c(chosen, name); used <- used + df
    }
  }
  list(ok = TRUE, covariates = chosen, covariate_df = used,
       reduced = length(chosen) < length(available))
}

s32v1_prepare_model_core <- function(z) {
  z <- copy(as.data.table(z))
  if ("sex" %in% names(z)) {
    sex_value <- tolower(trimws(as.character(z$sex)))
    sex_value[!sex_value %in% c("female", "male")] <- NA_character_
    z[, sex := factor(
      fifelse(sex_value == "female", "Female",
              fifelse(sex_value == "male", "Male", NA_character_))
    )]
  }
  z
}

s32v1_model_formula <- function(covariates, response = TRUE) {
  rhs <- paste(c(".exp", covariates), collapse = "+")
  if (isTRUE(response)) {
    as.formula(paste("survival::Surv(fu,event) ~", rhs))
  } else {
    as.formula(paste("~", rhs))
  }
}

s32v1_warning_terms <- function(messages, formula, z) {
  messages <- unique(messages[grepl("before variable", messages, fixed = TRUE)])
  if (!length(messages)) return(character())
  indices <- unique(unlist(lapply(messages, function(message) {
    part <- sub(".*before variable[[:space:]]+", "", message)
    part <- sub(";.*$", "", part)
    value <- suppressWarnings(as.integer(strsplit(gsub("[^0-9,]", "", part), ",")[[1]]))
    value[is.finite(value) & value >= 1L]
  })))
  if (!length(indices)) return(character())
  mm <- tryCatch(stats::model.matrix(formula, data = z), error = function(e) NULL)
  if (is.null(mm)) return(character())
  keep <- colnames(mm) != "(Intercept)"
  assignment <- attr(mm, "assign")[keep]
  labels <- attr(stats::terms(formula), "term.labels")
  indices <- indices[indices <= length(assignment)]
  if (!length(indices)) return(character())
  unique(labels[assignment[indices]])
}

s32v1_reduce_covariates <- function(covariates, bad_terms = character()) {
  if (any(c(".exp", S32V1_FORCED_COVARIATES) %in% bad_terms)) return(NULL)
  bad_position <- match(intersect(bad_terms, covariates), covariates)
  bad_position <- bad_position[is.finite(bad_position)]
  if (length(bad_position)) {
    cut_at <- min(bad_position)
    candidate <- if (cut_at > 1L) covariates[seq_len(cut_at - 1L)] else character()
  } else {
    removable <- which(!covariates %in% S32V1_FORCED_COVARIATES)
    if (!length(removable)) return(NULL)
    candidate <- covariates[seq_len(max(removable) - 1L)]
  }
  if (!all(S32V1_FORCED_COVARIATES %in% candidate)) return(NULL)
  candidate
}

s32v1_prepare_exposure <- function(z, exposure, type) {
  if (!exposure %in% names(z)) return(list(ok = FALSE, reason = "exposure absent"))
  z <- z[!is.na(get(exposure))]
  if (nrow(z) < 100L) return(list(ok = FALSE, reason = "exposure-complete n below 100"))
  if (type == "binary") {
    if (!all(unique(z[[exposure]]) %in% c(0, 1))) {
      return(list(ok = FALSE, reason = "binary exposure contains values outside 0/1"))
    }
    z[, .exp := as.numeric(get(exposure))]
    support <- z[, .(n = .N, events = sum(event)), by = .exp]
    if (!all(c(0, 1) %in% support$.exp)) {
      return(list(ok = FALSE, reason = "binary exposure is constant"))
    }
    if (any(support$n < S32V1_MIN_CELL) || any(support$events < S32V1_MIN_CELL)) {
      return(list(ok = FALSE, reason = "binary exposure group n/event support below 21"))
    }
  } else {
    x <- suppressWarnings(as.numeric(z[[exposure]]))
    sd_x <- sd(x, na.rm = TRUE)
    if (!is.finite(sd_x) || sd_x <= 0) {
      return(list(ok = FALSE, reason = "continuous exposure is constant/non-finite"))
    }
    z[, .exp := (as.numeric(get(exposure)) - mean(as.numeric(get(exposure)), na.rm = TRUE)) / sd_x]
  }
  list(ok = TRUE, data = z)
}

s32v1_fit_cox <- function(frame, endpoint, exposure, type) {
  if (is.null(frame) || nrow(frame) < 100L || sum(frame$event) < S32V1_MIN_CELL) {
    return(list(ok = FALSE, reason = "at-risk n/events below 100/21"))
  }
  prepared <- s32v1_prepare_exposure(copy(frame), exposure, type)
  if (!isTRUE(prepared$ok)) return(prepared)
  z_source <- s32v1_prepare_model_core(prepared$data)
  picked <- s32v1_pick_covariates(z_source, sum(z_source$event), reserved_df = 1L)
  if (!isTRUE(picked$ok)) return(picked)
  covariates <- picked$covariates
  dropped_covariates <- character()
  repeat {
    needed <- c("fu", "event", ".exp", covariates)
    z <- copy(z_source[complete.cases(z_source[, ..needed])])
    for (name in covariates) {
      if (is.factor(z[[name]])) set(z, j = name, value = droplevels(z[[name]]))
    }
    support_ok <- nrow(z) >= 100L && sum(z$event) >= S32V1_MIN_CELL
    if (support_ok && type == "binary") {
      support <- z[, .(n = .N, events = sum(event)), by = .exp]
      support_ok <- nrow(support) == 2L &&
        all(support$n >= S32V1_MIN_CELL) && all(support$events >= S32V1_MIN_CELL)
    }
    if (!support_ok) {
      reduced <- s32v1_reduce_covariates(covariates)
      if (is.null(reduced)) {
        return(list(ok = FALSE, reason = "complete-case support below frozen 100/21 gates"))
      }
      dropped_covariates <- unique(c(dropped_covariates, setdiff(covariates, reduced)))
      covariates <- reduced
      next
    }
    formula <- s32v1_model_formula(covariates, response = TRUE)
    fit_warning <- character(); fit_error <- NULL
    fit <- tryCatch(
      withCallingHandlers(
        survival::coxph(formula, data = z, ties = "efron", x = TRUE, y = TRUE),
        warning = function(w) {
          fit_warning <<- c(fit_warning, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        fit_error <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(fit) && !length(fit_warning)) break
    bad_terms <- if (length(fit_warning)) {
      s32v1_warning_terms(fit_warning, formula, z)
    } else character()
    reduced <- s32v1_reduce_covariates(covariates, bad_terms)
    if (is.null(reduced)) {
      detail <- if (length(fit_warning)) paste(unique(fit_warning), collapse = " | ") else fit_error
      return(list(ok = FALSE, reason = paste0("Cox forced-core fit failed: ", detail)))
    }
    dropped_covariates <- unique(c(dropped_covariates, setdiff(covariates, reduced)))
    covariates <- reduced
  }
  beta <- unname(coef(fit)[".exp"]); se <- sqrt(diag(vcov(fit)))[".exp"]
  if (!is.finite(beta) || !is.finite(se) || se <= 0 || abs(beta) > 5) {
    return(list(ok = FALSE, reason = "Cox exposure coefficient absent/non-finite/unstable"))
  }
  ph_warning <- character()
  ph <- tryCatch(
    withCallingHandlers(
      survival::cox.zph(fit, terms = TRUE),
      warning = function(w) {
        ph_warning <<- c(ph_warning, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  if (length(ph_warning)) {
    return(list(
      ok = FALSE,
      reason = paste0("Cox PH diagnostic warning: ", paste(unique(ph_warning), collapse = " | "))
    ))
  }
  ph_p <- NA_real_
  if (!is.null(ph)) {
    ph_tab <- as.data.frame(ph$table)
    row <- grep("^\\.exp$", rownames(ph_tab))
    if (length(row)) ph_p <- ph_tab[row[1], "p"]
  }
  if (type == "binary") {
    exposed_n <- sum(z$.exp == 1); unexposed_n <- sum(z$.exp == 0)
    exposed_events <- sum(z$event[z$.exp == 1]); unexposed_events <- sum(z$event[z$.exp == 0])
    prevalence <- mean(z$.exp == 1)
  } else {
    exposed_n <- unexposed_n <- exposed_events <- unexposed_events <- NA_integer_
    prevalence <- NA_real_
  }
  row <- data.table(
    endpoint = endpoint, exposure = exposure, exposure_type = type,
    contrast = if (type == "binary") "1_vs_0" else "per_SD",
    n = nrow(z), events = sum(z$event), exposed_n = exposed_n,
    unexposed_n = unexposed_n, exposed_events = exposed_events,
    unexposed_events = unexposed_events, exposure_prevalence = prevalence,
    log_hr = beta, se_log_hr = unname(se), hr = exp(beta),
    lcl = exp(beta - 1.96 * se), ucl = exp(beta + 1.96 * se),
    p = 2 * pnorm(-abs(beta / se)), ph_p = ph_p,
    covariates = paste(covariates, collapse = ";"),
    covariate_df = sum(vapply(covariates, function(name) {
      s32v1_covariate_df(z[[name]])
    }, integer(1))),
    reduced_adjustment = picked$reduced || length(dropped_covariates) > 0L,
    stability_dropped = paste(dropped_covariates, collapse = ";"),
    model_patch = S32V1_MODEL_PATCH, version = S32V1_VERSION
  )
  list(ok = TRUE, row = row, data = z, covariates = covariates)
}

s32v1_fit_fine_gray <- function(cox_result) {
  if (!isTRUE(cox_result$ok)) return(list(ok = FALSE, reason = "paired Cox fit absent"))
  z <- copy(cox_result$data)
  covariates <- cox_result$covariates
  dropped_covariates <- character()
  repeat {
    formula <- s32v1_model_formula(covariates, response = FALSE)
    mm <- tryCatch(stats::model.matrix(formula, data = z), error = function(e) NULL)
    if (!is.null(mm)) mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
    if (is.null(mm)) {
      reduced <- s32v1_reduce_covariates(covariates)
      if (is.null(reduced)) return(list(ok = FALSE, reason = "Fine-Gray forced-core matrix failed"))
      dropped_covariates <- unique(c(dropped_covariates, setdiff(covariates, reduced)))
      covariates <- reduced
      next
    }
    keep <- complete.cases(mm, z$fu, z$fstatus)
    mm <- mm[keep, , drop = FALSE]; zz <- z[keep]
    if (nrow(zz) < 100L || sum(zz$fstatus == 1L) < S32V1_MIN_CELL) {
      return(list(ok = FALSE, reason = "Fine-Gray complete-case support below 100/21"))
    }
    fit_warning <- character(); fit_error <- NULL
    fit <- tryCatch(
      withCallingHandlers(
        cmprsk::crr(
          ftime = zz$fu, fstatus = zz$fstatus, cov1 = mm,
          failcode = 1, cencode = 0, maxiter = 50
        ),
        warning = function(w) {
          fit_warning <<- c(fit_warning, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        fit_error <<- conditionMessage(e)
        NULL
      }
    )
    converged <- !is.null(fit) && isTRUE(fit$converged) && ".exp" %in% names(fit$coef)
    coefficient_ok <- FALSE
    if (converged && !length(fit_warning)) {
      exposure_index <- match(".exp", names(fit$coef))
      variance_diagonal <- tryCatch(diag(fit$var), error = function(e) numeric())
      beta_candidate <- unname(fit$coef[exposure_index])
      se_candidate <- if (length(variance_diagonal) >= exposure_index) {
        sqrt(unname(variance_diagonal[exposure_index]))
      } else NA_real_
      coefficient_ok <- is.finite(beta_candidate) && is.finite(se_candidate) &&
        se_candidate > 0 && abs(beta_candidate) <= 5
      if (coefficient_ok) {
        beta <- beta_candidate
        se <- se_candidate
        break
      }
      fit_error <- "exposure coefficient non-finite/unstable"
    }
    bad_terms <- if (length(fit_warning)) {
      s32v1_warning_terms(fit_warning, formula, z)
    } else character()
    reduced <- s32v1_reduce_covariates(covariates, bad_terms)
    if (is.null(reduced)) {
      detail <- if (length(fit_warning)) paste(unique(fit_warning), collapse = " | ") else fit_error
      if (is.null(detail) || !nzchar(detail)) detail <- "non-convergence"
      return(list(ok = FALSE, reason = paste0("Fine-Gray forced-core fit failed: ", detail)))
    }
    dropped_covariates <- unique(c(dropped_covariates, setdiff(covariates, reduced)))
    covariates <- reduced
  }
  base <- copy(cox_result$row)
  prior_dropped <- as.character(base$stability_dropped[1])
  prior_dropped <- if (nzchar(prior_dropped)) strsplit(prior_dropped, ";", fixed = TRUE)[[1]] else character()
  all_dropped <- unique(c(prior_dropped, dropped_covariates))
  base[, `:=`(
    log_hr = beta, se_log_hr = unname(se), hr = exp(beta),
    lcl = exp(beta - 1.96 * se), ucl = exp(beta + 1.96 * se),
    p = 2 * pnorm(-abs(beta / se)), ph_p = NA_real_,
    covariates = paste(covariates, collapse = ";"), covariate_df = ncol(mm) - 1L,
    reduced_adjustment = reduced_adjustment | length(dropped_covariates) > 0L,
    stability_dropped = paste(all_dropped, collapse = ";"),
    model_patch = S32V1_MODEL_PATCH
  )]
  list(ok = TRUE, row = base)
}

D <- copy(as.data.table(CHECKPOINT$cohort))
EVENTS <- copy(as.data.table(CHECKPOINT$events))
PRIMARY <- list(); NEGATIVE <- list(); FINEGRAY <- list(); FAILURES <- list()

for (endpoint in CHECKPOINT$primary_outcomes) {
  frame <- s32v1_make_frame(D, EVENTS, endpoint)
  for (i in seq_len(nrow(S32V1_EXPOSURE_PLAN))) {
    exposure <- S32V1_EXPOSURE_PLAN$exposure[i]
    type <- S32V1_EXPOSURE_PLAN$type[i]
    fit <- s32v1_fit_cox(frame, endpoint, exposure, type)
    if (isTRUE(fit$ok)) {
      fit$row[, exposure_group := S32V1_EXPOSURE_PLAN$exposure_group[i]]
      PRIMARY[[length(PRIMARY) + 1L]] <- fit$row
      fg <- s32v1_fit_fine_gray(fit)
      if (isTRUE(fg$ok)) {
        fg$row[, exposure_group := S32V1_EXPOSURE_PLAN$exposure_group[i]]
        FINEGRAY[[length(FINEGRAY) + 1L]] <- fg$row
      } else {
        FAILURES[[length(FAILURES) + 1L]] <- data.table(
          analysis = "fine_gray", endpoint = endpoint, exposure = exposure, reason = fg$reason
        )
      }
    } else {
      FAILURES[[length(FAILURES) + 1L]] <- data.table(
        analysis = "cause_specific_cox", endpoint = endpoint,
        exposure = exposure, reason = fit$reason
      )
    }
  }
}

for (endpoint in CHECKPOINT$negative_controls) {
  frame <- s32v1_make_frame(D, EVENTS, endpoint)
  for (i in seq_len(nrow(S32V1_EXPOSURE_PLAN))) {
    exposure <- S32V1_EXPOSURE_PLAN$exposure[i]
    type <- S32V1_EXPOSURE_PLAN$type[i]
    fit <- s32v1_fit_cox(frame, endpoint, exposure, type)
    if (isTRUE(fit$ok)) {
      fit$row[, exposure_group := S32V1_EXPOSURE_PLAN$exposure_group[i]]
      NEGATIVE[[length(NEGATIVE) + 1L]] <- fit$row
    } else {
      FAILURES[[length(FAILURES) + 1L]] <- data.table(
        analysis = "negative_control", endpoint = endpoint,
        exposure = exposure, reason = fit$reason
      )
    }
  }
}

PRIMARY <- rbindlist(PRIMARY, fill = TRUE)
NEGATIVE <- rbindlist(NEGATIVE, fill = TRUE)
FINEGRAY <- rbindlist(FINEGRAY, fill = TRUE)
FAILURES <- rbindlist(FAILURES, fill = TRUE)
if (!nrow(PRIMARY)) stop("No primary Cox model passed the frozen support/stability gates.", call. = FALSE)
if (!nrow(FINEGRAY)) FINEGRAY <- copy(PRIMARY[0])
if (!nrow(NEGATIVE)) NEGATIVE <- copy(PRIMARY[0])

if (nrow(NEGATIVE)) {
  EMPIRICAL_NULL <- NEGATIVE[, {
    k <- .N
    w <- 1 / (se_log_hr^2)
    mu <- sum(w * log_hr) / sum(w)
    q <- sum(w * (log_hr - mu)^2)
    denom <- sum(w) - sum(w^2) / sum(w)
    tau2 <- if (k >= 2L && is.finite(denom) && denom > 0) max(0, (q - (k - 1)) / denom) else NA_real_
    list(
      negative_controls_used = k, null_mu = mu,
      null_sigma = if (is.finite(tau2)) sqrt(tau2) else NA_real_,
      calibration_available = k >= 5L && is.finite(mu) && is.finite(tau2)
    )
  }, by = exposure]
} else {
  EMPIRICAL_NULL <- data.table(
    exposure = character(), negative_controls_used = integer(), null_mu = numeric(),
    null_sigma = numeric(), calibration_available = logical()
  )
}

PRIMARY <- merge(PRIMARY, EMPIRICAL_NULL, by = "exposure", all.x = TRUE)
PRIMARY[is.na(calibration_available), calibration_available := FALSE]
S32V1_PRIMARY_FAMILY_SIZE <- length(CHECKPOINT$primary_outcomes) * nrow(S32V1_EXPOSURE_PLAN)
PRIMARY[, `:=`(
  q_raw = p.adjust(p, method = "BH", n = S32V1_PRIMARY_FAMILY_SIZE),
  multiplicity_family_n = S32V1_PRIMARY_FAMILY_SIZE
)]
PRIMARY[, `:=`(
  se_log_hr_cal = fifelse(
    calibration_available, sqrt(se_log_hr^2 + null_sigma^2), NA_real_
  ),
  log_hr_cal = fifelse(calibration_available, log_hr - null_mu, NA_real_)
)]
PRIMARY[, `:=`(
  hr_cal = exp(log_hr_cal),
  lcl_cal = exp(log_hr_cal - 1.96 * se_log_hr_cal),
  ucl_cal = exp(log_hr_cal + 1.96 * se_log_hr_cal),
  p_cal = 2 * pnorm(-abs(log_hr_cal / se_log_hr_cal))
)]
PRIMARY[, q_cal := NA_real_]
PRIMARY[is.finite(p_cal), q_cal := p.adjust(
  p_cal, method = "BH", n = S32V1_PRIMARY_FAMILY_SIZE
)]

PAF <- PRIMARY[
  exposure_type == "binary" & endpoint != "falls" & calibration_available &
    is.finite(q_cal) & q_cal < .05 & is.finite(ph_p) & ph_p >= .05 &
    is.finite(exposure_prevalence) & exposure_prevalence > 0 & exposure_prevalence < 1
]
paf_fun <- function(prevalence, rr) {
  prevalence * (rr - 1) / (1 + prevalence * (rr - 1))
}
if (nrow(PAF)) {
  PAF[, `:=`(
    paf = paf_fun(exposure_prevalence, hr_cal),
    paf_lcl = paf_fun(exposure_prevalence, lcl_cal),
    paf_ucl = paf_fun(exposure_prevalence, ucl_cal),
    interpretation = "association_based_not_preventable_fraction"
  )]
} else {
  PAF <- data.table(
    endpoint = character(), exposure = character(), exposure_group = character(),
    n = integer(), events = integer(), exposed_n = integer(), exposure_prevalence = numeric(),
    hr_cal = numeric(), lcl_cal = numeric(), ucl_cal = numeric(), q_cal = numeric(),
    ph_p = numeric(), paf = numeric(), paf_lcl = numeric(), paf_ucl = numeric(),
    interpretation = character()
  )
}

POSITIVE_GATE <- PRIMARY[endpoint == "major_fracture" & exposure == "osteoporosis_baseline"]
if (nrow(POSITIVE_GATE)) {
  POSITIVE_GATE[, `:=`(
    gate = "osteoporosis_baseline_predicts_incident_major_fracture",
    direction_pass = hr > 1,
    note = "Plausibility diagnostic only; failure limits interpretation but does not validate causality."
  )]
} else {
  POSITIVE_GATE <- data.table(
    gate = "osteoporosis_baseline_predicts_incident_major_fracture",
    direction_pass = NA, note = "Model not estimable under frozen support/stability gates."
  )
}

FB_DESCRIPTION <- data.table()
FB <- copy(as.data.table(CHECKPOINT$fitbit))
if (nrow(FB)) {
  metrics <- setdiff(names(FB)[vapply(FB, is.numeric, logical(1))], "person_id")
  FB_DESCRIPTION <- rbindlist(lapply(metrics, function(metric) {
    x <- suppressWarnings(as.numeric(FB[[metric]])); x <- x[is.finite(x)]
    data.table(
      metric = metric, n = length(x), mean = if (length(x)) mean(x) else NA_real_,
      sd = if (length(x) > 1L) sd(x) else NA_real_,
      median = if (length(x)) median(x) else NA_real_,
      q25 = if (length(x)) unname(quantile(x, .25)) else NA_real_,
      q75 = if (length(x)) unname(quantile(x, .75)) else NA_real_,
      role = "descriptive_only_no_outcome_model"
    )
  }), fill = TRUE)
}
if (!nrow(FB_DESCRIPTION)) {
  FB_DESCRIPTION <- data.table(
    metric = "fitbit_not_available", n = 0L, mean = NA_real_, sd = NA_real_,
    median = NA_real_, q25 = NA_real_, q75 = NA_real_,
    role = "descriptive_only_no_outcome_model"
  )
}

s32v1_write_safe(
  PRIMARY, "s32v1_10_primary_cause_specific_cox.csv",
  count_cols = c("n", "events", "exposed_n", "unexposed_n", "exposed_events", "unexposed_events"),
  support_cols = c("n", "events"),
  complementary_sets = list(c("exposed_n", "unexposed_n"),
                            c("exposed_events", "unexposed_events"))
)
s32v1_write_safe(
  FINEGRAY, "s32v1_11_competing_risk_fine_gray.csv",
  count_cols = c("n", "events", "exposed_n", "unexposed_n", "exposed_events", "unexposed_events"),
  support_cols = c("n", "events"),
  complementary_sets = list(c("exposed_n", "unexposed_n"),
                            c("exposed_events", "unexposed_events"))
)
s32v1_write_safe(
  NEGATIVE, "s32v1_12_negative_controls.csv",
  count_cols = c("n", "events", "exposed_n", "unexposed_n", "exposed_events", "unexposed_events"),
  support_cols = c("n", "events"),
  complementary_sets = list(c("exposed_n", "unexposed_n"),
                            c("exposed_events", "unexposed_events"))
)
s32v1_write_safe(EMPIRICAL_NULL, "s32v1_13_empirical_null.csv", count_cols = character())
s32v1_write_safe(
  PAF, "s32v1_14_attributable_fractions.csv",
  count_cols = c("n", "events", "exposed_n"), support_cols = c("n", "events")
)
s32v1_write_safe(
  POSITIVE_GATE, "s32v1_15_positive_control_gate.csv",
  count_cols = intersect(c("n", "events", "exposed_n", "unexposed_n",
                           "exposed_events", "unexposed_events"), names(POSITIVE_GATE)),
  support_cols = intersect(c("n", "events"), names(POSITIVE_GATE))
)
s32v1_write_safe(
  FB_DESCRIPTION, "s32v1_16_fitbit_description.csv",
  count_cols = "n", support_cols = "n"
)
if (nrow(FAILURES)) {
  s32v1_write_safe(FAILURES, "s32v1_17_model_failures.csv", count_cols = character())
} else {
  s32v1_write_safe(
    data.table(analysis = "all", endpoint = "all", exposure = "all", reason = "NO_FAILURES"),
    "s32v1_17_model_failures.csv", count_cols = character()
  )
}

result_dir <- file.path(S32V1_HOME, "results")
members <- list.files(
  result_dir, pattern = "^s32v1_(0[1-8]|1[0-7])_.+[.]csv$", full.names = TRUE
)
if (!length(members)) stop("No disclosure-safe S32 result CSVs found for packaging.", call. = FALSE)
members <- normalizePath(members, winslash = "/", mustWork = TRUE)
manifest <- data.frame(
  file = basename(members), bytes = file.info(members)$size,
  md5 = unname(tools::md5sum(members)), stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(result_dir, "s32v1_98_manifest.csv"), row.names = FALSE)
readme <- c(
  "EPI-BONE All of Us S32 standalone v1.0 aggregate package",
  paste0("Version: ", S32V1_VERSION),
  paste0("Model patch: ", S32V1_MODEL_PATCH),
  paste0("Phenotype: ", S32V1_PHENOTYPE),
  "Counts 1-20 inclusive are suppressed; zero and counts >=21 may be reported.",
  "Cause-specific Cox estimates are observational associations.",
  "Fine-Gray estimates treat death as a competing event.",
  "Empirical-null calibration requires at least five estimable negative controls per exposure.",
  "Raw and calibrated BH adjustments use the full frozen 50-cell primary family, including unsupported cells.",
  "Numerical stability reduction keeps age, binary-known sex, and index year forced and trims only the lower-priority adjustment tail.",
  "PAFs are emitted only after calibration, BH, and PH gates and are not preventable fractions.",
  "Fitbit results are descriptive only. No participant-level data or model object is included."
)
writeLines(readme, file.path(result_dir, "s32v1_99_README.txt"))

members <- normalizePath(
  c(members, file.path(result_dir, "s32v1_98_manifest.csv"),
    file.path(result_dir, "s32v1_99_README.txt")),
  winslash = "/", mustWork = TRUE
)
zip_path <- file.path(result_dir, "EPIBONE_AOU_S32_STANDALONE_V1_SAFE_AGGREGATES.zip")
if (file.exists(zip_path) && !file.remove(zip_path)) {
  stop("Could not replace the prior S32 ZIP candidate.", call. = FALSE)
}
zip_cmd <- Sys.which("zip")
if (!nzchar(zip_cmd)) stop("System zip command is unavailable.", call. = FALSE)
zip_output <- suppressWarnings(system2(
  zip_cmd,
  args = c("-q", "-j", shQuote(zip_path), shQuote(members)),
  stdout = TRUE, stderr = TRUE
))
zip_status <- attr(zip_output, "status")
if (is.null(zip_status)) zip_status <- 0L
if (zip_status != 0L || !file.exists(zip_path)) {
  stop(
    "Final S32 ZIP creation failed", if (length(zip_output)) paste0(": ", paste(zip_output, collapse = " | ")),
    call. = FALSE
  )
}
listed <- utils::unzip(zip_path, list = TRUE)$Name
if (!setequal(listed, basename(members))) stop("Final S32 ZIP member audit failed.", call. = FALSE)
if (any(grepl("rds|person|participant|private", listed, ignore.case = TRUE))) {
  stop("Final S32 ZIP contains a forbidden member name.", call. = FALSE)
}
cat("\nS32 v1 COMPLETE. Review before egress: ", normalizePath(zip_path), "\n", sep = "")
