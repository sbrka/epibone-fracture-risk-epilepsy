# EPI-BONE All of Us S34 v1.0: multimorbidity gradient on the frozen S32 checkpoint.
# This script performs no BigQuery extraction.

S34V1_VERSION <- "EPIBONE_AOU_S34_MULTIMORBIDITY_v1.0_2026-08-25"
S34V1_MODEL_PATCH <- "S34V1_INITIAL_LOCKED"
S34V1_FAMILY_N <- 65L
S34V1_COMPONENTS <- c(
  "diabetes", "chronic_kidney_disease", "cancer", "cardiovascular_disease",
  "cerebrovascular_disease", "depression", "alcohol_disorder", "tobacco", "obesity"
)
S34V1_COVARIATE_PRIORITY <- c(
  "age_index", "sex", "index_year", "log_prior_visits", "race", "ethnicity"
)
S34V1_PRIMARY_ENDPOINTS <- c(
  "major_fracture", "osteoporosis", "osteopenia", "vitamin_d_deficiency", "falls"
)
if (!identical(as.character(CHECKPOINT$primary_outcomes), S34V1_PRIMARY_ENDPOINTS)) {
  stop("The checkpoint primary endpoint contract differs from the frozen five-endpoint S34 family.",
       call. = FALSE)
}

# Load only the frozen S32 modelling helpers. Stop before the original S32 data loop.
s32_model_file <- file.path(S34V1_HOME, "s32v1_stage_b_models.R")
s32_expr <- parse(s32_model_file, encoding = "UTF-8")
is_data_start <- vapply(s32_expr, function(expr) {
  is.call(expr) && identical(as.character(expr[[1]]), "<-") &&
    length(expr) >= 3L && identical(as.character(expr[[2]]), "D")
}, logical(1))
data_start <- which(is_data_start)[1]
if (!is.finite(data_start) || data_start <= 1L) {
  stop("Could not isolate the frozen S32 modelling helpers.", call. = FALSE)
}
for (i in seq_len(data_start - 1L)) eval(s32_expr[[i]], envir = .GlobalEnv)
rm(s32_expr, is_data_start, data_start, i)
S32V1_MODEL_PATCH <- S34V1_MODEL_PATCH

D <- copy(as.data.table(CHECKPOINT$cohort))
EVENTS <- copy(as.data.table(CHECKPOINT$events))
if (!all(S34V1_COMPONENTS %in% names(D))) {
  stop(
    "The checkpoint lacks frozen multimorbidity components: ",
    paste(setdiff(S34V1_COMPONENTS, names(D)), collapse = ", "), call. = FALSE
  )
}

D[, multimorbidity_count := rowSums(.SD, na.rm = FALSE), .SDcols = S34V1_COMPONENTS]
if (any(!is.finite(D$multimorbidity_count))) {
  stop("Multimorbidity count contains missing/non-finite values.", call. = FALSE)
}

s34v1_collapse_categories <- function(x, minimum = S32V1_MIN_CELL) {
  maximum <- length(S34V1_COMPONENTS)
  groups <- list(`0` = 0L, `1` = 1L, `2` = 2L, `3plus` = 3:maximum)
  group_n <- function(values) sum(x %in% values)
  repeat {
    counts <- vapply(groups, group_n, integer(1))
    sparse <- which(counts > 0L & counts < minimum)
    if (!length(sparse) || length(groups) <= 1L) break
    j <- sparse[1]
    if (j < length(groups)) {
      groups[[j + 1L]] <- sort(unique(c(groups[[j]], groups[[j + 1L]])))
      groups[[j]] <- NULL
    } else {
      groups[[j - 1L]] <- sort(unique(c(groups[[j - 1L]], groups[[j]])))
      groups[[j]] <- NULL
    }
  }
  label_for <- function(values) {
    low <- min(values); high <- max(values)
    if (high == maximum) paste0(low, "plus") else if (low == high) as.character(low) else paste0(low, "to", high)
  }
  labels <- vapply(groups, label_for, character(1))
  out <- rep(NA_character_, length(x))
  mapping <- vector("list", length(groups))
  for (j in seq_along(groups)) {
    out[x %in% groups[[j]]] <- labels[j]
    mapping[[j]] <- data.table(
      category = labels[j], raw_values = paste(groups[[j]], collapse = ";"),
      n = sum(x %in% groups[[j]])
    )
  }
  list(
    factor = factor(out, levels = labels),
    audit = rbindlist(mapping, fill = TRUE)
  )
}

collapsed <- s34v1_collapse_categories(D$multimorbidity_count)
D[, multimorbidity_category := collapsed$factor]
CATEGORY_AUDIT <- collapsed$audit
rm(collapsed)
if (nlevels(D$multimorbidity_category) < 2L) {
  stop("Multimorbidity category collapse left fewer than two categories.", call. = FALSE)
}
s32v1_write_safe(
  CATEGORY_AUDIT, "s34v1_01_category_audit.csv", count_cols = "n", support_cols = "n"
)

s34v1_with_raw_continuous <- function(expr) {
  old_prepare <- s32v1_prepare_exposure
  on.exit(assign("s32v1_prepare_exposure", old_prepare, envir = .GlobalEnv), add = TRUE)
  assign("s32v1_prepare_exposure", function(z, exposure, type) {
    if (!exposure %in% names(z)) return(list(ok = FALSE, reason = "exposure absent"))
    z <- z[!is.na(get(exposure))]
    if (nrow(z) < 100L) return(list(ok = FALSE, reason = "exposure-complete n below 100"))
    x <- suppressWarnings(as.numeric(z[[exposure]]))
    if (any(!is.finite(x)) || !is.finite(sd(x)) || sd(x) <= 0) {
      return(list(ok = FALSE, reason = "continuous exposure is constant/non-finite"))
    }
    z[, .exp := as.numeric(get(exposure))]
    list(ok = TRUE, data = z)
  }, envir = .GlobalEnv)
  force(expr)
}

s34v1_fit_continuous <- function(frame, endpoint) {
  old_priority <- S32V1_FIXED_COVARIATE_PRIORITY
  on.exit(assign("S32V1_FIXED_COVARIATE_PRIORITY", old_priority, envir = .GlobalEnv), add = TRUE)
  assign("S32V1_FIXED_COVARIATE_PRIORITY", S34V1_COVARIATE_PRIORITY, envir = .GlobalEnv)
  fit <- s34v1_with_raw_continuous(
    s32v1_fit_cox(frame, endpoint, "multimorbidity_count", "continuous")
  )
  if (isTRUE(fit$ok)) {
    fit$row[, `:=`(
      contrast = "per_additional_condition", exposure_type = "continuous_raw",
      exposure_group = "multimorbidity", model_patch = S34V1_MODEL_PATCH,
      version = S34V1_VERSION
    )]
  }
  fit
}

s34v1_fit_binary_positive <- function(frame, endpoint) {
  old_priority <- S32V1_FIXED_COVARIATE_PRIORITY
  on.exit(assign("S32V1_FIXED_COVARIATE_PRIORITY", old_priority, envir = .GlobalEnv), add = TRUE)
  assign("S32V1_FIXED_COVARIATE_PRIORITY", old_priority, envir = .GlobalEnv)
  fit <- s32v1_fit_cox(frame, endpoint, "osteoporosis_baseline", "binary")
  if (isTRUE(fit$ok)) fit$row[, `:=`(model_patch = S34V1_MODEL_PATCH, version = S34V1_VERSION)]
  fit
}

s34v1_category_formula <- function(covariates, response = TRUE) {
  rhs <- paste(c(".exp", covariates), collapse = "+")
  if (isTRUE(response)) {
    as.formula(paste("survival::Surv(fu,event) ~", rhs))
  } else as.formula(paste("~", rhs))
}

s34v1_fit_category_cox <- function(frame, endpoint) {
  if (is.null(frame) || nrow(frame) < 100L || sum(frame$event) < S32V1_MIN_CELL) {
    return(list(ok = FALSE, reason = "at-risk n/events below 100/21"))
  }
  z_source <- s32v1_prepare_model_core(copy(as.data.table(frame)))
  z_source[, .exp := droplevels(multimorbidity_category)]
  support <- z_source[, .(n = .N, events = sum(event)), by = .exp]
  if (nrow(support) < 2L || any(support$n < S32V1_MIN_CELL) ||
      any(support$events < S32V1_MIN_CELL)) {
    return(list(ok = FALSE, reason = "categorical group n/event support below 21"))
  }
  exposure_df <- nlevels(z_source$.exp) - 1L
  old_priority <- S32V1_FIXED_COVARIATE_PRIORITY
  on.exit(assign("S32V1_FIXED_COVARIATE_PRIORITY", old_priority, envir = .GlobalEnv), add = TRUE)
  assign("S32V1_FIXED_COVARIATE_PRIORITY", S34V1_COVARIATE_PRIORITY, envir = .GlobalEnv)
  picked <- s32v1_pick_covariates(z_source, sum(z_source$event), reserved_df = exposure_df)
  if (!isTRUE(picked$ok)) return(picked)
  covariates <- picked$covariates
  dropped_covariates <- character()
  repeat {
    needed <- c("fu", "event", ".exp", covariates)
    z <- copy(z_source[complete.cases(z_source[, ..needed])])
    z[, .exp := droplevels(.exp)]
    for (name in covariates) {
      if (is.factor(z[[name]])) set(z, j = name, value = droplevels(z[[name]]))
    }
    support <- z[, .(n = .N, events = sum(event)), by = .exp]
    support_ok <- nrow(z) >= 100L && sum(z$event) >= S32V1_MIN_CELL &&
      nrow(support) >= 2L && all(support$n >= S32V1_MIN_CELL) &&
      all(support$events >= S32V1_MIN_CELL)
    if (!support_ok) {
      reduced <- s32v1_reduce_covariates(covariates)
      if (is.null(reduced)) {
        return(list(ok = FALSE, reason = "categorical complete-case support below frozen gates"))
      }
      dropped_covariates <- unique(c(dropped_covariates, setdiff(covariates, reduced)))
      covariates <- reduced
      next
    }
    formula <- s34v1_category_formula(covariates, response = TRUE)
    fit_warning <- character(); fit_error <- NULL
    fit <- tryCatch(
      withCallingHandlers(
        survival::coxph(formula, data = z, ties = "efron", x = TRUE, y = TRUE),
        warning = function(w) {
          fit_warning <<- c(fit_warning, conditionMessage(w)); invokeRestart("muffleWarning")
        }
      ),
      error = function(e) { fit_error <<- conditionMessage(e); NULL }
    )
    exp_terms <- if (!is.null(fit)) grep("^\\.exp", names(coef(fit)), value = TRUE) else character()
    beta <- if (length(exp_terms)) unname(coef(fit)[exp_terms]) else numeric()
    se <- if (length(exp_terms)) sqrt(diag(vcov(fit)))[exp_terms] else numeric()
    coefficient_ok <- length(exp_terms) == exposure_df && all(is.finite(beta)) &&
      all(is.finite(se)) && all(se > 0) && all(abs(beta) <= 5)
    if (!is.null(fit) && !length(fit_warning) && coefficient_ok) break
    bad_terms <- if (length(fit_warning)) s32v1_warning_terms(fit_warning, formula, z) else character()
    reduced <- s32v1_reduce_covariates(covariates, bad_terms)
    if (is.null(reduced)) {
      detail <- if (length(fit_warning)) paste(unique(fit_warning), collapse = " | ") else fit_error
      if (is.null(detail) || !nzchar(detail)) detail <- "categorical coefficient instability"
      return(list(ok = FALSE, reason = paste0("categorical Cox forced-core fit failed: ", detail)))
    }
    dropped_covariates <- unique(c(dropped_covariates, setdiff(covariates, reduced)))
    covariates <- reduced
  }
  v <- vcov(fit)[exp_terms, exp_terms, drop = FALSE]
  global_stat <- tryCatch(drop(t(beta) %*% solve(v, beta)), error = function(e) NA_real_)
  global_p <- if (is.finite(global_stat)) pchisq(global_stat, df = length(beta), lower.tail = FALSE) else NA_real_
  ph_p <- NA_real_
  ph <- tryCatch(survival::cox.zph(fit, terms = TRUE), error = function(e) NULL)
  if (!is.null(ph)) {
    tab <- as.data.frame(ph$table)
    hit <- grep("^\\.exp$", rownames(tab))
    if (length(hit)) ph_p <- tab[hit[1], "p"]
  }
  nonref <- levels(z$.exp)[-1L]
  rows <- rbindlist(lapply(seq_along(exp_terms), function(j) {
    label <- nonref[j]
    data.table(
      endpoint = endpoint, exposure = "multimorbidity_category",
      exposure_key = paste0("multimorbidity_category_", label, "_vs_", levels(z$.exp)[1]),
      exposure_type = "ordered_categorical", contrast = paste0(label, "_vs_", levels(z$.exp)[1]),
      model = "cause_specific_cox", n = nrow(z), events = sum(z$event),
      category_n = support[.exp == label, n], category_events = support[.exp == label, events],
      reference_n = support[.exp == levels(z$.exp)[1], n],
      reference_events = support[.exp == levels(z$.exp)[1], events],
      log_hr = beta[j], se_log_hr = unname(se[j]), hr = exp(beta[j]),
      lcl = exp(beta[j] - 1.96 * se[j]), ucl = exp(beta[j] + 1.96 * se[j]),
      p = 2 * pnorm(-abs(beta[j] / se[j])), global_p = global_p, ph_global_p = ph_p,
      covariates = paste(covariates, collapse = ";"),
      covariate_df = sum(vapply(covariates, function(name) s32v1_covariate_df(z[[name]]), integer(1))),
      reduced_adjustment = picked$reduced || length(dropped_covariates) > 0L,
      stability_dropped = paste(dropped_covariates, collapse = ";"),
      model_patch = S34V1_MODEL_PATCH, version = S34V1_VERSION
    )
  }), fill = TRUE)
  list(ok = TRUE, rows = rows, data = z, covariates = covariates, support = support)
}

s34v1_fit_category_fg <- function(cox_result) {
  if (!isTRUE(cox_result$ok)) return(list(ok = FALSE, reason = "paired categorical Cox fit absent"))
  z <- copy(cox_result$data)
  covariates <- cox_result$covariates
  formula <- s34v1_category_formula(covariates, response = FALSE)
  mm <- tryCatch(stats::model.matrix(formula, data = z), error = function(e) NULL)
  if (!is.null(mm)) mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  if (is.null(mm)) return(list(ok = FALSE, reason = "categorical Fine-Gray matrix failed"))
  keep <- complete.cases(mm, z$fu, z$fstatus)
  mm <- mm[keep, , drop = FALSE]; zz <- z[keep]
  exp_terms <- grep("^\\.exp", colnames(mm), value = TRUE)
  fit_warning <- character()
  fit <- tryCatch(
    withCallingHandlers(
      cmprsk::crr(ftime = zz$fu, fstatus = zz$fstatus, cov1 = mm, failcode = 1, cencode = 0, maxiter = 50),
      warning = function(w) {
        fit_warning <<- c(fit_warning, conditionMessage(w)); invokeRestart("muffleWarning")
      }
    ), error = function(e) NULL
  )
  if (is.null(fit) || length(fit_warning) || !isTRUE(fit$converged) ||
      !all(exp_terms %in% names(fit$coef))) {
    return(list(ok = FALSE, reason = "categorical Fine-Gray did not converge cleanly"))
  }
  beta <- unname(fit$coef[exp_terms])
  se <- sqrt(diag(fit$var))[match(exp_terms, names(fit$coef))]
  if (any(!is.finite(beta)) || any(!is.finite(se)) || any(se <= 0) || any(abs(beta) > 5)) {
    return(list(ok = FALSE, reason = "categorical Fine-Gray coefficients unstable"))
  }
  base <- copy(cox_result$rows)
  base[, `:=`(
    model = "fine_gray", log_hr = beta, se_log_hr = unname(se), hr = exp(beta),
    lcl = exp(beta - 1.96 * se), ucl = exp(beta + 1.96 * se),
    p = 2 * pnorm(-abs(beta / se)), global_p = NA_real_, ph_global_p = NA_real_
  )]
  list(ok = TRUE, rows = base)
}

s34v1_empirical_null <- function(negative_rows) {
  if (!nrow(negative_rows)) return(data.table(
    exposure_key = character(), negative_controls_used = integer(), null_mu = numeric(),
    null_sigma = numeric(), calibration_available = logical()
  ))
  negative_rows[, {
    k <- .N; w <- 1 / (se_log_hr^2); mu <- sum(w * log_hr) / sum(w)
    q <- sum(w * (log_hr - mu)^2)
    denom <- sum(w) - sum(w^2) / sum(w)
    tau2 <- if (k >= 2L && is.finite(denom) && denom > 0) max(0, (q - (k - 1)) / denom) else NA_real_
    list(
      negative_controls_used = k, null_mu = mu,
      null_sigma = if (is.finite(tau2)) sqrt(tau2) else NA_real_,
      calibration_available = k >= 5L && is.finite(mu) && is.finite(tau2)
    )
  }, by = exposure_key]
}

s34v1_add_calibration <- function(rows, empirical_null) {
  if (!nrow(rows)) return(rows)
  rows <- merge(rows, empirical_null, by = "exposure_key", all.x = TRUE)
  rows[is.na(calibration_available), calibration_available := FALSE]
  rows[, `:=`(
    se_log_hr_cal = fifelse(calibration_available, sqrt(se_log_hr^2 + null_sigma^2), NA_real_),
    log_hr_cal = fifelse(calibration_available, log_hr - null_mu, NA_real_)
  )]
  rows[, `:=`(
    hr_cal = exp(log_hr_cal), lcl_cal = exp(log_hr_cal - 1.96 * se_log_hr_cal),
    ucl_cal = exp(log_hr_cal + 1.96 * se_log_hr_cal),
    p_cal = 2 * pnorm(-abs(log_hr_cal / se_log_hr_cal))
  )]
  rows
}

CONTINUOUS <- list(); CATEGORICAL <- list(); NEGATIVE <- list(); FAILURES <- list()

for (endpoint in S34V1_PRIMARY_ENDPOINTS) {
  if (identical(endpoint, "falls")) {
    FAILURES[[length(FAILURES) + 1L]] <- data.table(
      analysis = "continuous_and_categorical", endpoint = endpoint,
      exposure = "multimorbidity", reason = "falls descriptive only; 26 events; no adjusted model"
    )
    next
  }
  frame <- s32v1_make_frame(D, EVENTS, endpoint)
  fit <- s34v1_fit_continuous(frame, endpoint)
  if (isTRUE(fit$ok)) {
    fit$row[, `:=`(model = "cause_specific_cox", exposure_key = "multimorbidity_count")]
    CONTINUOUS[[length(CONTINUOUS) + 1L]] <- fit$row
    fg <- s32v1_fit_fine_gray(fit)
    if (isTRUE(fg$ok)) {
      fg$row[, `:=`(
        model = "fine_gray", exposure_key = "multimorbidity_count",
        model_patch = S34V1_MODEL_PATCH, version = S34V1_VERSION
      )]
      CONTINUOUS[[length(CONTINUOUS) + 1L]] <- fg$row
    } else FAILURES[[length(FAILURES) + 1L]] <- data.table(
      analysis = "fine_gray", endpoint = endpoint, exposure = "multimorbidity_count", reason = fg$reason
    )
  } else FAILURES[[length(FAILURES) + 1L]] <- data.table(
    analysis = "cause_specific_cox", endpoint = endpoint,
    exposure = "multimorbidity_count", reason = fit$reason
  )

  cfit <- s34v1_fit_category_cox(frame, endpoint)
  if (isTRUE(cfit$ok)) {
    CATEGORICAL[[length(CATEGORICAL) + 1L]] <- cfit$rows
    cfg <- s34v1_fit_category_fg(cfit)
    if (isTRUE(cfg$ok)) CATEGORICAL[[length(CATEGORICAL) + 1L]] <- cfg$rows else
      FAILURES[[length(FAILURES) + 1L]] <- data.table(
        analysis = "categorical_fine_gray", endpoint = endpoint,
        exposure = "multimorbidity_category", reason = cfg$reason
      )
  } else FAILURES[[length(FAILURES) + 1L]] <- data.table(
    analysis = "categorical_cox", endpoint = endpoint,
    exposure = "multimorbidity_category", reason = cfit$reason
  )
}

for (endpoint in CHECKPOINT$negative_controls) {
  frame <- s32v1_make_frame(D, EVENTS, endpoint)
  fit <- s34v1_fit_continuous(frame, endpoint)
  if (isTRUE(fit$ok)) {
    fit$row[, `:=`(
      model = "cause_specific_cox", exposure_key = "multimorbidity_count",
      exposure_group = "multimorbidity_negative_control"
    )]
    NEGATIVE[[length(NEGATIVE) + 1L]] <- fit$row
  } else FAILURES[[length(FAILURES) + 1L]] <- data.table(
    analysis = "negative_control_continuous", endpoint = endpoint,
    exposure = "multimorbidity_count", reason = fit$reason
  )
  cfit <- s34v1_fit_category_cox(frame, endpoint)
  if (isTRUE(cfit$ok)) {
    cfit$rows[, exposure_group := "multimorbidity_negative_control"]
    NEGATIVE[[length(NEGATIVE) + 1L]] <- cfit$rows
  } else FAILURES[[length(FAILURES) + 1L]] <- data.table(
    analysis = "negative_control_categorical", endpoint = endpoint,
    exposure = "multimorbidity_category", reason = cfit$reason
  )
}

empty_model_rows <- function() data.table(
  model = character(), endpoint = character(), exposure = character(),
  exposure_key = character(), exposure_group = character(), n = integer(), events = integer(),
  log_hr = numeric(), se_log_hr = numeric(), p = numeric(), global_p = numeric()
)
CONTINUOUS <- if (length(CONTINUOUS)) rbindlist(CONTINUOUS, fill = TRUE) else empty_model_rows()
CATEGORICAL <- if (length(CATEGORICAL)) rbindlist(CATEGORICAL, fill = TRUE) else empty_model_rows()
NEGATIVE <- if (length(NEGATIVE)) rbindlist(NEGATIVE, fill = TRUE) else empty_model_rows()
FAILURES <- rbindlist(FAILURES, fill = TRUE)
if (!nrow(FAILURES)) FAILURES <- data.table(
  analysis = "all", endpoint = "all", exposure = "all", reason = "NO_FAILURES"
)

EMPIRICAL_NULL <- s34v1_empirical_null(NEGATIVE[model == "cause_specific_cox"])
cox_cont <- CONTINUOUS[model == "cause_specific_cox"]
fg_cont <- CONTINUOUS[model == "fine_gray"]
cox_cat <- CATEGORICAL[model == "cause_specific_cox"]
fg_cat <- CATEGORICAL[model == "fine_gray"]
cox_cont <- s34v1_add_calibration(cox_cont, EMPIRICAL_NULL)
cox_cat <- s34v1_add_calibration(cox_cat, EMPIRICAL_NULL)

prior_path <- file.path(S34V1_HOME, "results", "s32v1_10_primary_cause_specific_cox.csv")
if (!file.exists(prior_path)) {
  stop("The disclosure-safe patch-04 S32 primary aggregate is absent.", call. = FALSE)
}
PRIOR <- fread(prior_path, na.strings = c("", "NA", "SUPPRESSED"))
if (!all(c("multiplicity_family_n", "model_patch", "endpoint", "exposure", "p", "p_cal") %in%
         names(PRIOR)) ||
    any(PRIOR$multiplicity_family_n != 50L, na.rm = TRUE) ||
    any(PRIOR$model_patch != "S32V1_STAGE_B_PATCH_04")) {
  stop("The existing S32 primary aggregate is not the accepted patch-04 result.", call. = FALSE)
}

raw_family <- rbindlist(list(
  PRIOR[is.finite(p), .(key = paste0("s32:", endpoint, ":", exposure), p)],
  cox_cont[is.finite(p), .(key = paste0("s34_cont:", endpoint), p)],
  unique(cox_cat[is.finite(global_p), .(key = paste0("s34_cat:", endpoint), p = global_p)])
), fill = TRUE)
raw_family[, q := p.adjust(p, method = "BH", n = S34V1_FAMILY_N)]

cal_family <- rbindlist(list(
  PRIOR[is.finite(p_cal), .(key = paste0("s32:", endpoint, ":", exposure), p = p_cal)],
  cox_cont[is.finite(p_cal), .(key = paste0("s34_cont:", endpoint), p = p_cal)]
), fill = TRUE)
if (nrow(cal_family)) cal_family[, q := p.adjust(p, method = "BH", n = S34V1_FAMILY_N)]

cox_cont[, `:=`(
  q_raw = raw_family$q[match(paste0("s34_cont:", endpoint), raw_family$key)],
  q_cal = if (nrow(cal_family)) cal_family$q[match(paste0("s34_cont:", endpoint), cal_family$key)] else NA_real_,
  multiplicity_family_n = S34V1_FAMILY_N
)]
cox_cat[, `:=`(
  q_global_raw = raw_family$q[match(paste0("s34_cat:", endpoint), raw_family$key)],
  q_global_cal = NA_real_, multiplicity_family_n = S34V1_FAMILY_N,
  calibration_note = "coefficient calibration shown; multi-df global calibrated test not defined"
)]
fg_cont[, `:=`(q_raw = NA_real_, q_cal = NA_real_, multiplicity_family_n = S34V1_FAMILY_N)]
fg_cat[, `:=`(
  q_global_raw = NA_real_, q_global_cal = NA_real_,
  multiplicity_family_n = S34V1_FAMILY_N,
  calibration_note = "Fine-Gray sensitivity; no empirical-null calibration"
)]
CONTINUOUS <- rbindlist(list(cox_cont, fg_cont), fill = TRUE)
CATEGORICAL <- rbindlist(list(cox_cat, fg_cat), fill = TRUE)

positive_frame <- s32v1_make_frame(D, EVENTS, "major_fracture")
positive_fit <- s34v1_fit_binary_positive(positive_frame, "major_fracture")
if (isTRUE(positive_fit$ok)) {
  POSITIVE <- copy(positive_fit$row)
  POSITIVE[, `:=`(
    gate = "osteoporosis_baseline_predicts_incident_major_fracture",
    direction_pass = hr > 1,
    note = "Plausibility control; not a causal validation."
  )]
  exploratory <- !isTRUE(POSITIVE$direction_pass[1])
} else {
  POSITIVE <- data.table(
    gate = "osteoporosis_baseline_predicts_incident_major_fracture",
    direction_pass = NA,
    note = paste0("Model not estimable under frozen gates: ", positive_fit$reason)
  )
  exploratory <- TRUE
  FAILURES <- rbind(
    FAILURES,
    data.table(
      analysis = "positive_control", endpoint = "major_fracture",
      exposure = "osteoporosis_baseline", reason = positive_fit$reason
    ), fill = TRUE
  )
}

GLUCOCORTICOID <- data.table(
  endpoint = S34V1_PRIMARY_ENDPOINTS,
  exposure = "systemic_glucocorticoid",
  requested_code = "ATC H02AB",
  estimable = FALSE,
  multiplicity_family_n = S34V1_FAMILY_N,
  reason = paste0(
    "Not derivable from frozen S32 checkpoint/drug audit; S34 forbids new extraction; ",
    "prespecified family cell retained as unsupported."
  )
)
FAILURES <- rbind(
  FAILURES,
  GLUCOCORTICOID[, .(
    analysis = "glucocorticoid_feasibility", endpoint,
    exposure, reason
  )], fill = TRUE
)

DISSOCIATION <- cox_cont[endpoint %in% c("major_fracture", "osteoporosis")]
if (nrow(DISSOCIATION)) {
  DISSOCIATION[, `:=`(
    comparison_role = "fracture_vs_osteoporosis_gradient_side_by_side",
    formal_between_endpoint_test = FALSE,
    interpretation = if (exploratory) {
      "exploratory_positive_control_not_recovered"
    } else "plausibility_control_recovered"
  )]
}

status_value <- if (exploratory) "EXPLORATORY" else "SUPPORTED_BY_POSITIVE_CONTROL"
if (nrow(CONTINUOUS)) CONTINUOUS[, analysis_status := status_value]
if (nrow(CATEGORICAL)) CATEGORICAL[, analysis_status := status_value]
if (nrow(DISSOCIATION)) DISSOCIATION[, analysis_status := status_value]

s32v1_write_safe(
  CONTINUOUS, "s34v1_10_multimorbidity_continuous.csv",
  count_cols = intersect(c("n", "events"), names(CONTINUOUS)),
  support_cols = intersect(c("n", "events"), names(CONTINUOUS))
)
s32v1_write_safe(
  CATEGORICAL, "s34v1_11_multimorbidity_categorical.csv",
  count_cols = intersect(
    c("n", "events", "category_n", "category_events", "reference_n", "reference_events"),
    names(CATEGORICAL)
  ),
  support_cols = intersect(c("n", "events"), names(CATEGORICAL)),
  complementary_sets = list(
    intersect(c("category_n", "reference_n"), names(CATEGORICAL)),
    intersect(c("category_events", "reference_events"), names(CATEGORICAL))
  )
)
s32v1_write_safe(
  DISSOCIATION, "s34v1_12_dissociation.csv",
  count_cols = intersect(c("n", "events"), names(DISSOCIATION)),
  support_cols = intersect(c("n", "events"), names(DISSOCIATION))
)
s32v1_write_safe(
  NEGATIVE, "s34v1_13_negative_controls.csv",
  count_cols = intersect(
    c("n", "events", "category_n", "category_events", "reference_n", "reference_events"),
    names(NEGATIVE)
  ),
  support_cols = intersect(c("n", "events"), names(NEGATIVE))
)
s32v1_write_safe(EMPIRICAL_NULL, "s34v1_14_empirical_null.csv", count_cols = character())
s32v1_write_safe(
  POSITIVE, "s34v1_15_positive_control_retest.csv",
  count_cols = intersect(
    c("n", "events", "exposed_n", "unexposed_n", "exposed_events", "unexposed_events"),
    names(POSITIVE)
  ),
  support_cols = intersect(c("n", "events"), names(POSITIVE))
)
s32v1_write_safe(FAILURES, "s34v1_16_model_failures.csv", count_cols = character())
s32v1_write_safe(GLUCOCORTICOID, "s34v1_17_glucocorticoid_feasibility.csv", count_cols = character())

result_dir <- file.path(S34V1_HOME, "results")
members <- list.files(result_dir, pattern = "^s34v1_(0[1]|1[0-7])_.+[.]csv$", full.names = TRUE)
members <- normalizePath(members, winslash = "/", mustWork = TRUE)
expected <- c(
  "s34v1_01_category_audit.csv", "s34v1_10_multimorbidity_continuous.csv",
  "s34v1_11_multimorbidity_categorical.csv", "s34v1_12_dissociation.csv",
  "s34v1_13_negative_controls.csv", "s34v1_14_empirical_null.csv",
  "s34v1_15_positive_control_retest.csv", "s34v1_16_model_failures.csv",
  "s34v1_17_glucocorticoid_feasibility.csv"
)
if (!setequal(basename(members), expected)) {
  stop("S34 result member set is incomplete or contaminated.", call. = FALSE)
}
manifest <- data.frame(
  file = basename(members), bytes = file.info(members)$size,
  md5 = unname(tools::md5sum(members)), stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(result_dir, "s34v1_98_manifest.csv"), row.names = FALSE)
readme <- c(
  "EPI-BONE All of Us S34 multimorbidity aggregate package",
  paste0("Version: ", S34V1_VERSION),
  paste0("Source checkpoint: ", CHECKPOINT$version),
  paste0("Phenotype: ", CHECKPOINT$phenotype_version),
  "No BigQuery extraction was submitted; the frozen S32 checkpoint was reused.",
  "Counts 1-20 inclusive are suppressed; zero and counts >=21 may be reported.",
  "Multiplicity family n=65: prior S32 50 plus 5 continuous, 5 categorical-global, and 5 reserved glucocorticoid cells.",
  "Systemic glucocorticoids were not derivable without a forbidden new extraction and remain unsupported.",
  "Categorical coefficients show shape; BH uses one global test per endpoint.",
  paste0("Analysis status: ", if (exploratory) "EXPLORATORY because the positive control was not recovered." else "positive-control direction recovered."),
  "All estimates are observational associations; the multimorbidity count is not a frailty index."
)
writeLines(readme, file.path(result_dir, "s34v1_99_README.txt"))
package_members <- normalizePath(
  c(members, file.path(result_dir, "s34v1_98_manifest.csv"),
    file.path(result_dir, "s34v1_99_README.txt")),
  winslash = "/", mustWork = TRUE
)
zip_path <- file.path(result_dir, "EPIBONE_AOU_S34_MULTIMORBIDITY_SAFE_AGGREGATES.zip")
if (file.exists(zip_path)) {
  stop("A prior S34 ZIP already exists; preserve it and rename it before an authorized rerun.", call. = FALSE)
}
zip_cmd <- Sys.which("zip")
if (!nzchar(zip_cmd)) stop("System zip command is unavailable.", call. = FALSE)
zip_output <- suppressWarnings(system2(
  zip_cmd, args = c("-q", "-j", shQuote(zip_path), shQuote(package_members)),
  stdout = TRUE, stderr = TRUE
))
zip_status <- attr(zip_output, "status")
if (is.null(zip_status)) zip_status <- 0L
if (zip_status != 0L || !file.exists(zip_path)) {
  stop("S34 ZIP creation failed: ", paste(zip_output, collapse = " | "), call. = FALSE)
}
listed <- utils::unzip(zip_path, list = TRUE)$Name
if (!setequal(listed, basename(package_members)) ||
    any(grepl("rds|person|participant|private", listed, ignore.case = TRUE))) {
  stop("S34 ZIP disclosure/member audit failed.", call. = FALSE)
}

message("S34 COMPLETE. Review and download: ", zip_path)
