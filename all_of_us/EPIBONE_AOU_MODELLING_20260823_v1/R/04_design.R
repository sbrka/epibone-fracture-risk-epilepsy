# Design-only stage. No outcome or death-date column is referenced here.

epibone_arm_n <- function(d) {
  out <- table(factor(d$exposure, levels = c(0, 1)))
  c(comparator = unname(out[1]), exposure = unname(out[2]))
}

epibone_calendar_window <- function(d) {
  d$calendar_band <- 5L * floor(as.integer(d$index_year) / 5L)
  counts <- stats::aggregate(
    rep(1L, nrow(d)),
    by = list(calendar_band = d$calendar_band, exposure = d$exposure),
    FUN = sum
  )
  names(counts)[3] <- "n_people"
  wide <- reshape(counts, idvar = "calendar_band", timevar = "exposure",
                  direction = "wide")
  names(wide) <- sub("n_people\\.", "n_", names(wide))
  for (name in c("n_0", "n_1")) if (!name %in% names(wide)) wide[[name]] <- 0L
  eligible <- sort(wide$calendar_band[wide$n_0 > 20 & wide$n_1 > 20])
  if (!length(eligible)) {
    return(list(ok = FALSE, data = d[0, , drop = FALSE], table = wide,
                start = NA_integer_, end = NA_integer_))
  }
  run_id <- cumsum(c(TRUE, diff(eligible) != 5L))
  runs <- split(eligible, run_id)
  score <- do.call(rbind, lapply(runs, function(x) {
    in_run <- wide$calendar_band %in% x
    data.frame(
      start = min(x), end = max(x), n_bands = length(x),
      total = sum(wide$n_0[in_run] + wide$n_1[in_run])
    )
  }))
  score <- score[order(-score$n_bands, -score$total, -score$end), , drop = FALSE]
  chosen <- score[1, ]
  keep <- d$calendar_band >= chosen$start & d$calendar_band <= chosen$end
  list(ok = TRUE, data = d[keep, , drop = FALSE], table = wide,
       start = as.integer(chosen$start), end = as.integer(chosen$end + 4L))
}

epibone_contrast_data <- function(cohort, spec) {
  module <- if (spec$module == "V4") "V3" else spec$module
  base <- if (spec$scenario == "strict90") "strict90" else if (module == "V2") {
    "primary365"
  } else if (module == "V3") {
    "exploratory_broad30"
  } else {
    "primary30"
  }
  d <- cohort[cohort$module == module & cohort$base_scenario == base, , drop = FALSE]

  if (module == "V1") {
    d <- d[d$arm %in% c(spec$exposure, spec$comparator), , drop = FALSE]
    d$analysis_arm <- d$arm
  } else if (module == "V2") {
    d <- d[d$arm %in% c("multiple_ingredients", "one_ingredient"), , drop = FALSE]
    d$analysis_arm <- d$arm
  } else {
    canonical <- c("CBZ", "PHT", "PHB")
    selected <- c("LEV", "LTG", "LCM")
    if (spec$scenario == "include_primidone") canonical <- c(canonical, "PRM")
    if (spec$scenario == "two_code") d <- d[d$phenotype_days >= 2, , drop = FALSE]
    if (spec$scenario == "obs730") d <- d[d$obs730_flag == 1, , drop = FALSE]
    if (spec$scenario == "nonacute") d <- d[d$index_acute == 0, , drop = FALSE]

    if (spec$scenario %in% c("primary30", "two_code", "obs730", "nonacute",
                             "include_primidone")) {
      d <- d[d$ingredient %in% c(canonical, selected), , drop = FALSE]
      d$analysis_arm <- ifelse(d$ingredient %in% canonical,
                               spec$exposure, spec$comparator)
    } else if (spec$scenario == "lev_reference") {
      d <- d[d$ingredient %in% c(canonical, "LEV"), , drop = FALSE]
      d$analysis_arm <- ifelse(d$ingredient %in% canonical, spec$exposure, "LEV")
    } else if (spec$scenario == "pair") {
      d <- d[d$ingredient %in% c(spec$exposure, spec$comparator), , drop = FALSE]
      d$analysis_arm <- d$ingredient
    } else {
      stop("Unknown V3/V4 scenario: ", spec$scenario, call. = FALSE)
    }
  }

  d$exposure <- as.integer(d$analysis_arm == spec$exposure)
  d <- d[d$analysis_arm %in% c(spec$exposure, spec$comparator), , drop = FALSE]
  duplicated_people <- unique(d$person_id[duplicated(d$person_id) |
                                           duplicated(d$person_id, fromLast = TRUE)])
  n_multiarm <- length(duplicated_people)
  n_ties <- 0L
  if (n_multiarm) {
    index_number <- as.numeric(d$index_date)
    first_number <- ave(index_number, d$person_id, FUN = min)
    d <- d[index_number == first_number, , drop = FALSE]
    tied_people <- names(which(table(d$person_id) > 1L))
    n_ties <- length(tied_people)
    if (n_ties) d <- d[!as.character(d$person_id) %in% tied_people, , drop = FALSE]
  }
  attr(d, "n_multiarm_people_resolved") <- n_multiarm
  attr(d, "n_same_day_ties_removed") <- n_ties
  d
}

epibone_collapse_administrative_demographic <- function(value) {
  value <- trimws(as.character(value))
  value[is.na(value) | !nzchar(value)] <- "Unknown"
  administrative <- grepl(
    paste0(
      "prefer[.]? not|pmi.*skip|none[.]? indicated|none[.]? of[.]? these|",
      "not[.]? man[.]? only.*not[.]? woman[.]? only.*skipp|^unknown$"
    ),
    tolower(value), perl = TRUE
  )
  value[administrative] <- "Administrative nonresponse or unknown"
  value
}

epibone_covariate_frame <- function(d) {
  continuous <- intersect(c(
    "age", "index_year", "history_days", "visit_days_pre", "acute_visit_days_pre",
    "condition_days_pre", "drug_days_pre", "prior_epilepsy_days",
    "acute_visit_days_assessment", "epilepsy_days_assessment"
  ), names(d))
  binary <- names(d)[grepl("^(hx_|prior_med_)", names(d))]
  if ("index_acute" %in% names(d)) binary <- c(binary, "index_acute")
  x <- data.frame(row.names = seq_len(nrow(d)))

  for (name in continuous) {
    value <- suppressWarnings(as.numeric(d[[name]]))
    if (grepl("days|history", name)) value <- log1p(pmax(value, 0))
    missing <- !is.finite(value)
    replacement <- if (all(missing)) 0 else stats::median(value[!missing])
    value[missing] <- replacement
    scale_value <- stats::sd(value)
    if (!is.finite(scale_value) || scale_value == 0) scale_value <- 1
    x[[paste0(name, "_z")]] <- (value - mean(value)) / scale_value
    if (any(missing)) x[[paste0(name, "_missing")]] <- as.integer(missing)
  }
  for (name in binary) {
    value <- suppressWarnings(as.numeric(d[[name]]))
    value[!is.finite(value)] <- 0
    x[[name]] <- as.integer(value > 0)
  }
  if ("medication_epilepsy_route" %in% names(d) &&
      any(!is.na(d$medication_epilepsy_route))) {
    route <- as.character(d$medication_epilepsy_route)
    allowed_routes <- c(
      "DIRECT_G40_OR_ICD9_345",
      "R56_OR_ICD9_7803_THEN_INDEX_ASM_0_180D"
    )
    if (any(is.na(route)) || any(!route %in% allowed_routes)) {
      stop("Medication epilepsy route is missing or unrecognised.", call. = FALSE)
    }
    x$medication_epilepsy_route_r56_supported <- as.integer(
      route == "R56_OR_ICD9_7803_THEN_INDEX_ASM_0_180D"
    )
  }
  demographic_actions <- list()
  for (name in intersect(c("sex", "race", "ethnicity"), names(d))) {
    original <- trimws(as.character(d[[name]]))
    original[is.na(original) | !nzchar(original)] <- "Unknown"
    value <- epibone_collapse_administrative_demographic(original)
    if (any(value != original)) {
      demographic_actions[[name]] <- data.frame(
        covariate = name,
        action = "COLLAPSED_ADMINISTRATIVE_NONRESPONSE",
        detail = "Prefer-not-to-answer, PMI-skip, none-indicated/none-of-these and unknown labels",
        n_level_comparator = NA_integer_, n_level_exposure = NA_integer_,
        n_removed_comparator = 0L, n_removed_exposure = 0L,
        stringsAsFactors = FALSE
      )
    }
    frequency <- sort(table(value), decreasing = TRUE)
    ordered_levels <- names(frequency)[order(-as.numeric(frequency), names(frequency))]
    x[[name]] <- factor(value, levels = ordered_levels)
  }
  mm <- stats::model.matrix(~ ., data = x)
  mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  storage.mode(mm) <- "double"
  colnames(mm) <- make.names(colnames(mm), unique = TRUE)
  attr(mm, "demographic_actions") <- demographic_actions
  mm
}

epibone_trim_empirical_nonoverlap <- function(d, x) {
  keep <- rep(TRUE, nrow(d))
  actions <- list()
  iteration <- 0L
  repeat {
    iteration <- iteration + 1L
    if (iteration > max(5L, ncol(x) + 1L)) {
      stop("Empirical nonoverlap trimming did not converge.", call. = FALSE)
    }
    current <- which(keep)
    if (!length(current) || any(epibone_arm_n(d[current, , drop = FALSE]) <= 20)) break
    remove <- integer()
    round_actions <- list()
    for (j in seq_len(ncol(x))) {
      z <- x[current, j]
      if (!all(z %in% c(0, 1))) next
      for (level in c(0, 1)) {
        at_level <- current[z == level]
        n0 <- sum(d$exposure[at_level] == 0)
        n1 <- sum(d$exposure[at_level] == 1)
        if (xor(n0 == 0, n1 == 0)) {
          doomed <- at_level[if (n0 > 0) d$exposure[at_level] == 0 else
                               d$exposure[at_level] == 1]
          if (length(doomed)) {
            remove <- union(remove, doomed)
            round_actions[[length(round_actions) + 1L]] <- data.frame(
              covariate = colnames(x)[j], action = "TRIMMED_ONE_ARM_ONLY_LEVEL",
              detail = paste0("level=", level, "; iteration=", iteration),
              n_level_comparator = n0, n_level_exposure = n1,
              n_removed_comparator = sum(d$exposure[doomed] == 0),
              n_removed_exposure = sum(d$exposure[doomed] == 1),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    if (!length(remove)) break
    keep[remove] <- FALSE
    actions <- c(actions, round_actions)
  }
  list(keep = keep, actions = actions)
}

epibone_qr_prune <- function(x, tolerance = 1e-8) {
  # Centering makes affine dependencies among covariates linear while keeping
  # the fitted intercept outside the pivot decision and therefore guaranteed.
  centered <- sweep(x, 2L, colMeans(x), FUN = "-")
  decomposition <- qr(centered, tol = tolerance, LAPACK = FALSE)
  keep <- sort(decomposition$pivot[seq_len(decomposition$rank)])
  dropped <- setdiff(seq_len(ncol(x)), keep)
  list(
    x = x[, keep, drop = FALSE],
    dropped = colnames(x)[dropped],
    rank = decomposition$rank + 1L,
    tolerance = tolerance
  )
}

epibone_filter_design_matrix <- function(x, exposure, d = NULL) {
  if (!ncol(x)) stop("No baseline covariates were constructed.", call. = FALSE)
  if (is.null(d)) d <- data.frame(exposure = exposure)
  demographic_actions <- attr(x, "demographic_actions")
  trimmed <- epibone_trim_empirical_nonoverlap(d, x)
  d2 <- d[trimmed$keep, , drop = FALSE]
  x2 <- x[trimmed$keep, , drop = FALSE]
  if (!nrow(d2) || any(epibone_arm_n(d2) <= 20)) {
    return(list(
      data = d2, x = x2, diagnostic = x2,
      actions = c(demographic_actions, trimmed$actions),
      trim_keep = trimmed$keep, qr_dropped = character(), ok = FALSE
    ))
  }
  variable <- apply(x2, 2, function(z) length(unique(z[is.finite(z)])) > 1)
  invariant <- colnames(x2)[!variable]
  diagnostic <- x2[, variable, drop = FALSE]
  if (!ncol(diagnostic)) stop("All covariates were invariant after support trimming.",
                              call. = FALSE)
  pruned <- epibone_qr_prune(diagnostic)
  if (!ncol(pruned$x)) stop("No independent baseline covariates remained.", call. = FALSE)
  actions <- c(demographic_actions, trimmed$actions)
  for (j in seq_len(ncol(diagnostic))) {
    z <- diagnostic[, j]
    if (!all(z %in% c(0, 1))) next
    for (level in c(0, 1)) {
      n0 <- sum(z[d2$exposure == 0] == level)
      n1 <- sum(z[d2$exposure == 1] == level)
      if (n0 > 0 && n1 > 0 && any(c(n0, n1) <= 20)) {
        actions[[length(actions) + 1L]] <- data.frame(
          covariate = colnames(diagnostic)[j],
          action = "RETAINED_SUPPORTED_SPARSE_LEVEL_IN_PS",
          detail = paste0("level=", level, "; observed in both arms"),
          n_level_comparator = n0, n_level_exposure = n1,
          n_removed_comparator = 0L, n_removed_exposure = 0L,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(invariant)) {
    actions[[length(actions) + 1L]] <- data.frame(
      covariate = invariant, action = "DROPPED_INVARIANT_AFTER_SUPPORT_TRIM",
      detail = "Retained in the audit trail; not estimable in the PS",
      n_level_comparator = NA_integer_, n_level_exposure = NA_integer_,
      n_removed_comparator = 0L, n_removed_exposure = 0L,
      stringsAsFactors = FALSE
    )
  }
  if (length(pruned$dropped)) {
    actions[[length(actions) + 1L]] <- data.frame(
      covariate = pruned$dropped, action = "DROPPED_EXACT_COLLINEARITY_QR",
      detail = paste0("Deterministic QR tolerance=", format(pruned$tolerance)),
      n_level_comparator = NA_integer_, n_level_exposure = NA_integer_,
      n_removed_comparator = 0L, n_removed_exposure = 0L,
      stringsAsFactors = FALSE
    )
  }
  list(
    data = d2, x = pruned$x, diagnostic = diagnostic, actions = actions,
    trim_keep = trimmed$keep, qr_dropped = pruned$dropped, ok = TRUE
  )
}

epibone_richer_matrix <- function(x) {
  age <- grep("^age_z$", colnames(x), value = TRUE)
  year <- grep("^index_year_z$", colnames(x), value = TRUE)
  modifiers <- grep("^(sex|hx_falls|hx_osteoporosis|hx_dementia|hx_gait_disorder|hx_ckd|hx_stroke)",
                    colnames(x), value = TRUE)
  additions <- list()
  if (length(age)) {
    additions[["age_quadratic"]] <- x[, age[1]]^2
    for (name in head(modifiers, 7L)) {
      additions[[paste0("age_by_", name)]] <- x[, age[1]] * x[, name]
    }
  }
  if (length(year)) additions[["index_year_quadratic"]] <- x[, year[1]]^2
  if (!length(additions)) return(x)
  extra <- do.call(cbind, additions)
  cbind(x, extra)
}

epibone_fit_ps <- function(x, exposure) {
  fit <- suppressWarnings(stats::glm.fit(
    x = cbind(`(Intercept)` = 1, x), y = exposure,
    family = stats::binomial(), control = stats::glm.control(maxit = 100)
  ))
  beta <- fit$coefficients
  if (any(!is.finite(beta))) stop("Propensity model was rank-deficient.", call. = FALSE)
  if (!isTRUE(fit$converged)) stop("Propensity model did not converge.", call. = FALSE)
  ps <- stats::plogis(drop(cbind(1, x) %*% beta))
  if (any(!is.finite(ps))) stop("Propensity scores were non-finite.", call. = FALSE)
  list(fit = fit, ps = pmin(pmax(ps, 1e-6), 1 - 1e-6))
}

epibone_weighted_var <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) < 2) return(NA_real_)
  x <- x[ok]; w <- w[ok]
  mu <- sum(w * x) / sum(w)
  sum(w * (x - mu)^2) / sum(w)
}

epibone_balance <- function(x, exposure, weights) {
  rows <- lapply(seq_len(ncol(x)), function(j) {
    z <- x[, j]
    i0 <- exposure == 0; i1 <- exposure == 1
    m0 <- stats::weighted.mean(z[i0], weights[i0])
    m1 <- stats::weighted.mean(z[i1], weights[i1])
    s0 <- epibone_weighted_var(z[i0], weights[i0])
    s1 <- epibone_weighted_var(z[i1], weights[i1])
    denom <- sqrt((s0 + s1) / 2)
    asmd <- if (is.finite(denom) && denom > 0) abs(m1 - m0) / denom else 0
    binary <- all(z %in% c(0, 1))
    data.frame(
      covariate = colnames(x)[j], comparator_mean = m0,
      exposure_mean = m1, asmd = asmd,
      raw_positive_n_comparator = if (binary) sum(z[i0] == 1) else sum(i0),
      raw_positive_n_exposure = if (binary) sum(z[i1] == 1) else sum(i1),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

epibone_ess <- function(w) sum(w)^2 / sum(w^2)

epibone_attempt_overlap_design <- function(d, x, model_version) {
  first <- epibone_fit_ps(x, d$exposure)
  lower <- max(min(first$ps[d$exposure == 0]), min(first$ps[d$exposure == 1]))
  upper <- min(max(first$ps[d$exposure == 0]), max(first$ps[d$exposure == 1]))
  keep <- first$ps >= lower & first$ps <= upper
  retention <- vapply(c(0, 1), function(a) mean(keep[d$exposure == a]), numeric(1))
  if (!is.finite(lower) || !is.finite(upper) || lower >= upper || any(retention < 0.90)) {
    return(list(ok = FALSE, status = "STOP_COMMON_SUPPORT", retention = retention,
                lower = lower, upper = upper, model_version = model_version))
  }
  d2 <- d[keep, , drop = FALSE]
  x2 <- x[keep, , drop = FALSE]
  refit <- epibone_fit_ps(x2, d2$exposure)
  weights <- ifelse(d2$exposure == 1, 1 - refit$ps, refit$ps)
  balance <- epibone_balance(x2, d2$exposure, weights)
  ess <- vapply(c(0, 1), function(a) epibone_ess(weights[d2$exposure == a]), numeric(1))
  list(ok = TRUE, status = "FITTED", data = d2, x = x2, ps = refit$ps,
       weights = weights, balance = balance, max_asmd = max(balance$asmd, na.rm = TRUE),
       ess = ess, retention = retention, lower = lower, upper = upper,
       model_version = model_version, keep = keep)
}

epibone_matchit_positions <- function(fit, n_rows) {
  if (is.null(fit$weights) || length(fit$weights) != n_rows) {
    stop("MatchIt weights do not align with the input design rows.", call. = FALSE)
  }
  index <- unname(which(fit$weights > 0))
  if (!length(index) || any(index < 1L | index > n_rows)) {
    stop("Could not recover matching row positions from MatchIt weights.", call. = FALSE)
  }
  matched_weights <- unname(fit$weights[index])
  if (length(matched_weights) != length(index) ||
      any(!is.finite(matched_weights)) || any(matched_weights <= 0)) {
    stop("MatchIt returned invalid matched-sample weights.", call. = FALSE)
  }
  list(index = index, weights = matched_weights)
}

epibone_matching_sensitivity <- function(design) {
  d <- design$data
  match_data <- data.frame(
    exposure = d$exposure,
    logit_ps = stats::qlogis(pmin(pmax(design$ps, 1e-6), 1 - 1e-6))
  )
  fit <- MatchIt::matchit(
    exposure ~ logit_ps, data = match_data, method = "nearest",
    distance = match_data$logit_ps, caliper = 0.2, std.caliper = TRUE,
    estimand = "ATT", ratio = 1, replace = FALSE, m.order = "largest"
  )
  recovered <- epibone_matchit_positions(fit, nrow(d))
  index <- recovered$index
  matched_weights <- recovered$weights
  balance_matrix <- if (!is.null(design$diagnostic)) design$diagnostic else design$x
  bal <- epibone_balance(balance_matrix[index, , drop = FALSE],
                         d$exposure[index], matched_weights)
  n <- table(factor(d$exposure[index], levels = c(0, 1)))
  list(
    summary = data.frame(
      n_comparator = unname(n[1]), n_exposure = unname(n[2]),
      retention_comparator = unname(n[1]) / sum(d$exposure == 0),
      retention_exposure = unname(n[2]) / sum(d$exposure == 1),
      maximum_asmd = max(bal$asmd, na.rm = TRUE),
      caliper_sd_logit = 0.2, matching_status = "MATCHED",
      matching_note = "", stringsAsFactors = FALSE
    ),
    index = index,
    weights = matched_weights
  )
}

epibone_design_one <- function(cohort, spec) {
  d <- epibone_contrast_data(cohort, spec)
  n_multiarm <- attr(d, "n_multiarm_people_resolved")
  n_ties <- attr(d, "n_same_day_ties_removed")
  n_initial <- epibone_arm_n(d)
  empty <- list(
    contrast_id = spec$contrast_id, role = spec$role,
    n_initial = n_initial, n_multiarm = n_multiarm, n_ties = n_ties,
    n_calendar = c(comparator = 0, exposure = 0),
    n_support_trim = c(comparator = 0, exposure = 0),
    n_final = c(comparator = 0, exposure = 0),
    calendar_start = NA_integer_, calendar_end = NA_integer_,
    support_trim_retention = c(NA_real_, NA_real_),
    ps_support_retention = c(NA_real_, NA_real_),
    retention = c(NA_real_, NA_real_), ess = c(NA_real_, NA_real_),
    max_asmd = NA_real_, support_lower = NA_real_, support_upper = NA_real_,
    model_version = NA_character_, status = "STOP_SMALL_ARM", note = "",
    composition = NULL, phenotype_routes = NULL, covariate_actions = NULL
  )
  if (any(n_initial <= 20)) return(empty)

  calendar <- epibone_calendar_window(d)
  empty$calendar_start <- calendar$start
  empty$calendar_end <- calendar$end
  if (!calendar$ok) {
    empty$status <- "STOP_CALENDAR_SUPPORT"
    empty$calendar_table <- calendar$table
    return(empty)
  }
  d <- calendar$data
  empty$n_calendar <- epibone_arm_n(d)
  empty$calendar_table <- calendar$table
  if (any(empty$n_calendar <= 20)) return(empty)

  if ("ingredient" %in% names(d) && any(!is.na(d$ingredient))) {
    empty$composition <- stats::aggregate(
      rep(1L, nrow(d)),
      by = list(
        population = rep("calendar_eligible_preweighting", nrow(d)),
        analysis_arm = d$analysis_arm, ingredient = d$ingredient
      ), FUN = sum
    )
    names(empty$composition)[4] <- "n_people"
  }
  if ("medication_epilepsy_route" %in% names(d) &&
      any(!is.na(d$medication_epilepsy_route))) {
    empty$phenotype_routes <- stats::aggregate(
      rep(1L, nrow(d)),
      by = list(
        population = rep("calendar_eligible_preweighting", nrow(d)),
        analysis_arm = d$analysis_arm,
        medication_epilepsy_route = d$medication_epilepsy_route
      ), FUN = sum
    )
    names(empty$phenotype_routes)[4] <- "n_people"
  }

  prepared <- epibone_filter_design_matrix(
    epibone_covariate_frame(d), d$exposure, d = d
  )
  empty$covariate_actions <- prepared$actions
  d <- prepared$data
  empty$n_support_trim <- epibone_arm_n(d)
  empty$support_trim_retention <- empty$n_support_trim / empty$n_calendar
  if (!isTRUE(prepared$ok) || any(empty$n_support_trim <= 20) ||
      any(empty$support_trim_retention < 0.90)) {
    empty$status <- "STOP_EMPIRICAL_NONOVERLAP"
    empty$note <- paste0(
      "One-arm-only baseline levels required more than 10% trimming or left a small arm."
    )
    return(empty)
  }
  x <- prepared$x
  attempt <- tryCatch(
    epibone_attempt_overlap_design(d, x, "main_effects_qr_pruned"),
    error = function(e) e
  )
  if (inherits(attempt, "error")) {
    empty$status <- "STOP_DESIGN_ERROR"
    empty$note <- conditionMessage(attempt)
    return(empty)
  }
  if (attempt$ok) {
    attempt$diagnostic <- prepared$diagnostic[attempt$keep, , drop = FALSE]
    attempt$balance <- epibone_balance(
      attempt$diagnostic, attempt$data$exposure, attempt$weights
    )
    attempt$max_asmd <- max(attempt$balance$asmd, na.rm = TRUE)
  }
  if (attempt$ok && attempt$max_asmd > 0.10) {
    richer <- epibone_qr_prune(epibone_richer_matrix(x))
    attempt <- tryCatch(
      epibone_attempt_overlap_design(d, richer$x,
                                     "prespecified_richer_qr_pruned"),
      error = function(e) e
    )
    if (inherits(attempt, "error")) {
      empty$status <- "STOP_DESIGN_ERROR"
      empty$note <- conditionMessage(attempt)
      return(empty)
    }
    if (attempt$ok) {
      attempt$diagnostic <- prepared$diagnostic[attempt$keep, , drop = FALSE]
      attempt$balance <- epibone_balance(
        attempt$diagnostic, attempt$data$exposure, attempt$weights
      )
      attempt$max_asmd <- max(attempt$balance$asmd, na.rm = TRUE)
    }
  }
  empty$model_version <- attempt$model_version
  empty$ps_support_retention <- attempt$retention
  if (!is.null(attempt$keep)) {
    empty$retention <- epibone_arm_n(attempt$data) / empty$n_calendar
  } else {
    empty$retention <- empty$support_trim_retention * attempt$retention
  }
  empty$support_lower <- attempt$lower
  empty$support_upper <- attempt$upper
  if (!attempt$ok) {
    empty$status <- attempt$status
    return(empty)
  }
  empty$n_final <- epibone_arm_n(attempt$data)
  empty$ess <- attempt$ess
  empty$max_asmd <- attempt$max_asmd
  if (any(empty$retention < 0.90)) {
    empty$status <- "STOP_COMMON_SUPPORT_CUMULATIVE"
    empty$balance <- attempt$balance
    empty$note <- "Combined empirical-level and propensity common-support retention was below 90%."
    return(empty)
  }
  if (any(attempt$ess < 100)) {
    empty$status <- "STOP_ESS"
    empty$balance <- attempt$balance
    return(empty)
  }
  if (!is.finite(attempt$max_asmd) || attempt$max_asmd > 0.10) {
    empty$status <- "STOP_BALANCE"
    empty$balance <- attempt$balance
    return(empty)
  }

  empty$status <- "PROCEED"
  empty$balance <- attempt$balance
  keys <- attempt$data[, c(
    "module", "base_scenario", "person_id", "arm", "analysis_arm", "exposure",
    "index_date", "landmark_date", "obs_end"
  ), drop = FALSE]
  keys$overlap_weight <- attempt$weights
  keys$propensity_score <- attempt$ps
  empty$private_design <- keys
  if (spec$role == "co_primary") {
    matching <- tryCatch(epibone_matching_sensitivity(attempt), error = function(e) e)
    if (inherits(matching, "error")) {
      empty$matching <- data.frame(
        n_comparator = NA_real_, n_exposure = NA_real_,
        retention_comparator = NA_real_, retention_exposure = NA_real_,
        maximum_asmd = NA_real_, caliper_sd_logit = 0.2,
        matching_status = "ERROR", matching_note = conditionMessage(matching),
        stringsAsFactors = FALSE
      )
    } else {
      empty$matching <- matching$summary
      if (is.finite(empty$matching$maximum_asmd) &&
          empty$matching$maximum_asmd <= 0.10) {
        empty$matching$matching_status <- "MATCHED_BALANCED"
        matched_keys <- keys[matching$index, , drop = FALSE]
        matched_keys$overlap_weight <- matching$weights
        empty$private_matching <- matched_keys
      } else {
        empty$matching$matching_status <- "REJECTED_BALANCE"
        empty$matching$matching_note <- "Maximum post-match ASMD exceeded 0.10."
      }
    }
  }
  empty
}

epibone_design_gate_row <- function(x) {
  data.frame(
    contrast_id = x$contrast_id, role = x$role,
    n_initial_comparator = x$n_initial[1], n_initial_exposure = x$n_initial[2],
    n_multiarm_people_resolved_by_earliest_index = x$n_multiarm,
    n_same_day_multiarm_ties_removed = x$n_ties,
    calendar_start = x$calendar_start, calendar_end = x$calendar_end,
    n_calendar_comparator = x$n_calendar[1], n_calendar_exposure = x$n_calendar[2],
    n_support_trim_comparator = x$n_support_trim[1],
    n_support_trim_exposure = x$n_support_trim[2],
    empirical_nonoverlap_retention_comparator = x$support_trim_retention[1],
    empirical_nonoverlap_retention_exposure = x$support_trim_retention[2],
    ps_common_support_retention_after_trim_comparator = x$ps_support_retention[1],
    ps_common_support_retention_after_trim_exposure = x$ps_support_retention[2],
    common_support_retention_comparator = x$retention[1],
    common_support_retention_exposure = x$retention[2],
    n_final_comparator = x$n_final[1], n_final_exposure = x$n_final[2],
    ess_comparator = x$ess[1], ess_exposure = x$ess[2],
    maximum_asmd = x$max_asmd, support_lower = x$support_lower,
    support_upper = x$support_upper, ps_model = x$model_version,
    gate = x$status, note = x$note, stringsAsFactors = FALSE
  )
}

epibone_run_design_stage <- function(cohort = epibone_build_or_resume_cohort()) {
  aou_ducc_preflight()
  plan <- epibone_contrast_plan()
  results <- vector("list", nrow(plan))
  for (i in seq_len(nrow(plan))) {
    spec <- plan[i, , drop = FALSE]
    message("Design ", i, "/", nrow(plan), ": ", spec$contrast_id)
    results[[i]] <- tryCatch(epibone_design_one(cohort, spec), error = function(e) {
      d <- tryCatch(epibone_contrast_data(cohort, spec), error = function(e2) cohort[0, ])
      n <- if (nrow(d)) epibone_arm_n(d) else c(comparator = 0, exposure = 0)
      list(
        contrast_id = spec$contrast_id, role = spec$role, n_initial = n,
        n_multiarm = 0, n_ties = 0, n_calendar = c(0, 0),
        n_support_trim = c(0, 0), n_final = c(0, 0),
        calendar_start = NA_integer_, calendar_end = NA_integer_,
        support_trim_retention = c(NA, NA), ps_support_retention = c(NA, NA),
        retention = c(NA, NA), ess = c(NA, NA), max_asmd = NA,
        support_lower = NA, support_upper = NA, model_version = NA,
        status = "STOP_DESIGN_ERROR", note = conditionMessage(e),
        composition = NULL, phenotype_routes = NULL, covariate_actions = NULL
      )
    })
  }
  names(results) <- plan$contrast_id
  outdir <- file.path(EPIBONE_ROOT, "results", "design")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  gates <- do.call(rbind, lapply(results, epibone_design_gate_row))
  aou_write_explicit_csv(
    gates, file.path(outdir, "01_design_gates.csv"),
    count_cols = c("n_initial_comparator", "n_initial_exposure",
                   "n_multiarm_people_resolved_by_earliest_index",
                   "n_same_day_multiarm_ties_removed", "n_calendar_comparator",
                   "n_calendar_exposure", "n_support_trim_comparator",
                   "n_support_trim_exposure", "n_final_comparator", "n_final_exposure")
  )

  balance <- do.call(rbind, lapply(results, function(x) {
    if (is.null(x$balance)) return(NULL)
    cbind(contrast_id = x$contrast_id, x$balance, row.names = NULL)
  }))
  if (is.null(balance)) balance <- data.frame(contrast_id = character(), covariate = character())
  aou_write_explicit_csv(
    balance, file.path(outdir, "02_weighted_balance.csv"),
    count_cols = intersect(
      c("raw_positive_n_comparator", "raw_positive_n_exposure"), names(balance)
    ),
    dependent_cols = intersect(
      c("comparator_mean", "exposure_mean", "asmd"), names(balance)
    )
  )

  composition <- do.call(rbind, lapply(results, function(x) {
    if (is.null(x$composition)) return(NULL)
    cbind(contrast_id = x$contrast_id, x$composition, row.names = NULL)
  }))
  if (is.null(composition)) {
    composition <- data.frame(
      contrast_id = character(), population = character(),
      analysis_arm = character(), ingredient = character(), n_people = numeric()
    )
  }
  aou_write_explicit_csv(composition, file.path(outdir, "03_ingredient_composition.csv"),
                         count_cols = "n_people")

  phenotype_routes <- do.call(rbind, lapply(results, function(x) {
    if (is.null(x$phenotype_routes)) return(NULL)
    cbind(contrast_id = x$contrast_id, x$phenotype_routes, row.names = NULL)
  }))
  if (is.null(phenotype_routes)) {
    phenotype_routes <- data.frame(
      contrast_id = character(), population = character(),
      analysis_arm = character(), medication_epilepsy_route = character(),
      n_people = numeric(), stringsAsFactors = FALSE
    )
  }
  aou_write_explicit_csv(
    phenotype_routes,
    file.path(outdir, "08_medication_epilepsy_phenotype_routes.csv"),
    count_cols = "n_people"
  )

  matching <- do.call(rbind, lapply(results, function(x) {
    if (is.null(x$matching)) return(NULL)
    cbind(contrast_id = x$contrast_id, x$matching, row.names = NULL)
  }))
  if (is.null(matching)) matching <- data.frame(contrast_id = character())
  aou_write_explicit_csv(
    matching, file.path(outdir, "04_matching_sensitivity_design.csv"),
    count_cols = intersect(c("n_comparator", "n_exposure"), names(matching)),
    dependent_cols = intersect(
      c("retention_comparator", "retention_exposure", "maximum_asmd"),
      names(matching)
    )
  )

  hard_stops <- data.frame(
    comparison = epibone_hard_stops(), gate = "STOP_FEASIBILITY_NOT_MODELLED",
    reason = "At least one prespecified arm or event-support cell was 1-20 in AoU feasibility.",
    stringsAsFactors = FALSE
  )
  aou_write_explicit_csv(hard_stops, file.path(outdir, "05_hard_stops.csv"))

  calendar_counts <- do.call(rbind, lapply(results, function(x) {
    if (is.null(x$calendar_table)) return(NULL)
    cbind(contrast_id = x$contrast_id, x$calendar_table, row.names = NULL)
  }))
  if (is.null(calendar_counts)) {
    calendar_counts <- data.frame(contrast_id = character(), calendar_band = integer(),
                                  n_0 = numeric(), n_1 = numeric())
  }
  aou_write_explicit_csv(
    calendar_counts, file.path(outdir, "06_calendar_band_counts.csv"),
    count_cols = intersect(c("n_0", "n_1"), names(calendar_counts))
  )

  actions <- do.call(rbind, lapply(results, function(x) {
    if (is.null(x$covariate_actions) || !length(x$covariate_actions)) return(NULL)
    rows <- do.call(rbind, x$covariate_actions)
    cbind(contrast_id = x$contrast_id, rows, row.names = NULL)
  }))
  if (is.null(actions)) {
    actions <- data.frame(
      contrast_id = character(), covariate = character(), action = character(),
      detail = character(), n_level_comparator = numeric(),
      n_level_exposure = numeric(), n_removed_comparator = numeric(),
      n_removed_exposure = numeric(), stringsAsFactors = FALSE
    )
  }
  aou_write_explicit_csv(
    actions, file.path(outdir, "07_covariate_actions.csv"),
    count_cols = c(
      "n_level_comparator", "n_level_exposure",
      "n_removed_comparator", "n_removed_exposure"
    )
  )
  aou_write_explicit_csv(
    data.frame(
      status = "SUPERSEDED_BY_HOTFIX04",
      replacement = "07_covariate_actions.csv",
      explanation = paste0(
        "Sparse supported covariates are now retained in the PS; exact collinearity and ",
        "one-arm-only levels are handled and audited explicitly."
      ), stringsAsFactors = FALSE
    ),
    file.path(outdir, "07_sparse_covariate_actions.csv")
  )

  private <- lapply(results, function(x) {
    if (identical(x$status, "PROCEED")) {
      list(overlap = x$private_design,
           matching = if (!is.null(x$private_matching)) x$private_matching else NULL)
    } else NULL
  })
  aou_write_private_rds(private, file.path(EPIBONE_ROOT, "private_enclave_only",
                                           "accepted_designs.rds"))
  epibone_write_manifest("design", c(
    "01_design_gates.csv", "02_weighted_balance.csv",
    "03_ingredient_composition.csv", "04_matching_sensitivity_design.csv",
    "05_hard_stops.csv", "06_calendar_band_counts.csv",
    "07_covariate_actions.csv", "07_sparse_covariate_actions.csv",
    "08_medication_epilepsy_phenotype_routes.csv"
  ))
  message("Stage A design diagnostics complete. No outcome model was fitted.")
  invisible(list(gates = gates, results = results))
}
