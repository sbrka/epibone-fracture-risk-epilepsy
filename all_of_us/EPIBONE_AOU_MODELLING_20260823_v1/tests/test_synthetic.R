script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- sub("^--file=", "", script_arg[1])
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
Sys.setenv(AOU_EPIBONE_ROOT = root, WORKSPACE_CDR = "test.dataset",
           GOOGLE_PROJECT = "test-project")
source(file.path(root, "R", "00_setup.R"))
source(file.path(root, "00_aou_ducc_guard.R"))
source(file.path(root, "R", "01_definitions.R"))
source(file.path(root, "R", "03_build_sql.R"))
source(file.path(root, "R", "04_design.R"))
source(file.path(root, "R", "05_outcomes.R"))
source(file.path(root, "R", "06_validate.R"))

# The explicit guard suppresses small participant cells but preserves labels.
safe <- aou_prepare_explicit_export(
  data.frame(label = c("small", "large", "zero"), n_people = c(7, 21, 0),
             estimate = c(1.2, 1.3, NA)),
  count_cols = "n_people", support_cols = "n_people", dependent_cols = "estimate"
)
stopifnot(is.na(safe$n_people[1]), is.na(safe$estimate[1]), safe$label[1] == "small",
          safe$n_people[2] == 21, safe$n_people[3] == 0)

# Calendar selection is count-only and chooses the longest supported run.
d <- data.frame(
  exposure = rep(c(0L, 1L), each = 150),
  index_year = rep(rep(c(2007L, 2012L, 2017L), each = 50), 2)
)
calendar <- epibone_calendar_window(d)
stopifnot(calendar$ok, calendar$start == 2005L, calendar$end == 2019L,
          nrow(calendar$data) == nrow(d))

# Overlap weighting recovers a balanced synthetic design.
set.seed(20260823)
n <- 800L
exposure <- rep(c(0L, 1L), each = n / 2)
x <- cbind(
  age_z = rnorm(n, 0.15 * exposure, 1),
  index_year_z = rnorm(n, 0.10 * exposure, 1),
  hx_falls = rbinom(n, 1, plogis(-1 + 0.15 * exposure)),
  utilisation_z = rnorm(n, 0.10 * exposure, 1)
)
synthetic <- data.frame(exposure = exposure)
design <- epibone_attempt_overlap_design(synthetic, x, "synthetic")
stopifnot(design$ok, all(design$retention >= 0.90), all(design$ess >= 100),
          is.finite(design$max_asmd))

# Matching positions must come from the input-order weight vector, not its
# arbitrary names, because enclave cohort subsets retain non-sequential rows.
recovered <- epibone_matchit_positions(
  list(weights = stats::setNames(c(0, 1, 0.5, 0), c("101", "205", "999", "1200"))),
  4L
)
stopifnot(
  identical(recovered$index, c(2L, 3L)),
  identical(recovered$weights, c(1, 0.5))
)
if (requireNamespace("MatchIt", quietly = TRUE)) {
  rownames(design$data) <- as.character(100000L + seq_len(nrow(design$data)))
  rownames(design$x) <- rownames(design$data)
  matched <- epibone_matching_sensitivity(design)
  stopifnot(
    length(matched$index) == length(matched$weights),
    all(matched$index >= 1L & matched$index <= nrow(design$data)),
    all(is.finite(matched$weights) & matched$weights > 0),
    is.finite(matched$summary$maximum_asmd)
  )
} else {
  message("MatchIt unavailable locally; input-order recovery helper tested directly.")
}

# The owner-approved broad medication phenotype route is an explicit baseline
# PS covariate whenever medication routes are populated; it is absent from V1/V2.
route_data <- data.frame(
  age = seq(40, 69),
  medication_epilepsy_route = rep(c(
    "DIRECT_G40_OR_ICD9_345",
    "R56_OR_ICD9_7803_THEN_INDEX_ASM_0_180D"
  ), 15),
  stringsAsFactors = FALSE
)
route_x <- epibone_covariate_frame(route_data)
stopifnot(
  "medication_epilepsy_route_r56_supported" %in% colnames(route_x),
  identical(
    as.integer(route_x[, "medication_epilepsy_route_r56_supported"]),
    rep(c(0L, 1L), 15)
  )
)
nonmedication_data <- route_data
nonmedication_data$medication_epilepsy_route <- NA_character_
nonmedication_x <- epibone_covariate_frame(nonmedication_data)
stopifnot(!"medication_epilepsy_route_r56_supported" %in% colnames(nonmedication_x))

# Supported sparse predictors remain in the PS, one-arm-only levels define
# empirical nonoverlap, and exact duplicate columns are pruned deterministically.
support_d <- data.frame(exposure = rep(c(0L, 1L), each = 100L))
support_x <- cbind(
  clinical_supported_sparse = c(rep(1, 5), rep(0, 95), rep(1, 5), rep(0, 95)),
  one_arm_only = c(rep(0, 100), rep(0, 95), rep(1, 5)),
  common = rep(c(0, 1), 100)
)
support_x <- cbind(support_x, exact_duplicate = support_x[, "common"])
prepared <- epibone_filter_design_matrix(
  support_x, support_d$exposure, d = support_d
)
action_rows <- do.call(rbind, prepared$actions)
stopifnot(
  prepared$ok, nrow(prepared$data) == 195L,
  "clinical_supported_sparse" %in% colnames(prepared$x),
  any(action_rows$action == "TRIMMED_ONE_ARM_ONLY_LEVEL"),
  any(action_rows$action == "RETAINED_SUPPORTED_SPARSE_LEVEL_IN_PS"),
  any(action_rows$action == "DROPPED_EXACT_COLLINEARITY_QR")
)

# Cause-specific and absolute-risk estimators return finite values.
set.seed(12)
outcome <- data.frame(
  person_id = seq_len(n), exposure = exposure,
  time = sample(30:1095, n, replace = TRUE),
  event = rbinom(n, 1, 0.25), overlap_weight = rep(1, n)
)
outcome$status <- ifelse(outcome$event == 1, 1L, rbinom(n, 1, 0.10) * 2L)
cox <- epibone_weighted_cox(outcome)
cif0 <- epibone_weighted_cif(outcome[outcome$exposure == 0, ])
stopifnot(all(is.finite(cox)), is.finite(cif0), cif0 >= 0, cif0 <= 1)

lock <- epibone_validate_static_lock()
stopifnot(all(lock$passed))

mapping_audit <- data.frame(
  domain = c("asm_ingredient", "asm_ingredient"),
  set_name = c("VPA", "VPA"),
  requested_value = c("valproic acid", "valproate"),
  n_source_concepts = c(0, 1),
  n_standard_concepts = c(0, 1),
  n_mapped_concepts = c(0, 10)
)
mapping_exceptions <- epibone_validate_mapping_exceptions(mapping_audit)
stopifnot(nrow(mapping_exceptions) == 1)

condition_ctes <- epibone_condition_map_ctes(
  epibone_model_condition_requests(), prefix = "cond"
)
logical_cohort_sql <- epibone_model_cohort_sql()
stopifnot(
  grepl("ICD10CM", condition_ctes, fixed = TRUE),
  grepl("ICD9CM", condition_ctes, fixed = TRUE),
  !grepl("concept_ancestor", condition_ctes, fixed = TRUE),
  grepl("m.source_concept_id=co.condition_source_concept_id",
        logical_cohort_sql, fixed = TRUE),
  !grepl("m.concept_id=co.condition_concept_id", logical_cohort_sql, fixed = TRUE),
  grepl(
    "ce.set_name='convulsion' AND ce.event_date BETWEEN DATE_SUB(x.index_date,INTERVAL 180 DAY) AND x.index_date",
    logical_cohort_sql, fixed = TRUE
  ),
  grepl(
    "c.n_epi_dates>=1 OR c.n_r56_dates_180>=1",
    logical_cohort_sql, fixed = TRUE
  ),
  grepl("exploratory_broad30", logical_cohort_sql, fixed = TRUE),
  lengths(regmatches(
    logical_cohort_sql,
    gregexpr("medication_epilepsy_route", logical_cohort_sql, fixed = TRUE)
  )) == 4L,
  epibone_contrast_plan()$role[
    epibone_contrast_plan()$contrast_id == "V3_EIA_VS_SELECTED_NEIA"
  ] == "exploratory_medication"
)

# The preweighting composition audit must use grouping vectors of equal length.
composition_n <- 25L
composition <- stats::aggregate(
  rep(1L, composition_n),
  by = list(
    population = rep("calendar_eligible_preweighting", composition_n),
    analysis_arm = rep(c("canonical_eia", "selected_neia"), length.out = composition_n),
    ingredient = rep(c("CBZ", "LEV"), length.out = composition_n)
  ),
  FUN = sum
)
stopifnot(sum(composition$x) == composition_n)

staged_sql <- epibone_model_cohort_staged_sql()
parsed_cohort <- epibone_split_top_level_ctes(epibone_model_cohort_sql())
stopifnot(
  length(parsed_cohort$ctes) == 44L,
  grepl("CREATE TEMP TABLE `anchors`", staged_sql, fixed = TRUE),
  grepl("FROM anchors a", parsed_cohort$final, fixed = TRUE)
)
cat("All synthetic and static-lock tests passed.\n")
