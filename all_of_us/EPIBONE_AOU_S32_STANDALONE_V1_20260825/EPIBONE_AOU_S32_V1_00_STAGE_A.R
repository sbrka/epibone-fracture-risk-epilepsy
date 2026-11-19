# EPI-BONE All of Us S32 standalone v1.0: Stage A extraction and audit.
# The Authorized Data User runs this inside RStudio. It deliberately stops
# after creating aggregate audits and one enclave-only participant checkpoint.

S32V1_HOME <- path.expand("~")
old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
setwd(S32V1_HOME)
source("~/s32v1_common.R", local = FALSE, encoding = "UTF-8")
source("~/s32v1_definitions.R", local = FALSE, encoding = "UTF-8")

cat("\nS32 v1 Stage A: building the directional landmark cohort ...\n")
asm_sql <- s32v1_sql_strings(S32V1_ASM_INGREDIENTS)
all_drugs <- unique(c(S32V1_ASM_INGREDIENTS, S32V1_BONE_RX_INGREDIENTS))
DRUG_MAP_SQL <- glue::glue("
WITH requested AS (
  SELECT ingredient FROM UNNEST([{s32v1_sql_strings(all_drugs)}]) AS ingredient
)
SELECT r.ingredient AS requested_ingredient, c.concept_id, c.concept_name,
       c.vocabulary_id, c.concept_class_id, c.standard_concept
FROM requested r
LEFT JOIN {s32v1_tbl('concept')} c
  ON c.vocabulary_id='RxNorm' AND c.concept_class_id='Ingredient'
  AND c.standard_concept='S' AND LOWER(c.concept_name)=r.ingredient
ORDER BY requested_ingredient, c.concept_id
")
DRUG_MAP <- as.data.table(s32v1_fix_int64(s32v1_bq(DRUG_MAP_SQL)))
critical_ingredients <- c(
  "levetiracetam", "lamotrigine", "lacosamide", "carbamazepine", "phenytoin",
  "phenobarbital"
)
missing_critical <- setdiff(
  critical_ingredients, unique(DRUG_MAP[!is.na(concept_id), requested_ingredient])
)
if (length(missing_critical)) {
  stop(
    "Critical RxNorm ingredients did not resolve in the current CDR: ",
    paste(missing_critical, collapse = ", "), call. = FALSE
  )
}
utils::write.csv(
  as.data.frame(DRUG_MAP), file.path(S32V1_HOME, "results", "s32v1_08_drug_ingredient_audit.csv"),
  row.names = FALSE
)

COHORT_SQL <- glue::glue("
WITH epi_src AS (
  SELECT concept_id, concept_code
  FROM {s32v1_tbl('concept')}
  WHERE vocabulary_id='ICD10CM'
    AND (STARTS_WITH(concept_code,'G40') OR STARTS_WITH(concept_code,'R56'))
),
epi_events AS (
  SELECT co.person_id, co.condition_start_date AS dt, s.concept_code
  FROM {s32v1_tbl('condition_occurrence')} co
  JOIN epi_src s ON co.condition_source_concept_id=s.concept_id
  WHERE co.condition_start_date IS NOT NULL
),
first_epi AS (
  SELECT person_id, MIN(dt) AS index_date,
         ARRAY_AGG(concept_code ORDER BY dt, concept_code LIMIT 1)[OFFSET(0)] AS first_code
  FROM epi_events GROUP BY person_id
),
confirmation AS (
  SELECT f.person_id, MIN(e.dt) AS confirmation_date
  FROM first_epi f JOIN epi_events e ON e.person_id=f.person_id
  WHERE e.dt BETWEEN DATE_ADD(f.index_date, INTERVAL 7 DAY)
                 AND DATE_ADD(f.index_date, INTERVAL {S32V1_LANDMARK_DAYS} DAY)
  GROUP BY f.person_id
),
g40_flag AS (
  SELECT f.person_id,
         MAX(IF(STARTS_WITH(e.concept_code,'G40'),1,0)) AS has_g40_by_landmark
  FROM first_epi f JOIN epi_events e ON e.person_id=f.person_id
  WHERE e.dt <= DATE_ADD(f.index_date, INTERVAL {S32V1_LANDMARK_DAYS} DAY)
  GROUP BY f.person_id
),
op_pick AS (
  SELECT f.person_id, op.observation_period_start_date AS op_start,
         op.observation_period_end_date AS op_end
  FROM first_epi f
  JOIN {s32v1_tbl('observation_period')} op ON op.person_id=f.person_id
    AND op.observation_period_start_date <= DATE_SUB(f.index_date, INTERVAL {S32V1_WASHOUT_DAYS} DAY)
    AND op.observation_period_end_date >= DATE_ADD(f.index_date, INTERVAL {S32V1_LANDMARK_DAYS} DAY)
  QUALIFY ROW_NUMBER() OVER (
    PARTITION BY f.person_id ORDER BY op.observation_period_start_date DESC,
                                     op.observation_period_end_date DESC
  )=1
),
asm_ing AS (
  SELECT concept_id, LOWER(concept_name) AS ingredient
  FROM {s32v1_tbl('concept')}
  WHERE vocabulary_id='RxNorm' AND concept_class_id='Ingredient'
    AND standard_concept='S' AND LOWER(concept_name) IN ({asm_sql})
),
asm_codes AS (
  SELECT concept_id AS drug_concept_id, ingredient FROM asm_ing
  UNION DISTINCT
  SELECT ca.descendant_concept_id AS drug_concept_id, i.ingredient
  FROM {s32v1_tbl('concept_ancestor')} ca
  JOIN asm_ing i ON ca.ancestor_concept_id=i.concept_id
),
asm_events AS (
  SELECT de.person_id, de.drug_exposure_start_date AS dt, a.ingredient
  FROM {s32v1_tbl('drug_exposure')} de
  JOIN asm_codes a ON de.drug_concept_id=a.drug_concept_id
  WHERE de.drug_exposure_start_date IS NOT NULL
),
asm_summary AS (
  SELECT f.person_id,
    MIN(IF(a.dt BETWEEN f.index_date AND DATE_ADD(f.index_date, INTERVAL {S32V1_LANDMARK_DAYS} DAY),
           a.dt, NULL)) AS asm_start,
    COUNT(DISTINCT IF(a.dt BETWEEN DATE_SUB(f.index_date, INTERVAL {S32V1_WASHOUT_DAYS} DAY)
                                      AND DATE_SUB(f.index_date, INTERVAL 1 DAY),
                      a.ingredient, NULL)) AS prior_asm_ingredients,
    COUNT(DISTINCT IF(a.dt BETWEEN f.index_date
                                      AND DATE_ADD(f.index_date, INTERVAL {S32V1_LANDMARK_DAYS} DAY),
                      a.ingredient, NULL)) AS landmark_asm_ingredients
  FROM first_epi f LEFT JOIN asm_events a ON a.person_id=f.person_id
  GROUP BY f.person_id
),
death_first AS (
  SELECT person_id, MIN(death_date) AS death_date
  FROM {s32v1_tbl('death')} GROUP BY person_id
),
prior_visits AS (
  SELECT f.person_id, COUNT(DISTINCT v.visit_start_date) AS prior_visit_days
  FROM first_epi f
  LEFT JOIN {s32v1_tbl('visit_occurrence')} v ON v.person_id=f.person_id
    AND v.visit_start_date BETWEEN DATE_SUB(f.index_date, INTERVAL 365 DAY)
                               AND DATE_SUB(f.index_date, INTERVAL 1 DAY)
  GROUP BY f.person_id
)
SELECT f.person_id, f.index_date,
       DATE_ADD(f.index_date, INTERVAL {S32V1_LANDMARK_DAYS} DAY) AS landmark_date,
       f.first_code, c.confirmation_date, COALESCE(g.has_g40_by_landmark,0) AS has_g40_by_landmark,
       op.op_start, op.op_end, a.asm_start,
       COALESCE(a.prior_asm_ingredients,0) AS prior_asm_ingredients,
       COALESCE(a.landmark_asm_ingredients,0) AS landmark_asm_ingredients,
       d.death_date, COALESCE(v.prior_visit_days,0) AS prior_visit_days,
       p.year_of_birth, cg.concept_name AS sex, cr.concept_name AS race,
       ce.concept_name AS ethnicity, EXTRACT(YEAR FROM f.index_date) AS index_year,
       EXTRACT(YEAR FROM f.index_date)-p.year_of_birth AS age_index,
       IF(p.year_of_birth IS NOT NULL
          AND EXTRACT(YEAR FROM f.index_date)-p.year_of_birth BETWEEN 18 AND 120,1,0) AS adult_flag,
       IF(op.person_id IS NOT NULL,1,0) AS observation_flag,
       IF(c.confirmation_date IS NOT NULL,1,0) AS confirmation_flag,
       IF(a.asm_start IS NOT NULL,1,0) AS new_asm_flag,
       IF(COALESCE(a.prior_asm_ingredients,0)=0,1,0) AS washout_flag,
       IF(d.death_date IS NULL OR d.death_date>DATE_ADD(f.index_date, INTERVAL {S32V1_LANDMARK_DAYS} DAY),1,0)
         AS alive_landmark_flag
FROM first_epi f
JOIN {s32v1_tbl('person')} p ON p.person_id=f.person_id
LEFT JOIN confirmation c ON c.person_id=f.person_id
LEFT JOIN g40_flag g ON g.person_id=f.person_id
LEFT JOIN op_pick op ON op.person_id=f.person_id
LEFT JOIN asm_summary a ON a.person_id=f.person_id
LEFT JOIN death_first d ON d.person_id=f.person_id
LEFT JOIN prior_visits v ON v.person_id=f.person_id
LEFT JOIN {s32v1_tbl('concept')} cg ON cg.concept_id=p.gender_concept_id
LEFT JOIN {s32v1_tbl('concept')} cr ON cr.concept_id=p.race_concept_id
LEFT JOIN {s32v1_tbl('concept')} ce ON ce.concept_id=p.ethnicity_concept_id
")
s32v1_assert_restricted(COHORT_SQL, "standalone cohort build", allow_cohort_build = TRUE)
COH_RAW <- as.data.table(s32v1_fix_int64(s32v1_bq(COHORT_SQL)))
if (!nrow(COH_RAW)) stop("The cohort query returned no candidate participants.", call. = FALSE)

date_cols <- intersect(
  c("index_date", "landmark_date", "confirmation_date", "op_start", "op_end",
    "asm_start", "death_date"), names(COH_RAW)
)
for (name in date_cols) set(COH_RAW, j = name, value = as.Date(COH_RAW[[name]]))

stages <- list(
  candidate_first_G40_R56 = rep(TRUE, nrow(COH_RAW)),
  adult_18_120 = COH_RAW$adult_flag == 1L,
  observation_365d_through_landmark = COH_RAW$observation_flag == 1L,
  second_code_7_180d = COH_RAW$confirmation_flag == 1L,
  new_non_gabapentinoid_ASM_0_180d = COH_RAW$new_asm_flag == 1L,
  no_qualifying_ASM_prior_365d = COH_RAW$washout_flag == 1L,
  alive_at_common_180d_landmark = COH_RAW$alive_landmark_flag == 1L
)
keep <- rep(TRUE, nrow(COH_RAW)); attrition <- vector("list", length(stages))
for (i in seq_along(stages)) {
  keep <- keep & stages[[i]]
  attrition[[i]] <- data.frame(stage = names(stages)[i], n = sum(keep), stringsAsFactors = FALSE)
}
ATTRITION <- s32v1_safe_attrition(rbindlist(attrition))
s32v1_write_safe(ATTRITION, "s32v1_01_attrition.csv", count_cols = "n")

D <- copy(COH_RAW[keep])
if (nrow(D) < 100L) stop("Eligible standalone cohort has fewer than 100 participants.", call. = FALSE)
D[, `:=`(
  lm = as.Date(landmark_date),
  phenotype_version = S32V1_PHENOTYPE,
  stratum = ifelse(has_g40_by_landmark == 1L, "G40_by_landmark", "R56_only"),
  log_prior_visits = log1p(prior_visit_days),
  sex = s32v1_collapse_factor(sex),
  race = s32v1_collapse_factor(race),
  ethnicity = s32v1_collapse_factor(ethnicity)
)]

composition <- D[, .(n = .N), by = stratum]
if (any(composition$n >= 1L & composition$n <= AOU_MAX_SUPPRESSED_COUNT)) {
  composition[, n := NA_integer_]
}
s32v1_write_safe(composition, "s32v1_02_cohort_composition.csv", count_cols = "n")

cat("S32 v1 Stage A: pulling cohort-restricted condition history ...\n")
relevant_prefixes <- unique(c(
  "G40", "R56", "M80", "M81", "M83", "M85", "E55",
  unlist(S32V1_OUTCOMES, use.names = FALSE),
  unlist(S32V1_NEGATIVE_CONTROLS, use.names = FALSE),
  unlist(S32V1_BASELINE_CONDITIONS, use.names = FALSE)
))
prefix_sql <- paste(sprintf("STARTS_WITH(c.concept_code,'%s')", relevant_prefixes), collapse = " OR ")
COND_SQL <- glue::glue("
SELECT co.person_id, c.concept_code, c.concept_name, co.condition_start_date AS dt
FROM {s32v1_tbl('condition_occurrence')} co
JOIN {s32v1_tbl('concept')} c ON co.condition_source_concept_id=c.concept_id
WHERE c.vocabulary_id='ICD10CM' AND ({prefix_sql})
  AND {s32v1_id_in(D$person_id, 'co.person_id')}
")
s32v1_assert_restricted(COND_SQL, "condition history")
CH <- as.data.table(s32v1_fix_int64(s32v1_bq(COND_SQL)))
if (!nrow(CH)) stop("No relevant condition history was returned.", call. = FALSE)
CH[, dt := as.Date(dt)]
CH <- merge(
  CH, D[, .(person_id, index_date, lm, op_start, op_end)],
  by = "person_id", all.x = TRUE
)
CH <- CH[dt >= op_start & dt <= op_end]

CODE_AUDIT <- CH[, .(persons = uniqueN(person_id), records = .N),
                 by = .(concept_code, concept_name)][order(concept_code)]
s32v1_write_safe(
  CODE_AUDIT, "s32v1_03_condition_code_audit.csv",
  count_cols = c("persons", "records")
)

baseline_ids <- function(prefixes) {
  unique(CH[dt <= lm & s32v1_prefix_match(concept_code, prefixes), person_id])
}
for (name in names(S32V1_BASELINE_CONDITIONS)) {
  ids <- baseline_ids(S32V1_BASELINE_CONDITIONS[[name]])
  D[, (name) := as.integer(person_id %in% ids)]
}
D[, osteoporosis_baseline := as.integer(person_id %in% baseline_ids(c("M80", "M81")))]
D[, osteopenia_baseline := as.integer(person_id %in% baseline_ids("M85"))]
D[, metabolic_bone_baseline := as.integer(person_id %in% baseline_ids(c("M83", "E55")))]
D[, bone_disease_any := as.integer(
  osteoporosis_baseline == 1L | osteopenia_baseline == 1L | metabolic_bone_baseline == 1L
)]
D[, status_epilepticus_baseline := as.integer(
  person_id %in% unique(CH[dt <= lm & concept_code %in% S32V1_STATUS_CODES, person_id])
)]
D[, convulsive_seizure_baseline := as.integer(
  person_id %in% unique(CH[
    dt <= lm & s32v1_prefix_match(concept_code, c("G40.3", "G40.4", "R56")), person_id
  ])
)]
SZ <- CH[
  dt >= index_date - S32V1_WASHOUT_DAYS & dt <= lm &
    s32v1_prefix_match(concept_code, c("G40", "R56")),
  .(seizure_dates = uniqueN(dt)), by = person_id
]
D[, seizure_dates := 0L]
D[SZ, on = "person_id", seizure_dates := i.seizure_dates]
D[, seizure_rate := seizure_dates / ((S32V1_WASHOUT_DAYS + S32V1_LANDMARK_DAYS) / 365.25)]

cat("S32 v1 Stage A: pulling cohort-restricted medication history ...\n")
drug_sql <- s32v1_sql_strings(all_drugs)
DRUG_SQL <- glue::glue("
WITH ingredients AS (
  SELECT concept_id, LOWER(concept_name) AS ingredient
  FROM {s32v1_tbl('concept')}
  WHERE vocabulary_id='RxNorm' AND concept_class_id='Ingredient'
    AND standard_concept='S' AND LOWER(concept_name) IN ({drug_sql})
), drug_codes AS (
  SELECT concept_id AS drug_concept_id, ingredient FROM ingredients
  UNION DISTINCT
  SELECT ca.descendant_concept_id AS drug_concept_id, i.ingredient
  FROM {s32v1_tbl('concept_ancestor')} ca
  JOIN ingredients i ON ca.ancestor_concept_id=i.concept_id
)
SELECT de.person_id, d.ingredient, de.drug_exposure_start_date AS dt
FROM {s32v1_tbl('drug_exposure')} de
JOIN drug_codes d ON de.drug_concept_id=d.drug_concept_id
WHERE de.drug_exposure_start_date IS NOT NULL
  AND {s32v1_id_in(D$person_id, 'de.person_id')}
")
s32v1_assert_restricted(DRUG_SQL, "medication history")
DH <- as.data.table(s32v1_fix_int64(s32v1_bq(DRUG_SQL)))
if (!nrow(DH)) stop("No prespecified medication history was returned.", call. = FALSE)
DH[, dt := as.Date(dt)]
DH <- merge(DH, D[, .(person_id, index_date, lm, op_start)], by = "person_id", all.x = TRUE)
DH <- DH[dt >= op_start & dt <= lm]

D[, bone_active_therapy := as.integer(
  person_id %in% unique(DH[ingredient %in% S32V1_BONE_RX_INGREDIENTS, person_id])
)]
D[, enzyme_inducing_asm := as.integer(
  person_id %in% unique(DH[
    ingredient %in% S32V1_EIA_INGREDIENTS & dt >= index_date & dt <= lm, person_id
  ])
)]
ASM_N <- DH[
  ingredient %in% S32V1_ASM_INGREDIENTS & dt >= index_date & dt <= lm,
  .(asm_ingredient_n = uniqueN(ingredient)), by = person_id
]
D[, asm_ingredient_n := 0L]
D[ASM_N, on = "person_id", asm_ingredient_n := i.asm_ingredient_n]
D[, asm_polytherapy2 := as.integer(asm_ingredient_n >= 2L)]

EVENTS <- rbindlist(c(
  lapply(names(S32V1_OUTCOMES), function(name) {
    CH[s32v1_prefix_match(concept_code, S32V1_OUTCOMES[[name]]),
       .(person_id, dt, endpoint = name, endpoint_type = "primary")]
  }),
  lapply(names(S32V1_NEGATIVE_CONTROLS), function(name) {
    CH[s32v1_prefix_match(concept_code, S32V1_NEGATIVE_CONTROLS[[name]]),
       .(person_id, dt, endpoint = name, endpoint_type = "negative_control")]
  })
), fill = TRUE)
EVENTS <- unique(EVENTS, by = c("person_id", "dt", "endpoint", "endpoint_type"))

endpoint_audit <- rbindlist(lapply(
  c(names(S32V1_OUTCOMES), names(S32V1_NEGATIVE_CONTROLS)), function(ep_name) {
    kind <- if (ep_name %in% names(S32V1_OUTCOMES)) "primary" else "negative_control"
    e <- EVENTS[endpoint == ep_name]
    prev <- unique(e[D[, .(person_id, lm)], on = "person_id"][dt <= lm, person_id])
    at_risk <- D[!person_id %in% prev, person_id]
    inc <- unique(e[D[, .(person_id, lm, op_end)], on = "person_id"][
      person_id %in% at_risk & dt > lm & dt <= op_end, person_id
    ])
    data.table(
      endpoint = ep_name, endpoint_type = kind, prevalent_n = length(prev),
      at_risk_n = length(at_risk), incident_n = length(inc)
    )
  }
))
s32v1_write_safe(
  endpoint_audit, "s32v1_04_endpoint_support.csv",
  count_cols = c("prevalent_n", "at_risk_n", "incident_n")
)

EXPOSURE_AUDIT <- rbindlist(lapply(seq_len(nrow(S32V1_EXPOSURE_PLAN)), function(i) {
  ex <- S32V1_EXPOSURE_PLAN$exposure[i]
  typ <- S32V1_EXPOSURE_PLAN$type[i]
  x <- D[[ex]]
  data.table(
    exposure = ex, exposure_group = S32V1_EXPOSURE_PLAN$exposure_group[i], type = typ,
    available_n = sum(!is.na(x)), positive_n = if (typ == "binary") sum(x == 1L, na.rm = TRUE) else NA_integer_,
    negative_n = if (typ == "binary") sum(x == 0L, na.rm = TRUE) else NA_integer_,
    median = if (typ == "continuous") median(x, na.rm = TRUE) else NA_real_,
    q25 = if (typ == "continuous") unname(quantile(x, .25, na.rm = TRUE)) else NA_real_,
    q75 = if (typ == "continuous") unname(quantile(x, .75, na.rm = TRUE)) else NA_real_
  )
}))
s32v1_write_safe(
  EXPOSURE_AUDIT, "s32v1_05_exposure_support.csv",
  count_cols = c("available_n", "positive_n", "negative_n"),
  complementary_sets = list(c("positive_n", "negative_n"))
)

cat("S32 v1 Stage A: checking current Fitbit summary-table schema ...\n")
SCHEMA_SQL <- sprintf(
  "SELECT table_name, column_name, data_type FROM `%s.INFORMATION_SCHEMA.COLUMNS` WHERE table_name IN ('activity_summary','sleep_daily_summary') ORDER BY table_name, ordinal_position",
  CDR
)
SCHEMA <- as.data.table(s32v1_bq(SCHEMA_SQL))
FB_ACTIVITY <- data.table(); FB_SLEEP <- data.table()

activity_person <- s32v1_resolve_column(SCHEMA, "activity_summary", c("person_id"))
activity_date <- s32v1_resolve_column(SCHEMA, "activity_summary", c("date", "activity_date"))
steps_col <- s32v1_resolve_column(SCHEMA, "activity_summary", c("steps", "total_steps"))
sed_col <- s32v1_resolve_column(
  SCHEMA, "activity_summary", c("sedentary_minutes", "minutes_sedentary")
)
fair_col <- s32v1_resolve_column(
  SCHEMA, "activity_summary", c("fairly_active_minutes", "minutes_fairly_active")
)
very_col <- s32v1_resolve_column(
  SCHEMA, "activity_summary", c("very_active_minutes", "minutes_very_active")
)
if (!is.na(activity_person) && !is.na(activity_date)) {
  pcol <- s32v1_safe_identifier(activity_person)
  dcol <- s32v1_safe_identifier(activity_date)
  optional <- character()
  if (!is.na(steps_col)) optional <- c(optional, sprintf(
    "AVG(SAFE_CAST(%s AS FLOAT64)) AS mean_steps", s32v1_safe_identifier(steps_col)
  ))
  if (!is.na(sed_col)) optional <- c(optional, sprintf(
    "AVG(SAFE_CAST(%s AS FLOAT64)) AS mean_sedentary_minutes", s32v1_safe_identifier(sed_col)
  ))
  if (!is.na(fair_col)) optional <- c(optional, sprintf(
    "AVG(SAFE_CAST(%s AS FLOAT64)) AS mean_fairly_active_minutes", s32v1_safe_identifier(fair_col)
  ))
  if (!is.na(very_col)) optional <- c(optional, sprintf(
    "AVG(SAFE_CAST(%s AS FLOAT64)) AS mean_very_active_minutes", s32v1_safe_identifier(very_col)
  ))
  optional_sql <- if (length(optional)) paste0(",\n       ", paste(optional, collapse = ",\n       ")) else ""
  ACTIVITY_SQL <- sprintf(
    paste0(
      "SELECT %s AS person_id, COUNT(DISTINCT DATE(%s)) AS wear_days, ",
      "MIN(DATE(%s)) AS first_wear_date, MAX(DATE(%s)) AS last_wear_date%s ",
      "FROM %s WHERE %s GROUP BY person_id"
    ), pcol, dcol, dcol, dcol, optional_sql, s32v1_tbl("activity_summary"),
    s32v1_id_in(D$person_id, activity_person)
  )
  s32v1_assert_restricted(ACTIVITY_SQL, "Fitbit activity summary")
  FB_ACTIVITY <- as.data.table(s32v1_fix_int64(s32v1_bq(ACTIVITY_SQL)))
  if (nrow(FB_ACTIVITY)) {
    FB_ACTIVITY[, `:=`(
      first_wear_date = as.Date(first_wear_date), last_wear_date = as.Date(last_wear_date)
    )]
  }
}

sleep_person <- s32v1_resolve_column(SCHEMA, "sleep_daily_summary", c("person_id"))
sleep_date <- s32v1_resolve_column(SCHEMA, "sleep_daily_summary", c("date", "sleep_date"))
if (!is.na(sleep_person) && !is.na(sleep_date)) {
  pcol <- s32v1_safe_identifier(sleep_person)
  dcol <- s32v1_safe_identifier(sleep_date)
  SLEEP_SQL <- sprintf(
    paste0(
      "SELECT %s AS person_id, COUNT(DISTINCT DATE(%s)) AS sleep_days, ",
      "MIN(DATE(%s)) AS first_sleep_date, MAX(DATE(%s)) AS last_sleep_date ",
      "FROM %s WHERE %s GROUP BY person_id"
    ), pcol, dcol, dcol, dcol, s32v1_tbl("sleep_daily_summary"),
    s32v1_id_in(D$person_id, sleep_person)
  )
  s32v1_assert_restricted(SLEEP_SQL, "Fitbit sleep summary")
  FB_SLEEP <- as.data.table(s32v1_fix_int64(s32v1_bq(SLEEP_SQL)))
  if (nrow(FB_SLEEP)) {
    FB_SLEEP[, `:=`(
      first_sleep_date = as.Date(first_sleep_date), last_sleep_date = as.Date(last_sleep_date)
    )]
  }
}

if (nrow(FB_ACTIVITY) && nrow(FB_SLEEP)) {
  FB <- merge(FB_ACTIVITY, FB_SLEEP, by = "person_id", all = TRUE)
} else if (nrow(FB_ACTIVITY)) {
  FB <- copy(FB_ACTIVITY)
} else if (nrow(FB_SLEEP)) {
  FB <- copy(FB_SLEEP)
} else {
  FB <- data.table()
}
if (nrow(FB)) {
  if (all(c("first_wear_date", "last_wear_date") %in% names(FB))) {
    FB[, wear_span_days := as.numeric(last_wear_date - first_wear_date) + 1]
  }
  fb_audit <- data.table(
    metric = "cohort_with_fitbit_summary", n = nrow(FB),
    median_wear_days = if ("wear_days" %in% names(FB)) median(FB$wear_days, na.rm = TRUE) else NA_real_,
    median_wear_span_days = if ("wear_span_days" %in% names(FB)) median(FB$wear_span_days, na.rm = TRUE) else NA_real_,
    median_sleep_days = if ("sleep_days" %in% names(FB)) median(FB$sleep_days, na.rm = TRUE) else NA_real_
  )
} else {
  fb_audit <- data.table(
    metric = "cohort_with_fitbit_summary", n = 0L, median_wear_days = NA_real_,
    median_wear_span_days = NA_real_, median_sleep_days = NA_real_
  )
}
s32v1_write_safe(fb_audit, "s32v1_06_fitbit_feasibility.csv", count_cols = "n", support_cols = "n")
utils::write.csv(
  as.data.frame(SCHEMA), file.path(S32V1_HOME, "results", "s32v1_07_fitbit_schema.csv"),
  row.names = FALSE
)

CHECKPOINT <- list(
  version = S32V1_VERSION, phenotype_version = S32V1_PHENOTYPE,
  cdr = CDR, billing = BILLING, created_on = Sys.time(), cohort = D,
  events = EVENTS, fitbit = FB, exposure_plan = S32V1_EXPOSURE_PLAN,
  primary_outcomes = names(S32V1_OUTCOMES),
  negative_controls = names(S32V1_NEGATIVE_CONTROLS)
)
s32v1_write_private(CHECKPOINT, "epibone_s32_v1_stage_a.rds")

stop(
  paste0(
    "S32 v1 STAGE A COMPLETE and stopped as designed. Review ~/results/s32v1_01_attrition.csv ",
    "through s32v1_08_drug_ingredient_audit.csv. Confirm the phenotype direction, exact G40 status ",
    "leaves, code names, outcome support, exposure support, Fitbit schema, and RxNorm mappings. ",
    "If approved, run:\n",
    "Sys.setenv(EPIBONE_S32_V1_AUDIT_APPROVED='true')\n",
    "source('~/EPIBONE_AOU_S32_V1_01_RESUME_AFTER_AUDIT.R', encoding='UTF-8')"
  ), call. = FALSE
)
