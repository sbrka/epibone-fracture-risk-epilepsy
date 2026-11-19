epibone_cov_drug_map_ctes <- function(prefix = "covdrug") {
  requests <- epibone_covariate_drug_requests()
  structs <- paste(
    epibone_struct_rows(requests, c("med_group", "ingredient_name")),
    collapse = ",\n      "
  )
  paste0(
    prefix, "_requests AS (\n",
    "  SELECT * FROM UNNEST([\n      ", structs, "\n  ])\n),\n",
    prefix, "_ingredients AS (\n",
    "  SELECT DISTINCT r.med_group, r.ingredient_name, ing.concept_id AS ingredient_concept_id\n",
    "  FROM ", prefix, "_requests r\n",
    "  JOIN ", epibone_table("concept"), " ing\n",
    "    ON ing.vocabulary_id='RxNorm' AND ing.concept_class_id='Ingredient'\n",
    "   AND ing.standard_concept='S' AND LOWER(ing.concept_name)=r.ingredient_name\n",
    "),\n",
    prefix, "_map AS (\n",
    "  SELECT DISTINCT i.med_group, i.ingredient_name, ca.descendant_concept_id AS drug_concept_id\n",
    "  FROM ", prefix, "_ingredients i\n",
    "  JOIN ", epibone_table("concept_ancestor"), " ca\n",
    "    ON ca.ancestor_concept_id=i.ingredient_concept_id\n",
    "  UNION DISTINCT\n",
    "  SELECT med_group, ingredient_name, ingredient_concept_id\n",
    "  FROM ", prefix, "_ingredients\n",
    ")"
  )
}

epibone_model_vocabulary_sql <- function() {
  cond <- epibone_condition_map_ctes(
    requests = epibone_model_condition_requests(), prefix = "cond"
  )
  asm <- epibone_drug_map_ctes(prefix = "drug")
  covdrug <- epibone_cov_drug_map_ctes(prefix = "covdrug")
  paste0(
    "WITH ", cond, ",\n", asm, ",\n", covdrug, ",\n",
    "condition_summary AS (\n",
    "  SELECT 'condition' AS domain, r.set_name AS set_name,\n",
    "    CONCAT(r.vocabulary_id, ':SOURCE_PREFIX_REGISTRY') AS requested_value,\n",
    "    COUNT(DISTINCT s.source_concept_id) AS n_source_concepts,\n",
    "    CAST(NULL AS INT64) AS n_standard_concepts,\n",
    "    COUNT(DISTINCT s.source_concept_id) AS n_mapped_concepts\n",
    "  FROM cond_requests r\n",
    "  LEFT JOIN cond_source_map s\n",
    "    ON s.set_name=r.set_name AND s.vocabulary_id=r.vocabulary_id\n",
    "  GROUP BY r.set_name, r.vocabulary_id\n",
    "),\n",
    "asm_summary AS (\n",
    "  SELECT 'asm_ingredient', r.abbreviation, r.ingredient_name,\n",
    "    COUNT(DISTINCT i.ingredient_concept_id), COUNT(DISTINCT i.ingredient_concept_id),\n",
    "    COUNT(DISTINCT m.drug_concept_id)\n",
    "  FROM drug_requests r\n",
    "  LEFT JOIN drug_ingredients i\n",
    "    ON i.abbreviation=r.abbreviation AND i.group_name=r.group_name AND i.role=r.role\n",
    "   AND LOWER(i.ingredient_concept_name)=r.ingredient_name\n",
    "  LEFT JOIN drug_map m\n",
    "    ON m.abbreviation=i.abbreviation AND m.group_name=i.group_name AND m.role=i.role\n",
    "   AND m.ingredient_concept_id=i.ingredient_concept_id\n",
    "  GROUP BY r.abbreviation, r.ingredient_name\n",
    "),\n",
    "covdrug_summary AS (\n",
    "  SELECT 'covariate_ingredient', r.med_group, r.ingredient_name,\n",
    "    COUNT(DISTINCT i.ingredient_concept_id), COUNT(DISTINCT i.ingredient_concept_id),\n",
    "    COUNT(DISTINCT m.drug_concept_id)\n",
    "  FROM covdrug_requests r\n",
    "  LEFT JOIN covdrug_ingredients i\n",
    "    ON i.med_group=r.med_group AND i.ingredient_name=r.ingredient_name\n",
    "  LEFT JOIN covdrug_map m\n",
    "    ON m.med_group=i.med_group AND m.ingredient_name=i.ingredient_name\n",
    "  GROUP BY r.med_group, r.ingredient_name\n",
    ")\n",
    "SELECT * FROM condition_summary\n",
    "UNION ALL SELECT * FROM asm_summary\n",
    "UNION ALL SELECT * FROM covdrug_summary\n",
    "ORDER BY domain, set_name, requested_value"
  )
}

epibone_run_model_vocabulary_audit <- function() {
  aou_ducc_preflight()
  message("Auditing frozen condition and RxNorm mappings (vocabulary only)...")
  audit <- as.data.frame(epibone_bq(epibone_model_vocabulary_sql()))
  numeric_cols <- c("n_source_concepts", "n_standard_concepts", "n_mapped_concepts")
  for (name in intersect(numeric_cols, names(audit))) {
    audit[[name]] <- suppressWarnings(as.numeric(audit[[name]]))
  }
  condition_failed <- audit$domain == "condition" & (
    !is.finite(audit$n_source_concepts) | audit$n_source_concepts < 1 |
      !is.finite(audit$n_mapped_concepts) | audit$n_mapped_concepts < 1
  )
  asm_failed <- audit$domain == "asm_ingredient" & (
    !is.finite(audit$n_mapped_concepts) | audit$n_mapped_concepts < 1
  )
  failed <- condition_failed | asm_failed
  outdir <- file.path(EPIBONE_ROOT, "results", "model_vocabulary")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  aou_write_explicit_csv(audit, file.path(outdir, "01_model_vocabulary_audit.csv"))
  exceptions <- epibone_validate_mapping_exceptions(audit, failed)
  message("Auditing the exact executable ICD source-code registry (vocabulary only)...")
  registry <- as.data.frame(epibone_bq(epibone_condition_source_registry_sql()))
  aou_write_explicit_csv(
    registry, file.path(outdir, "03_condition_source_code_registry.csv")
  )
  expected <- unique(epibone_model_condition_requests()[
    , c("set_name", "vocabulary_id", "pattern"), drop = FALSE
  ])
  registry_key <- paste(
    registry$set_name, registry$vocabulary_id, registry$requested_pattern, sep = "|"
  )
  expected$key <- paste(
    expected$set_name, expected$vocabulary_id, expected$pattern, sep = "|"
  )
  expected$n_source_code_concepts <- vapply(
    expected$key, function(key) sum(registry_key == key), integer(1)
  )
  allowed_missing <- c(
    "stroke_prior|ICD10CM|I64%",
    "osteomalacia|ICD9CM|268.3%",
    paste0("cancer|ICD9CM|", c(166:169, 177:178), "%")
  )
  unexpected_missing <- setdiff(
    expected$key[expected$n_source_code_concepts == 0], allowed_missing
  )
  if (length(unexpected_missing)) {
    stop(
      "Unexpected direct-source ICD registry gaps: ",
      paste(unexpected_missing, collapse = "; "), call. = FALSE
    )
  }
  expected$status <- ifelse(
    expected$n_source_code_concepts > 0,
    "EXECUTABLE_DIRECT_SOURCE",
    ifelse(
      expected$key == "stroke_prior|ICD10CM|I64%",
      "KNOWN_CDR_GAP_UNSPECIFIED_STROKE_UNDERASCERTAINED",
      "KNOWN_CDR_GAP_NO_SOURCE_CONCEPT"
    )
  )
  coverage <- expected[, c(
    "set_name", "vocabulary_id", "pattern", "n_source_code_concepts", "status"
  )]
  aou_write_explicit_csv(
    coverage,
    file.path(outdir, "05_condition_source_pattern_coverage.csv"),
    count_cols = character()
  )
  if ("stroke_prior|ICD10CM|I64%" %in%
      expected$key[expected$n_source_code_concepts == 0]) {
    exceptions <- rbind(
      exceptions,
      data.frame(
        domain = "condition", set_name = "stroke_prior",
        requested_value = "I64%",
        disposition = "NO_DIRECT_SOURCE_CONCEPT_IN_CDR; I60-I63_RETAINED",
        rationale = paste0(
          "The current C2025Q4R6 concept registry contains no executable ICD10CM I64 ",
          "source concept. The baseline stroke covariate therefore uses direct I60-I63 ",
          "source concepts; no SNOMED or substitute concept was introduced."
        ),
        interpretation = paste0(
          "Unspecified prior stroke may be under-ascertained in baseline adjustment; ",
          "eligibility and outcomes are unaffected."
        ),
        stringsAsFactors = FALSE
      )
    )
  }
  aou_write_explicit_csv(
    exceptions, file.path(outdir, "02_mapping_exceptions.csv")
  )
  aou_write_explicit_csv(
    data.frame(
      artifact = "results/checkpoint_repair/01_condition_vocabulary_map.csv",
      status = "LEGACY_AUDIT_ONLY_NOT_EXECUTABLE_AFTER_HOTFIX05",
      replacement = "results/model_vocabulary/03_condition_source_code_registry.csv",
      rationale = paste0(
        "The earlier checkpoint described ICD-to-standard/SNOMED mappings. Definitive ",
        "cohort membership now uses direct ICD source concepts only."
      ), stringsAsFactors = FALSE
    ),
    file.path(outdir, "04_legacy_condition_map_disposition.csv")
  )
  epibone_write_manifest(
    "model_vocabulary",
    c("01_model_vocabulary_audit.csv", "02_mapping_exceptions.csv",
      "03_condition_source_code_registry.csv",
      "04_legacy_condition_map_disposition.csv",
      "05_condition_source_pattern_coverage.csv")
  )
  invisible(list(summary = audit, source_registry = registry, coverage = coverage))
}

epibone_condition_source_registry_sql <- function() {
  cond <- epibone_condition_map_ctes(
    requests = epibone_model_condition_requests(), prefix = "cond"
  )
  paste0(
    "WITH ", cond, "\n",
    "SELECT set_name, vocabulary_id, pattern AS requested_pattern, source_code,\n",
    "  source_name, source_concept_id,\n",
    "  'condition_source_concept_id' AS executable_column,\n",
    "  'DIRECT_SOURCE_ONLY_NO_SNOMED_EXPANSION' AS match_mode\n",
    "FROM cond_source_map\n",
    "ORDER BY set_name, vocabulary_id, source_code"
  )
}

epibone_validate_mapping_exceptions <- function(audit, failed = NULL) {
  audit <- as.data.frame(audit, stringsAsFactors = FALSE)
  numeric_cols <- c("n_source_concepts", "n_standard_concepts", "n_mapped_concepts")
  for (name in intersect(numeric_cols, names(audit))) {
    audit[[name]] <- suppressWarnings(as.numeric(audit[[name]]))
  }
  if (is.null(failed)) {
    condition_failed <- audit$domain == "condition" & (
      !is.finite(audit$n_source_concepts) | audit$n_source_concepts < 1 |
        !is.finite(audit$n_mapped_concepts) | audit$n_mapped_concepts < 1
    )
    asm_failed <- audit$domain == "asm_ingredient" & (
      !is.finite(audit$n_mapped_concepts) | audit$n_mapped_concepts < 1
    )
    failed <- condition_failed | asm_failed
  }
  key <- paste(audit$domain, audit$set_name, audit$requested_value, sep = "|")
  allowed <- c(
    "asm_ingredient|VPA|valproic acid"
  )
  unexpected <- setdiff(key[failed], allowed)
  missing_expected <- setdiff(allowed, key[failed])
  if (length(unexpected) || length(missing_expected)) {
    stop(
      "Frozen vocabulary mapping has an unapproved failure pattern. Unexpected: ",
      paste(unexpected, collapse = "; "), ". Expected-but-not-seen: ",
      paste(missing_expected, collapse = "; "), call. = FALSE
    )
  }
  required <- c(
    "asm_ingredient|VPA|valproate"
  )
  required_rows <- match(required, key)
  if (anyNA(required_rows) || any(!is.finite(audit$n_mapped_concepts[required_rows])) ||
      any(audit$n_mapped_concepts[required_rows] < 1)) {
    stop("The executable valproate fallback member did not map.",
         call. = FALSE)
  }
  data.frame(
    domain = "asm_ingredient",
    set_name = "VPA",
    requested_value = "valproic acid",
    disposition = "UNMAPPED_DUPLICATE_LABEL_OMITTED; MAPPED_VALPROATE_MEMBER_RETAINED",
    rationale = c(
      paste0(
        "Pre-outcome vocabulary audit: the exact valproic acid Ingredient label was absent, ",
        "while the prespecified valproate VPA member mapped. Feasibility already used the ",
        "same executable mapping; no new concept was substituted."
      )
    ),
    interpretation = "VPA-LTG remains exploratory and uses the executable valproate descendant set.",
    stringsAsFactors = FALSE
  )
}

epibone_set_predicate <- function(alias, set_names) {
  values <- strsplit(set_names, "|", fixed = TRUE)[[1]]
  paste0(alias, ".set_name IN (", paste(vapply(values, epibone_sql_string, character(1)),
                                         collapse = ", "), ")")
}

epibone_condition_feature_sql <- function() {
  features <- list(
    hx_falls = c("falls_00_09", "falls_10_19"),
    hx_head_injury = "head_face_injury",
    hx_osteoporosis = c("osteoporosis_m80", "osteoporosis_m81"),
    hx_dementia = "dementia",
    hx_parkinsonism = "parkinsonism",
    hx_gait_disorder = "gait_disorder",
    hx_stroke = "stroke_prior",
    hx_tbi = "tbi",
    hx_convulsion = "convulsion",
    hx_intellectual_disability = "intellectual_disability",
    hx_ckd = "ckd",
    hx_liver_disease = "liver_disease",
    hx_diabetes = "diabetes",
    hx_hyperparathyroid = "hyperparathyroid",
    hx_thyroid_disease = "thyroid_disease",
    hx_adrenal_disorder = "adrenal_disorder",
    hx_malabsorption = "malabsorption",
    hx_inflammatory_arthritis = "inflammatory_arthritis",
    hx_inflammatory_bowel = "inflammatory_bowel",
    hx_cancer = "cancer",
    hx_smoking = "smoking",
    hx_alcohol_disorder = "alcohol_disorder",
    hx_substance_disorder = "substance_disorder",
    hx_depression = "depression",
    hx_anxiety = "anxiety",
    hx_hypertension = "hypertension",
    hx_atrial_fibrillation = "atrial_fibrillation",
    hx_heart_failure = "heart_failure",
    hx_ischaemic_heart = "ischaemic_heart",
    hx_copd = "copd",
    hx_obesity = "obesity",
    hx_peripheral_neuropathy = "peripheral_neuropathy"
  )
  paste(vapply(names(features), function(name) {
    sets <- paste(vapply(features[[name]], epibone_sql_string, character(1)), collapse = ", ")
    paste0("    MAX(IF(ce.set_name IN (", sets,
           ") AND ce.event_date<a.index_date, 1, 0)) AS ", name)
  }, character(1)), collapse = ",\n")
}

epibone_medication_feature_sql <- function() {
  groups <- unique(epibone_covariate_drug_requests()$med_group)
  paste(vapply(groups, function(name) {
    paste0(
      "    MAX(IF(me.med_group=", epibone_sql_string(name),
      " AND me.start_date>=DATE_SUB(a.index_date, INTERVAL 365 DAY)",
      " AND me.start_date<a.index_date, 1, 0)) AS prior_med_", name
    )
  }, character(1)), collapse = ",\n")
}

epibone_outcome_summary_sql <- function() {
  outcomes <- epibone_outcome_sets()
  blocks <- vapply(seq_len(nrow(outcomes)), function(i) {
    predicate <- epibone_set_predicate("ce", outcomes$set_names[i])
    paste0(
      "    COUNTIF(", predicate, " AND ce.event_date<=a.landmark_date) AS prior_",
      outcomes$outcome[i], ",\n",
      "    MIN(IF(", predicate, " AND ce.event_date>a.landmark_date, ce.event_date, NULL)) AS ",
      outcomes$outcome[i], "_date"
    )
  }, character(1))
  paste(blocks, collapse = ",\n")
}

epibone_model_cohort_sql <- function() {
  cond <- epibone_condition_map_ctes(
    requests = epibone_model_condition_requests(), prefix = "cond"
  )
  drug <- epibone_drug_map_ctes(prefix = "drug")
  covdrug <- epibone_cov_drug_map_ctes(prefix = "covdrug")
  condition_features <- epibone_condition_feature_sql()
  medication_features <- epibone_medication_feature_sql()
  outcome_features <- epibone_outcome_summary_sql()

  paste0(
    "WITH ", cond, ",\n", drug, ",\n", covdrug, ",\n",
    "condition_events AS (\n",
    "  SELECT DISTINCT co.person_id, co.condition_start_date AS event_date, m.set_name,\n",
    "    CASE WHEN REGEXP_CONTAINS(LOWER(COALESCE(vc.concept_name,'')), r'(emergency|inpatient)')\n",
    "      THEN 1 ELSE 0 END AS acute_setting\n",
    "  FROM ", epibone_table("condition_occurrence"), " co\n",
    "  JOIN cond_map m ON m.source_concept_id=co.condition_source_concept_id\n",
    "  LEFT JOIN ", epibone_table("visit_occurrence"), " vo USING(visit_occurrence_id)\n",
    "  LEFT JOIN ", epibone_table("concept"), " vc ON vc.concept_id=vo.visit_concept_id\n",
    "  WHERE co.condition_start_date IS NOT NULL\n",
    "),\n",
    "drug_raw AS (\n",
    "  SELECT DISTINCT de.person_id, de.drug_exposure_id,\n",
    "    de.drug_exposure_start_date AS start_date, m.abbreviation, m.group_name,\n",
    "    COALESCE(vc.concept_name,'') AS visit_name,\n",
    "    LOWER(CONCAT(COALESCE(route.concept_name,''), ' ', COALESCE(src.concept_name,''),\n",
    "      ' ', COALESCE(prod.concept_name,''))) AS route_text\n",
    "  FROM ", epibone_table("drug_exposure"), " de\n",
    "  JOIN drug_map m ON m.drug_concept_id=de.drug_concept_id\n",
    "  LEFT JOIN ", epibone_table("concept"), " route ON route.concept_id=de.route_concept_id\n",
    "  LEFT JOIN ", epibone_table("concept"), " src ON src.concept_id=de.drug_source_concept_id\n",
    "  LEFT JOIN ", epibone_table("concept"), " prod ON prod.concept_id=de.drug_concept_id\n",
    "  LEFT JOIN ", epibone_table("visit_occurrence"), " vo USING(visit_occurrence_id)\n",
    "  LEFT JOIN ", epibone_table("concept"), " vc ON vc.concept_id=vo.visit_concept_id\n",
    "  WHERE de.drug_exposure_start_date IS NOT NULL\n",
    "),\n",
    "drug_events AS (\n",
    "  SELECT *, CASE WHEN REGEXP_CONTAINS(route_text, r'(oral|tablet|capsule|chewable)')\n",
    "      AND NOT REGEXP_CONTAINS(route_text, r'(inject|intraven|intramuscular)') THEN 1 ELSE 0 END\n",
    "      AS oral_identified,\n",
    "    CASE WHEN REGEXP_CONTAINS(LOWER(visit_name), r'(emergency|inpatient)') THEN 1 ELSE 0 END\n",
    "      AS acute_index_setting\n",
    "  FROM drug_raw\n",
    "),\n",
    "cov_med_events AS (\n",
    "  SELECT DISTINCT de.person_id, de.drug_exposure_start_date AS start_date, m.med_group\n",
    "  FROM ", epibone_table("drug_exposure"), " de\n",
    "  JOIN covdrug_map m ON m.drug_concept_id=de.drug_concept_id\n",
    "  WHERE de.drug_exposure_start_date IS NOT NULL\n",
    "),\n",
    "death_first AS (\n",
    "  SELECT person_id, MIN(death_date) AS death_date\n",
    "  FROM ", epibone_table("death"), " GROUP BY person_id\n",
    "),\n",
    "person_demo AS (\n",
    "  SELECT p.person_id, p.year_of_birth, COALESCE(g.concept_name,'Unknown') AS sex,\n",
    "    COALESCE(r.concept_name,'Unknown') AS race, COALESCE(e.concept_name,'Unknown') AS ethnicity\n",
    "  FROM ", epibone_table("person"), " p\n",
    "  LEFT JOIN ", epibone_table("concept"), " g ON g.concept_id=p.gender_concept_id\n",
    "  LEFT JOIN ", epibone_table("concept"), " r ON r.concept_id=p.race_concept_id\n",
    "  LEFT JOIN ", epibone_table("concept"), " e ON e.concept_id=p.ethnicity_concept_id\n",
    "),\n",

    "v1_first AS (\n",
    "  SELECT person_id,\n",
    "    MIN(IF(set_name='epilepsy',event_date,NULL)) AS epilepsy_date,\n",
    "    MIN(IF(set_name='migraine',event_date,NULL)) AS migraine_date,\n",
    "    MIN(IF(set_name='syncope',event_date,NULL)) AS syncope_date\n",
    "  FROM condition_events WHERE set_name IN ('epilepsy','migraine','syncope')\n",
    "  GROUP BY person_id\n",
    "),\n",
    "v1_indices AS (\n",
    "  SELECT person_id, 'epilepsy' AS arm, 'epilepsy' AS target_set, epilepsy_date AS index_date\n",
    "  FROM v1_first WHERE epilepsy_date IS NOT NULL UNION ALL\n",
    "  SELECT person_id, 'migraine', 'migraine', migraine_date FROM v1_first WHERE migraine_date IS NOT NULL UNION ALL\n",
    "  SELECT person_id, 'syncope', 'syncope', syncope_date FROM v1_first WHERE syncope_date IS NOT NULL\n",
    "),\n",
    "v1_base AS (\n",
    "  SELECT i.*, DATE_ADD(index_date, INTERVAL 30 DAY) AS landmark30,\n",
    "    DATE_ADD(index_date, INTERVAL 90 DAY) AS landmark90,\n",
    "    DATE_DIFF(index_date, DATE(p.year_of_birth,7,1),YEAR) AS age,\n",
    "    p.sex, p.race, p.ethnicity, d.death_date\n",
    "  FROM v1_indices i JOIN person_demo p USING(person_id) LEFT JOIN death_first d USING(person_id)\n",
    "),\n",
    "v1_obs AS (\n",
    "  SELECT x.*,\n",
    "    MIN(IF(op.observation_period_start_date<=DATE_SUB(index_date,INTERVAL 365 DAY)\n",
    "      AND op.observation_period_end_date>=landmark30,op.observation_period_start_date,NULL)) AS obs_start365,\n",
    "    MAX(IF(op.observation_period_start_date<=DATE_SUB(index_date,INTERVAL 365 DAY)\n",
    "      AND op.observation_period_end_date>=landmark30,op.observation_period_end_date,NULL)) AS obs_end365,\n",
    "    MIN(IF(op.observation_period_start_date<=DATE_SUB(index_date,INTERVAL 730 DAY)\n",
    "      AND op.observation_period_end_date>=landmark90,op.observation_period_start_date,NULL)) AS obs_start730,\n",
    "    MAX(IF(op.observation_period_start_date<=DATE_SUB(index_date,INTERVAL 730 DAY)\n",
    "      AND op.observation_period_end_date>=landmark90,op.observation_period_end_date,NULL)) AS obs_end730\n",
    "  FROM v1_base x LEFT JOIN ", epibone_table("observation_period"), " op USING(person_id)\n",
    "  GROUP BY person_id,arm,target_set,index_date,landmark30,landmark90,age,sex,race,ethnicity,death_date\n",
    "),\n",
    "v1_conditions AS (\n",
    "  SELECT x.person_id,x.arm,x.index_date,\n",
    "    COUNT(DISTINCT IF(ce.set_name=x.target_set AND ce.event_date BETWEEN x.index_date AND x.landmark90,ce.event_date,NULL)) AS confirmation_dates,\n",
    "    COUNTIF(ce.set_name IN ('epilepsy','convulsion') AND ce.event_date>=DATE_SUB(x.index_date,INTERVAL 365 DAY) AND ce.event_date<x.index_date) AS neuro_history365,\n",
    "    COUNTIF(ce.set_name IN ('epilepsy','convulsion') AND ce.event_date>=DATE_SUB(x.index_date,INTERVAL 730 DAY) AND ce.event_date<x.index_date) AS neuro_history730,\n",
    "    COUNTIF(ce.set_name='epilepsy' AND ce.event_date<=x.landmark30) AS epilepsy_through30,\n",
    "    COUNTIF(ce.set_name='epilepsy' AND ce.event_date<=x.landmark90) AS epilepsy_through90,\n",
    "    COUNTIF(ce.set_name='broad_fracture' AND ce.event_date<=x.landmark30) AS broad_through30,\n",
    "    COUNTIF(ce.set_name='broad_fracture' AND ce.event_date<=x.landmark90) AS broad_through90\n",
    "  FROM v1_obs x LEFT JOIN condition_events ce USING(person_id)\n",
    "  GROUP BY x.person_id,x.arm,x.index_date\n",
    "),\n",
    "v1_drugs AS (\n",
    "  SELECT x.person_id,x.arm,x.index_date,\n",
    "    COUNTIF(de.start_date>=DATE_SUB(x.index_date,INTERVAL 365 DAY) AND de.start_date<x.index_date) AS asm_history365,\n",
    "    COUNTIF(de.start_date>=DATE_SUB(x.index_date,INTERVAL 730 DAY) AND de.start_date<x.index_date) AS asm_history730\n",
    "  FROM v1_obs x LEFT JOIN drug_events de USING(person_id)\n",
    "  GROUP BY x.person_id,x.arm,x.index_date\n",
    "),\n",
    "v1_primary AS (\n",
    "  SELECT 'V1' AS module,'primary30' AS base_scenario,x.person_id,x.arm,CAST(NULL AS STRING) AS ingredient,\n",
    "    x.index_date,x.landmark30 AS landmark_date,x.age,x.sex,x.race,x.ethnicity,\n",
    "    x.obs_start365 AS obs_start,x.obs_end365 AS obs_end,x.death_date,0 AS index_acute,\n",
    "    c.confirmation_dates AS phenotype_days,CAST(NULL AS INT64) AS asm_ingredient_count,\n",
    "    IF(x.obs_end730 IS NOT NULL,1,0) AS obs730_flag,\n",
    "    CAST(NULL AS STRING) AS medication_epilepsy_route\n",
    "  FROM v1_obs x JOIN v1_conditions c USING(person_id,arm,index_date) JOIN v1_drugs d USING(person_id,arm,index_date)\n",
    "  WHERE x.age>=18 AND x.obs_end365 IS NOT NULL AND c.neuro_history365=0 AND d.asm_history365=0\n",
    "    AND (x.arm='epilepsy' OR c.epilepsy_through30=0) AND c.broad_through30=0\n",
    "    AND (x.death_date IS NULL OR x.death_date>x.landmark30)\n",
    "),\n",
    "v1_strict AS (\n",
    "  SELECT 'V1' AS module,'strict90' AS base_scenario,x.person_id,x.arm,CAST(NULL AS STRING) AS ingredient,\n",
    "    x.index_date,x.landmark90 AS landmark_date,x.age,x.sex,x.race,x.ethnicity,\n",
    "    x.obs_start730 AS obs_start,x.obs_end730 AS obs_end,x.death_date,0 AS index_acute,\n",
    "    c.confirmation_dates AS phenotype_days,CAST(NULL AS INT64) AS asm_ingredient_count,\n",
    "    1 AS obs730_flag,CAST(NULL AS STRING) AS medication_epilepsy_route\n",
    "  FROM v1_obs x JOIN v1_conditions c USING(person_id,arm,index_date) JOIN v1_drugs d USING(person_id,arm,index_date)\n",
    "  WHERE x.age>=18 AND x.obs_end730 IS NOT NULL AND c.confirmation_dates>=2\n",
    "    AND c.neuro_history730=0 AND d.asm_history730=0\n",
    "    AND (x.arm='epilepsy' OR c.epilepsy_through90=0) AND c.broad_through90=0\n",
    "    AND (x.death_date IS NULL OR x.death_date>x.landmark90)\n",
    "),\n",

    "epi_dates AS (\n",
    "  SELECT person_id,event_date,ROW_NUMBER() OVER(PARTITION BY person_id ORDER BY event_date) AS rn\n",
    "  FROM (SELECT DISTINCT person_id,event_date FROM condition_events WHERE set_name='epilepsy')\n",
    "),\n",
    "v2_confirmed AS (\n",
    "  SELECT person_id,MAX(IF(rn=2,event_date,NULL)) AS index_date\n",
    "  FROM epi_dates WHERE rn<=2 GROUP BY person_id HAVING index_date IS NOT NULL\n",
    "),\n",
    "v2_base AS (\n",
    "  SELECT c.person_id,c.index_date,DATE_ADD(c.index_date,INTERVAL 365 DAY) AS landmark_date,\n",
    "    DATE_DIFF(c.index_date,DATE(p.year_of_birth,7,1),YEAR) AS age,p.sex,p.race,p.ethnicity,d.death_date\n",
    "  FROM v2_confirmed c JOIN person_demo p USING(person_id) LEFT JOIN death_first d USING(person_id)\n",
    "),\n",
    "v2_obs AS (\n",
    "  SELECT x.*,MIN(op.observation_period_start_date) AS obs_start,MAX(op.observation_period_end_date) AS obs_end\n",
    "  FROM v2_base x JOIN ", epibone_table("observation_period"), " op ON op.person_id=x.person_id\n",
    "    AND op.observation_period_start_date<=DATE_SUB(x.index_date,INTERVAL 365 DAY)\n",
    "    AND op.observation_period_end_date>=x.landmark_date\n",
    "  GROUP BY person_id,index_date,landmark_date,age,sex,race,ethnicity,death_date\n",
    "),\n",
    "v2_classified AS (\n",
    "  SELECT x.*,COUNT(DISTINCT IF(de.oral_identified=1 AND de.start_date BETWEEN x.index_date AND x.landmark_date,de.abbreviation,NULL)) AS n_asm\n",
    "  FROM v2_obs x LEFT JOIN drug_events de USING(person_id)\n",
    "  GROUP BY person_id,index_date,landmark_date,age,sex,race,ethnicity,death_date,obs_start,obs_end\n",
    "),\n",
    "v2_broad AS (\n",
    "  SELECT x.person_id,x.index_date,COUNTIF(ce.set_name='broad_fracture' AND ce.event_date<=x.landmark_date) AS broad_through_landmark\n",
    "  FROM v2_classified x LEFT JOIN condition_events ce USING(person_id) GROUP BY x.person_id,x.index_date\n",
    "),\n",
    "v2_anchor AS (\n",
    "  SELECT 'V2' AS module,'primary365' AS base_scenario,x.person_id,\n",
    "    CASE WHEN x.n_asm=0 THEN 'none_recorded' WHEN x.n_asm=1 THEN 'one_ingredient' ELSE 'multiple_ingredients' END AS arm,\n",
    "    CAST(NULL AS STRING) AS ingredient,x.index_date,x.landmark_date,x.age,x.sex,x.race,x.ethnicity,\n",
    "    x.obs_start,x.obs_end,x.death_date,0 AS index_acute,2 AS phenotype_days,\n",
    "    x.n_asm AS asm_ingredient_count,0 AS obs730_flag,\n",
    "    CAST(NULL AS STRING) AS medication_epilepsy_route\n",
    "  FROM v2_classified x JOIN v2_broad b USING(person_id,index_date)\n",
    "  WHERE x.age>=18 AND b.broad_through_landmark=0 AND (x.death_date IS NULL OR x.death_date>x.landmark_date)\n",
    "),\n",

    "v3_first AS (\n",
    "  SELECT person_id,MIN(start_date) AS index_date FROM drug_events WHERE oral_identified=1 GROUP BY person_id\n",
    "),\n",
    "v3_index_drugs AS (\n",
    "  SELECT f.person_id,f.index_date,ANY_VALUE(de.group_name) AS arm,ANY_VALUE(de.abbreviation) AS ingredient,\n",
    "    COUNT(DISTINCT de.abbreviation) AS n_oral_index_ingredients,MAX(de.acute_index_setting) AS index_acute\n",
    "  FROM v3_first f JOIN drug_events de ON de.person_id=f.person_id AND de.start_date=f.index_date AND de.oral_identified=1\n",
    "  GROUP BY f.person_id,f.index_date\n",
    "),\n",
    "v3_base AS (\n",
    "  SELECT i.*,DATE_ADD(i.index_date,INTERVAL 30 DAY) AS landmark_date,\n",
    "    DATE_DIFF(i.index_date,DATE(p.year_of_birth,7,1),YEAR) AS age,p.sex,p.race,p.ethnicity,d.death_date\n",
    "  FROM v3_index_drugs i JOIN person_demo p USING(person_id) LEFT JOIN death_first d USING(person_id)\n",
    "  WHERE i.ingredient IN ('CBZ','PHT','PHB','PRM','LEV','LTG','LCM','VPA')\n",
    "),\n",
    "v3_obs AS (\n",
    "  SELECT x.*,\n",
    "    MIN(IF(op.observation_period_start_date<=DATE_SUB(x.index_date,INTERVAL 365 DAY) AND op.observation_period_end_date>=x.landmark_date,op.observation_period_start_date,NULL)) AS obs_start,\n",
    "    MAX(IF(op.observation_period_start_date<=DATE_SUB(x.index_date,INTERVAL 365 DAY) AND op.observation_period_end_date>=x.landmark_date,op.observation_period_end_date,NULL)) AS obs_end,\n",
    "    MAX(IF(op.observation_period_start_date<=DATE_SUB(x.index_date,INTERVAL 730 DAY) AND op.observation_period_end_date>=x.landmark_date,1,0)) AS obs730_flag\n",
    "  FROM v3_base x LEFT JOIN ", epibone_table("observation_period"), " op USING(person_id)\n",
    "  GROUP BY person_id,index_date,arm,ingredient,n_oral_index_ingredients,index_acute,landmark_date,age,sex,race,ethnicity,death_date\n",
    "),\n",
    "v3_conditions AS (\n",
    "  SELECT x.person_id,x.index_date,\n",
    "    COUNT(DISTINCT IF(ce.set_name='epilepsy' AND ce.event_date<=x.index_date,ce.event_date,NULL)) AS n_epi_dates,\n",
    "    COUNT(DISTINCT IF(ce.set_name='convulsion' AND ce.event_date BETWEEN DATE_SUB(x.index_date,INTERVAL 180 DAY) AND x.index_date,ce.event_date,NULL)) AS n_r56_dates_180,\n",
    "    COUNTIF(ce.set_name IN ('bipolar','trigeminal_neuralgia') AND ce.event_date<x.index_date) AS alternative_indications,\n",
    "    COUNTIF(ce.set_name='broad_fracture' AND ce.event_date<=x.landmark_date) AS broad_through_landmark\n",
    "  FROM v3_obs x LEFT JOIN condition_events ce USING(person_id) GROUP BY x.person_id,x.index_date\n",
    "),\n",
    "v3_drugs AS (\n",
    "  SELECT x.person_id,x.index_date,\n",
    "    COUNT(DISTINCT IF(de.start_date=x.index_date,de.abbreviation,NULL)) AS n_all_index_ingredients,\n",
    "    COUNTIF(de.start_date>=DATE_SUB(x.index_date,INTERVAL 365 DAY) AND de.start_date<x.index_date) AS prior_asm\n",
    "  FROM v3_obs x LEFT JOIN drug_events de USING(person_id) GROUP BY x.person_id,x.index_date\n",
    "),\n",
    "v3_anchor AS (\n",
    "  SELECT 'V3' AS module,'exploratory_broad30' AS base_scenario,x.person_id,x.arm,x.ingredient,x.index_date,x.landmark_date,\n",
    "    x.age,x.sex,x.race,x.ethnicity,x.obs_start,x.obs_end,x.death_date,x.index_acute,\n",
    "    c.n_epi_dates AS phenotype_days,CAST(NULL AS INT64) AS asm_ingredient_count,x.obs730_flag,\n",
    "    CASE WHEN c.n_epi_dates>=1 THEN 'DIRECT_G40_OR_ICD9_345'\n",
    "      ELSE 'R56_OR_ICD9_7803_THEN_INDEX_ASM_0_180D' END AS medication_epilepsy_route\n",
    "  FROM v3_obs x JOIN v3_conditions c USING(person_id,index_date) JOIN v3_drugs d USING(person_id,index_date)\n",
    "  WHERE x.age>=18 AND x.obs_end IS NOT NULL AND (c.n_epi_dates>=1 OR c.n_r56_dates_180>=1)\n",
    "    AND c.alternative_indications=0\n",
    "    AND c.broad_through_landmark=0 AND x.n_oral_index_ingredients=1 AND d.n_all_index_ingredients=1 AND d.prior_asm=0\n",
    "    AND (x.death_date IS NULL OR x.death_date>x.landmark_date)\n",
    "),\n",
    "anchors AS (\n",
    "  SELECT * FROM v1_primary UNION ALL SELECT * FROM v1_strict UNION ALL\n",
    "  SELECT * FROM v2_anchor UNION ALL SELECT * FROM v3_anchor\n",
    "),\n",
    "baseline_conditions AS (\n",
    "  SELECT a.module,a.base_scenario,a.person_id,a.arm,a.index_date,\n",
    condition_features, ",\n",
    "    COUNT(DISTINCT IF(ce.set_name='epilepsy' AND ce.event_date<a.index_date,ce.event_date,NULL)) AS prior_epilepsy_days\n",
    "  FROM anchors a LEFT JOIN condition_events ce USING(person_id)\n",
    "  GROUP BY a.module,a.base_scenario,a.person_id,a.arm,a.index_date\n",
    "),\n",
    "baseline_meds AS (\n",
    "  SELECT a.module,a.base_scenario,a.person_id,a.arm,a.index_date,\n",
    medication_features, "\n",
    "  FROM anchors a LEFT JOIN cov_med_events me USING(person_id)\n",
    "  GROUP BY a.module,a.base_scenario,a.person_id,a.arm,a.index_date\n",
    "),\n",
    "visit_util AS (\n",
    "  SELECT a.module,a.base_scenario,a.person_id,a.arm,a.index_date,\n",
    "    COUNT(DISTINCT IF(vo.visit_start_date>=DATE_SUB(a.index_date,INTERVAL 365 DAY) AND vo.visit_start_date<a.index_date,vo.visit_start_date,NULL)) AS visit_days_pre,\n",
    "    COUNT(DISTINCT IF(vo.visit_start_date>=DATE_SUB(a.index_date,INTERVAL 365 DAY) AND vo.visit_start_date<a.index_date\n",
    "      AND REGEXP_CONTAINS(LOWER(COALESCE(vc.concept_name,'')),r'(emergency|inpatient)'),vo.visit_start_date,NULL)) AS acute_visit_days_pre,\n",
    "    COUNT(DISTINCT IF(a.module='V2' AND vo.visit_start_date BETWEEN a.index_date AND a.landmark_date\n",
    "      AND REGEXP_CONTAINS(LOWER(COALESCE(vc.concept_name,'')),r'(emergency|inpatient)'),vo.visit_start_date,NULL)) AS acute_visit_days_assessment\n",
    "  FROM anchors a LEFT JOIN ", epibone_table("visit_occurrence"), " vo USING(person_id)\n",
    "  LEFT JOIN ", epibone_table("concept"), " vc ON vc.concept_id=vo.visit_concept_id\n",
    "  GROUP BY a.module,a.base_scenario,a.person_id,a.arm,a.index_date\n",
    "),\n",
    "condition_util AS (\n",
    "  SELECT a.module,a.base_scenario,a.person_id,a.arm,a.index_date,\n",
    "    COUNT(DISTINCT IF(co.condition_start_date>=DATE_SUB(a.index_date,INTERVAL 365 DAY) AND co.condition_start_date<a.index_date,co.condition_start_date,NULL)) AS condition_days_pre,\n",
    "    COUNT(DISTINCT IF(a.module='V2' AND ce.set_name='epilepsy' AND ce.event_date BETWEEN a.index_date AND a.landmark_date,ce.event_date,NULL)) AS epilepsy_days_assessment\n",
    "  FROM anchors a LEFT JOIN ", epibone_table("condition_occurrence"), " co USING(person_id)\n",
    "  LEFT JOIN condition_events ce ON ce.person_id=a.person_id AND ce.event_date=co.condition_start_date\n",
    "  GROUP BY a.module,a.base_scenario,a.person_id,a.arm,a.index_date\n",
    "),\n",
    "drug_util AS (\n",
    "  SELECT a.module,a.base_scenario,a.person_id,a.arm,a.index_date,\n",
    "    COUNT(DISTINCT IF(de.drug_exposure_start_date>=DATE_SUB(a.index_date,INTERVAL 365 DAY) AND de.drug_exposure_start_date<a.index_date,de.drug_exposure_start_date,NULL)) AS drug_days_pre\n",
    "  FROM anchors a LEFT JOIN ", epibone_table("drug_exposure"), " de USING(person_id)\n",
    "  GROUP BY a.module,a.base_scenario,a.person_id,a.arm,a.index_date\n",
    "),\n",
    "outcomes AS (\n",
    "  SELECT a.module,a.base_scenario,a.person_id,a.arm,a.index_date,\n",
    outcome_features, "\n",
    "  FROM anchors a LEFT JOIN condition_events ce USING(person_id)\n",
    "  GROUP BY a.module,a.base_scenario,a.person_id,a.arm,a.index_date\n",
    ")\n",
    "SELECT a.*,EXTRACT(YEAR FROM a.index_date) AS index_year,\n",
    "  DATE_DIFF(a.index_date,a.obs_start,DAY) AS history_days,bc.* EXCEPT(module,base_scenario,person_id,arm,index_date),\n",
    "  bm.* EXCEPT(module,base_scenario,person_id,arm,index_date),\n",
    "  vu.* EXCEPT(module,base_scenario,person_id,arm,index_date),\n",
    "  cu.* EXCEPT(module,base_scenario,person_id,arm,index_date),\n",
    "  du.* EXCEPT(module,base_scenario,person_id,arm,index_date),\n",
    "  o.* EXCEPT(module,base_scenario,person_id,arm,index_date)\n",
    "FROM anchors a\n",
    "JOIN baseline_conditions bc USING(module,base_scenario,person_id,arm,index_date)\n",
    "JOIN baseline_meds bm USING(module,base_scenario,person_id,arm,index_date)\n",
    "JOIN visit_util vu USING(module,base_scenario,person_id,arm,index_date)\n",
    "JOIN condition_util cu USING(module,base_scenario,person_id,arm,index_date)\n",
    "JOIN drug_util du USING(module,base_scenario,person_id,arm,index_date)\n",
    "JOIN outcomes o USING(module,base_scenario,person_id,arm,index_date)"
  )
}

epibone_coerce_dates <- function(x) {
  date_cols <- names(x)[grepl("(_date$|^index_date$|^landmark_date$|^obs_start$|^obs_end$)",
                              names(x), perl = TRUE)]
  for (name in date_cols) {
    value <- x[[name]]
    if (inherits(value, "Date")) next
    if (inherits(value, "POSIXt")) {
      x[[name]] <- as.Date(value)
    } else if (inherits(value, "integer64") || is.numeric(value)) {
      x[[name]] <- as.Date(as.numeric(value), origin = "1970-01-01")
    } else {
      text <- as.character(value)
      text[text == ""] <- NA_character_
      x[[name]] <- as.Date(text)
    }
  }
  x
}

# BigQuery can reject the logically correct all-CTE query when its optimizer
# expands the reused anchor graph into too many stages. Convert the same ordered
# CTE graph into one multi-statement script of job-scoped TEMP tables. This
# changes materialisation only: definitions, eligibility, covariates and final
# SELECT remain byte-for-byte bodies from epibone_model_cohort_sql().
epibone_split_top_level_ctes <- function(sql) {
  text <- trimws(sql)
  if (!grepl("^WITH[[:space:]]", text)) {
    stop("Expected a top-level WITH query.", call. = FALSE)
  }
  text <- sub("^WITH[[:space:]]+", "", text)
  n <- nchar(text)
  position <- 1L
  ctes <- list()
  repeat {
    while (position <= n && grepl("[[:space:]]", substr(text, position, position))) {
      position <- position + 1L
    }
    remaining <- substr(text, position, n)
    match <- regexec(
      "^([A-Za-z_][A-Za-z0-9_]*)[[:space:]]+AS[[:space:]]*\\(",
      remaining, perl = TRUE
    )
    parts <- regmatches(remaining, match)[[1]]
    if (!length(parts)) {
      stop("Could not parse CTE at character ", position, call. = FALSE)
    }
    name <- parts[2]
    opening <- position + attr(match[[1]], "match.length")[1] - 1L
    depth <- 1L
    cursor <- opening + 1L
    in_single <- FALSE
    in_double <- FALSE
    in_backtick <- FALSE
    while (cursor <= n && depth > 0L) {
      char <- substr(text, cursor, cursor)
      next_char <- if (cursor < n) substr(text, cursor + 1L, cursor + 1L) else ""
      if (in_single) {
        if (char == "'" && next_char == "'") {
          cursor <- cursor + 2L
          next
        }
        if (char == "'") in_single <- FALSE
      } else if (in_double) {
        if (char == '"') in_double <- FALSE
      } else if (in_backtick) {
        if (char == "`") in_backtick <- FALSE
      } else if (char == "'") {
        in_single <- TRUE
      } else if (char == '"') {
        in_double <- TRUE
      } else if (char == "`") {
        in_backtick <- TRUE
      } else if (char == "(") {
        depth <- depth + 1L
      } else if (char == ")") {
        depth <- depth - 1L
      }
      cursor <- cursor + 1L
    }
    if (depth != 0L) stop("Unbalanced parentheses in CTE ", name, call. = FALSE)
    closing <- cursor - 1L
    ctes[[length(ctes) + 1L]] <- list(
      name = name, body = substr(text, opening + 1L, closing - 1L)
    )
    position <- closing + 1L
    while (position <= n && grepl("[[:space:]]", substr(text, position, position))) {
      position <- position + 1L
    }
    if (position <= n && substr(text, position, position) == ",") {
      position <- position + 1L
      next
    }
    final <- trimws(substr(text, position, n))
    break
  }
  names_cte <- vapply(ctes, `[[`, character(1), "name")
  if (anyDuplicated(names_cte) || !grepl("^SELECT[[:space:]]", final)) {
    stop("Invalid CTE graph or final SELECT.", call. = FALSE)
  }
  list(ctes = ctes, final = final)
}

epibone_model_cohort_staged_sql <- function() {
  logical_sql <- epibone_model_cohort_sql()
  parsed <- epibone_split_top_level_ctes(logical_sql)
  required <- c(
    "cond_map", "drug_map", "condition_events", "drug_events", "anchors",
    "baseline_conditions", "baseline_meds", "visit_util", "condition_util",
    "drug_util", "outcomes"
  )
  names_cte <- vapply(parsed$ctes, `[[`, character(1), "name")
  if (!all(required %in% names_cte)) stop("Staged SQL is missing required CTEs.", call. = FALSE)
  statements <- vapply(parsed$ctes, function(x) {
    paste0("CREATE TEMP TABLE `", x$name, "` AS\n", x$body, ";")
  }, character(1))
  paste(c(
    "-- EPI-BONE job-scoped materialisation; no persistent BigQuery table is created.",
    statements,
    paste0(parsed$final, ";")
  ), collapse = "\n\n")
}

epibone_build_or_resume_cohort <- function() {
  aou_ducc_preflight()
  private_dir <- file.path(EPIBONE_ROOT, "private_enclave_only")
  dir.create(private_dir, recursive = TRUE, showWarnings = FALSE)
  sql <- epibone_model_cohort_staged_sql()
  signature <- digest::digest(list(
    sql = sql, cdr = Sys.getenv("WORKSPACE_CDR"),
    phenotype = paste0(
      "direct_icd_source_concept_id_no_snomed_expansion_v1;",
      "medication_exploratory_g40_once_or_r56_then_new_asm_0_180d_v1"
    )
  ), algo = "sha256")
  cache <- file.path(private_dir, paste0("epibone_model_cohort_", signature, ".rds"))
  if (file.exists(cache)) {
    message("Reusing enclave-only model cohort checkpoint.")
    return(readRDS(cache))
  }
  message(paste0(
    "Submitting the single minimum-necessary person-level cohort extraction as ",
    "job-scoped staged TEMP tables..."
  ))
  cohort <- epibone_bq(sql)
  cohort <- epibone_coerce_dates(as.data.frame(cohort))
  aou_write_private_rds(cohort, cache)
  manifest <- list(
    cdr = Sys.getenv("WORKSPACE_CDR"), sql_sha256 = digest::digest(sql, algo = "sha256"),
    condition_match_mode = "DIRECT_ICD9CM_ICD10CM_SOURCE_CONCEPT_ID",
    cache = basename(cache), created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  aou_write_private_rds(manifest, file.path(private_dir, "model_cohort_manifest.rds"))
  cohort
}
