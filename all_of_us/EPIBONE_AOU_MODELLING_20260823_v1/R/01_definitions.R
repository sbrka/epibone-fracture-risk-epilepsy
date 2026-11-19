epibone_condition_requests <- function() {
  data.frame(
    set_name = c(
      "epilepsy", "convulsion", "migraine", "syncope",
      rep("broad_fracture", 8),
      rep("mof", 8),
      "osteoporosis_m80", "osteoporosis_m81", "osteomalacia", "other_bone_density",
      "falls_00_09", "falls_10_19", "head_face_injury",
      "bipolar", "trigeminal_neuralgia",
      "stroke_sah", "stroke_ich", "stroke_ischaemic", "tbi",
      rep("dementia", 4), "parkinsonism", "gait_disorder",
      "nc_ingrowing_nail", "nc_chalazion", "nc_onychomycosis",
      "nc_benign_skin_neoplasm", "nc_presbyopia", "nc_intestinal_polyp",
      "nc_skin_scar", "nc_seborrhoeic_keratosis", "nc_viral_conjunctivitis",
      "nc_acute_tonsillitis"
    ),
    pattern = c(
      "G40%", "R56%", "G43%", "R55%",
      "S12%", "S22%", "S32%", "S42%", "S52%", "S62%", "S72%", "S82%",
      "S72.0%", "S72.1%", "S72.2%", "S22.0%", "S32.0%", "S52.5%", "S52.6%", "S42.2%",
      "M80%", "M81%", "M83%", "M85%",
      "W0%", "W1%", "S0%",
      "F31%", "G50.0%",
      "I60%", "I61%", "I63%", "S06%",
      "F01%", "F02%", "F03%", "G30%", "G20%", "R26%",
      "L60.0%", "H00.1%", "B35.1%", "D23%", "H52.4%", "K63.5%",
      "L90.5%", "L82%", "B30%", "J03%"
    ),
    vocabulary_id = "ICD10CM",
    stringsAsFactors = FALSE
  )
}

# Direct source-code companions used in the earlier audited AoU epilepsy
# analyses. These are matched only to condition_source_concept_id; they are
# never mapped to, or expanded through, a SNOMED hierarchy. Repeated source
# codes across clinically related sets are deliberate (for example, the same
# ICD-9 osteoporosis family supports both M80 and M81-era records).
epibone_icd9_condition_requests <- function() {
  spec <- list(
    epilepsy = "345%",
    convulsion = "780.3%",
    migraine = "346%",
    syncope = "780.2%",
    broad_fracture = c(
      "805.0%", "805.1%", "805.2%", "805.3%", "805.4%", "805.5%", "805.6%",
      "806.0%", "806.1%", "806.2%", "806.3%", "806.4%", "806.5%", "806.6%",
      "807%", "808%", "810%", "811%", "812%", "813%", "814%", "815%",
      "816%", "817%", "820%", "821%", "823%", "824%"
    ),
    mof = c(
      "820%", "805.2%", "805.3%", "806.2%", "806.3%", "805.4%", "805.5%",
      "806.4%", "806.5%", "813.4%", "813.5%", "812.0%", "812.1%"
    ),
    osteoporosis_m80 = "733.0%",
    osteoporosis_m81 = "733.0%",
    osteomalacia = c("268.2%", "268.3%"),
    other_bone_density = "733.9%",
    falls_00_09 = paste0("E", 880:884, "%"),
    falls_10_19 = paste0("E", 885:888, "%"),
    head_face_injury = paste0(c(
      800:804, 850:854, 870:873, 910:918, 920, 921, 925, 950, 951
    ), "%"),
    bipolar = c("296.4%", "296.5%", "296.6%", "296.7%", "296.8%"),
    trigeminal_neuralgia = "350.1%",
    stroke_sah = "430%",
    stroke_ich = "431%",
    stroke_ischaemic = c("433%", "434%"),
    tbi = paste0(850:854, "%"),
    dementia = c("290%", "294.1%", "331.0%"),
    parkinsonism = "332%",
    gait_disorder = "781.2%",
    nc_ingrowing_nail = "703.0%",
    nc_chalazion = "373.2%",
    nc_onychomycosis = "110.1%",
    nc_benign_skin_neoplasm = "216%",
    nc_presbyopia = "367.4%",
    nc_intestinal_polyp = c("211.3%", "211.4%"),
    nc_skin_scar = "709.2%",
    nc_seborrhoeic_keratosis = "702.1%",
    nc_viral_conjunctivitis = "077%",
    nc_acute_tonsillitis = "463%",
    intellectual_disability = c("317%", "318%", "319%"),
    ckd = "585%",
    liver_disease = paste0(570:573, "%"),
    diabetes = c("249%", "250%"),
    hyperparathyroid = "252.0%",
    thyroid_disease = c("242%", "244%"),
    adrenal_disorder = "255%",
    malabsorption = "579%",
    inflammatory_arthritis = "714.0%",
    inflammatory_bowel = c("555%", "556%"),
    cancer = paste0(140:209, "%"),
    smoking = c("305.1%", "V15.82%"),
    alcohol_disorder = c("303%", "305.0%"),
    substance_disorder = c("304%", paste0("305.", 2:9, "%")),
    depression = c("296.2%", "296.3%", "311%"),
    anxiety = c("300.0%", "300.2%"),
    hypertension = "401%",
    atrial_fibrillation = "427.3%",
    heart_failure = "428%",
    ischaemic_heart = paste0(410:414, "%"),
    copd = c("491%", "492%", "496%"),
    obesity = "278.0%",
    peripheral_neuropathy = c("356%", "357%"),
    stroke_prior = paste0(430:436, "%")
  )
  do.call(rbind, lapply(names(spec), function(name) {
    data.frame(
      set_name = name, pattern = spec[[name]], vocabulary_id = "ICD9CM",
      stringsAsFactors = FALSE
    )
  }))
}

epibone_drug_requests <- function() {
  data.frame(
    abbreviation = c(
      "CBZ", "PHT", "PHB", "PRM", "LEV", "LTG", "LCM", "OXC", "TPM", "ZNS",
      "VPA", "VPA", "BRV", "CNB", "PER", "ESL", "GBP", "PGB", "CLB", "RUF",
      "CBD", "ESM", "FBM", "VGB", "STP", "FOS"
    ),
    ingredient_name = c(
      "carbamazepine", "phenytoin", "phenobarbital", "primidone",
      "levetiracetam", "lamotrigine", "lacosamide", "oxcarbazepine",
      "topiramate", "zonisamide", "valproic acid", "valproate",
      "brivaracetam", "cenobamate", "perampanel", "eslicarbazepine",
      "gabapentin", "pregabalin", "clobazam", "rufinamide", "cannabidiol",
      "ethosuximide", "felbamate", "vigabatrin", "stiripentol", "fosphenytoin"
    ),
    group_name = c(
      "canonical_eia", "canonical_eia", "canonical_eia", "eia_sensitivity",
      "selected_neia", "selected_neia", "selected_neia",
      rep("other_asm", 19)
    ),
    role = c(
      rep("primary_exposure", 3), "sensitivity_exposure",
      rep("primary_reference", 3), rep("washout_only", 19)
    ),
    stringsAsFactors = FALSE
  )
}

epibone_condition_map_ctes <- function(requests = epibone_condition_requests(),
                                       prefix = "cond") {
  if (!"vocabulary_id" %in% names(requests)) requests$vocabulary_id <- "ICD10CM"
  structs <- paste(epibone_struct_rows(
    requests, c("set_name", "vocabulary_id", "pattern")
  ),
                   collapse = ",\n      ")
  paste0(
    prefix, "_requests AS (\n",
    "  SELECT * FROM UNNEST([\n      ", structs, "\n  ])\n),\n",
    prefix, "_source_map AS (\n",
    "  SELECT DISTINCT r.set_name, r.vocabulary_id, r.pattern,\n",
    "    src.concept_id AS source_concept_id, src.concept_code AS source_code,\n",
    "    src.concept_name AS source_name\n",
    "  FROM ", prefix, "_requests r\n",
    "  JOIN ", epibone_table("concept"), " src\n",
    "    ON src.vocabulary_id=r.vocabulary_id AND src.concept_code LIKE r.pattern\n",
    "),\n",
    prefix, "_map AS (\n",
    "  SELECT DISTINCT set_name, source_concept_id, vocabulary_id, source_code, source_name\n",
    "  FROM ", prefix, "_source_map\n",
    ")"
  )
}

epibone_drug_map_ctes <- function(requests = epibone_drug_requests(), prefix = "drug") {
  structs <- paste(
    epibone_struct_rows(requests, c("abbreviation", "ingredient_name", "group_name", "role")),
    collapse = ",\n      "
  )
  paste0(
    prefix, "_requests AS (\n",
    "  SELECT * FROM UNNEST([\n      ", structs, "\n  ])\n),\n",
    prefix, "_ingredients AS (\n",
    "  SELECT DISTINCT r.abbreviation, r.group_name, r.role,\n",
    "    ing.concept_id AS ingredient_concept_id, ing.concept_name AS ingredient_concept_name\n",
    "  FROM ", prefix, "_requests r\n",
    "  JOIN ", epibone_table("concept"), " ing\n",
    "    ON ing.vocabulary_id='RxNorm' AND ing.concept_class_id='Ingredient'\n",
    "   AND ing.standard_concept='S' AND LOWER(ing.concept_name)=r.ingredient_name\n",
    "),\n",
    prefix, "_map AS (\n",
    "  SELECT DISTINCT i.abbreviation, i.group_name, i.role, i.ingredient_concept_id,\n",
    "    i.ingredient_concept_name, ca.descendant_concept_id AS drug_concept_id\n",
    "  FROM ", prefix, "_ingredients i\n",
    "  JOIN ", epibone_table("concept_ancestor"), " ca\n",
    "    ON ca.ancestor_concept_id=i.ingredient_concept_id\n",
    "  UNION DISTINCT\n",
    "  SELECT abbreviation, group_name, role, ingredient_concept_id,\n",
    "    ingredient_concept_name, ingredient_concept_id AS drug_concept_id\n",
    "  FROM ", prefix, "_ingredients\n",
    ")"
  )
}

epibone_condition_audit_sql <- function() {
  ctes <- epibone_condition_map_ctes(
    requests = unique(rbind(epibone_condition_requests(),
                            epibone_icd9_condition_requests())), prefix = "cond"
  )
  paste0(
    "WITH ", ctes, ",\n",
    "source_counts AS (\n",
    "  SELECT s.set_name, s.source_concept_id,\n",
    "    COUNT(DISTINCT co.person_id) AS n_source_people, COUNT(*) AS n_source_records\n",
    "  FROM cond_source_map s\n",
    "  LEFT JOIN ", epibone_table("condition_occurrence"), " co\n",
    "    ON co.condition_source_concept_id=s.source_concept_id\n",
    "  GROUP BY s.set_name, s.source_concept_id\n",
    ")\n",
    "SELECT s.set_name, s.vocabulary_id, s.pattern, s.source_code, s.source_name,\n",
    "  s.source_concept_id, sc.n_source_people, sc.n_source_records,\n",
    "  'DIRECT_CONDITION_SOURCE_CONCEPT_ID' AS executable_match_mode\n",
    "FROM cond_source_map s\n",
    "LEFT JOIN source_counts sc USING(set_name, source_concept_id)\n",
    "ORDER BY set_name, vocabulary_id, source_code"
  )
}

epibone_drug_audit_sql <- function(route = FALSE) {
  ctes <- epibone_drug_map_ctes(prefix = "drug")
  if (!route) {
    return(paste0(
      "WITH ", ctes, "\n",
      "SELECT i.abbreviation, i.group_name, i.role, i.ingredient_concept_id,\n",
      "  i.ingredient_concept_name, COUNT(DISTINCT m.drug_concept_id) AS n_descendant_concepts,\n",
      "  COUNT(DISTINCT de.person_id) AS n_people, COUNT(de.person_id) AS n_records\n",
      "FROM drug_ingredients i\n",
      "LEFT JOIN drug_map m USING(abbreviation, group_name, role, ingredient_concept_id, ingredient_concept_name)\n",
      "LEFT JOIN ", epibone_table("drug_exposure"), " de ON de.drug_concept_id=m.drug_concept_id\n",
      "GROUP BY i.abbreviation, i.group_name, i.role, i.ingredient_concept_id, i.ingredient_concept_name\n",
      "ORDER BY i.abbreviation, i.ingredient_concept_id"
    ))
  }
  paste0(
    "WITH ", ctes, ",\n",
    "exposures AS (\n",
    "  SELECT DISTINCT de.person_id, de.drug_exposure_id, m.abbreviation, m.group_name,\n",
    "    COALESCE(route.concept_name, 'Missing/unknown') AS route_name,\n",
    "    LOWER(CONCAT(COALESCE(route.concept_name,''), ' ', COALESCE(src.concept_name,''),\n",
    "      ' ', COALESCE(prod.concept_name,''))) AS route_text\n",
    "  FROM ", epibone_table("drug_exposure"), " de\n",
    "  JOIN drug_map m ON m.drug_concept_id=de.drug_concept_id\n",
    "  LEFT JOIN ", epibone_table("concept"), " route ON route.concept_id=de.route_concept_id\n",
    "  LEFT JOIN ", epibone_table("concept"), " src ON src.concept_id=de.drug_source_concept_id\n",
    "  LEFT JOIN ", epibone_table("concept"), " prod ON prod.concept_id=de.drug_concept_id\n",
    ")\n",
    "SELECT abbreviation, group_name, route_name,\n",
    "  CASE\n",
    "    WHEN REGEXP_CONTAINS(route_text, r'(oral|tablet|capsule|chewable)')\n",
    "     AND NOT REGEXP_CONTAINS(route_text, r'(inject|intraven|intramuscular)') THEN 'oral_identified'\n",
    "    WHEN REGEXP_CONTAINS(route_text, r'(inject|intraven|intramuscular)') THEN 'parenteral_identified'\n",
    "    ELSE 'route_unresolved' END AS route_class,\n",
    "  COUNT(DISTINCT person_id) AS n_people, COUNT(DISTINCT drug_exposure_id) AS n_records\n",
    "FROM exposures\n",
    "GROUP BY abbreviation, group_name, route_name, route_class\n",
    "ORDER BY abbreviation, n_people DESC"
  )
}

epibone_inventory_sql <- function() {
  paste0(
    "SELECT 'person' AS table_name, COUNT(*) AS n_records, NULL AS min_year, NULL AS max_year\n",
    "FROM ", epibone_table("person"), "\nUNION ALL\n",
    "SELECT 'condition_occurrence', COUNT(*),\n",
    "  MIN(EXTRACT(YEAR FROM condition_start_date)), MAX(EXTRACT(YEAR FROM condition_start_date))\n",
    "FROM ", epibone_table("condition_occurrence"), "\nUNION ALL\n",
    "SELECT 'drug_exposure', COUNT(*),\n",
    "  MIN(EXTRACT(YEAR FROM drug_exposure_start_date)), MAX(EXTRACT(YEAR FROM drug_exposure_start_date))\n",
    "FROM ", epibone_table("drug_exposure"), "\nUNION ALL\n",
    "SELECT 'observation_period', COUNT(*),\n",
    "  MIN(EXTRACT(YEAR FROM observation_period_start_date)),\n",
    "  MAX(EXTRACT(YEAR FROM observation_period_end_date))\n",
    "FROM ", epibone_table("observation_period")
  )
}

epibone_run_concept_audit <- function() {
  aou_ducc_preflight()
  outdir <- file.path(EPIBONE_ROOT, "results", "concept_audit")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  message("Query 1/4: CDR inventory...")
  inventory <- epibone_bq(epibone_inventory_sql())
  message("Query 2/4: condition source-to-standard audit...")
  conditions <- epibone_bq(epibone_condition_audit_sql())
  message("Query 3/4: RxNorm ingredient audit...")
  drugs <- epibone_bq(epibone_drug_audit_sql(route = FALSE))
  message("Query 4/4: drug route/product audit...")
  routes <- epibone_bq(epibone_drug_audit_sql(route = TRUE))

  aou_write_private_rds(
    list(inventory = inventory, conditions = conditions, drugs = drugs, routes = routes),
    file.path(EPIBONE_ROOT, "private_enclave_only", "concept_audit_raw.rds")
  )
  files <- c(
    file.path(outdir, "01_cdr_inventory.csv"),
    file.path(outdir, "02_condition_concept_audit.csv"),
    file.path(outdir, "03_drug_ingredient_audit.csv"),
    file.path(outdir, "04_drug_route_audit.csv")
  )
  aou_write_aggregate_csv(inventory, files[1])
  aou_write_aggregate_csv(conditions, files[2])
  aou_write_aggregate_csv(drugs, files[3])
  aou_write_aggregate_csv(routes, files[4])
  epibone_write_manifest("concept_audit", basename(files))
  invisible(list(inventory = inventory, conditions = conditions, drugs = drugs, routes = routes))
}

epibone_covariate_condition_requests <- function() {
  spec <- list(
    intellectual_disability = "F7%",
    ckd = "N18%",
    liver_disease = "K7%",
    diabetes = c("E08%", "E09%", "E10%", "E11%", "E13%"),
    hyperparathyroid = "E21%",
    thyroid_disease = c("E03%", "E05%"),
    adrenal_disorder = "E27%",
    malabsorption = "K90%",
    inflammatory_arthritis = c("M05%", "M06%"),
    inflammatory_bowel = c("K50%", "K51%"),
    cancer = "C%",
    smoking = c("F17%", "Z72.0%"),
    alcohol_disorder = "F10%",
    substance_disorder = c("F11%", "F12%", "F13%", "F14%", "F15%", "F16%", "F18%", "F19%"),
    depression = c("F32%", "F33%"),
    anxiety = c("F40%", "F41%"),
    hypertension = "I10%",
    atrial_fibrillation = "I48%",
    heart_failure = "I50%",
    ischaemic_heart = c("I20%", "I21%", "I22%", "I23%", "I24%", "I25%"),
    copd = "J44%",
    obesity = "E66%",
    peripheral_neuropathy = "G62%",
    stroke_prior = c("I60%", "I61%", "I62%", "I63%", "I64%")
  )
  do.call(rbind, lapply(names(spec), function(name) {
    data.frame(set_name = name, pattern = spec[[name]], stringsAsFactors = FALSE)
  }))
}

epibone_model_condition_requests <- function() {
  icd10 <- unique(rbind(epibone_condition_requests(),
                        transform(epibone_covariate_condition_requests(),
                                  vocabulary_id = "ICD10CM")))
  unique(rbind(icd10, epibone_icd9_condition_requests()))
}

epibone_covariate_drug_requests <- function() {
  data.frame(
    med_group = c(
      rep("systemic_glucocorticoid", 5),
      rep("ppi", 5),
      rep("antidepressant", 6),
      rep("antipsychotic", 4),
      rep("benzodiazepine", 3),
      rep("osteoporosis_treatment", 8),
      rep("calcium_vitamin_d", 3)
    ),
    ingredient_name = c(
      "prednisone", "prednisolone", "methylprednisolone", "dexamethasone", "hydrocortisone",
      "omeprazole", "pantoprazole", "esomeprazole", "lansoprazole", "rabeprazole",
      "sertraline", "escitalopram", "citalopram", "fluoxetine", "paroxetine", "venlafaxine",
      "quetiapine", "olanzapine", "risperidone", "aripiprazole",
      "diazepam", "lorazepam", "clonazepam",
      "alendronate", "risedronate", "ibandronate", "zoledronic acid",
      "denosumab", "teriparatide", "abaloparatide", "raloxifene",
      "calcium carbonate", "cholecalciferol", "ergocalciferol"
    ),
    stringsAsFactors = FALSE
  )
}

epibone_outcome_sets <- function() {
  data.frame(
    outcome = c(
      "broad_fracture", "mof", "osteoporosis", "falls", "head_face_injury",
      "nc_ingrowing_nail", "nc_chalazion", "nc_onychomycosis",
      "nc_benign_skin_neoplasm", "nc_presbyopia", "nc_intestinal_polyp",
      "nc_skin_scar", "nc_seborrhoeic_keratosis", "nc_viral_conjunctivitis",
      "nc_acute_tonsillitis"
    ),
    set_names = c(
      "broad_fracture", "mof", "osteoporosis_m80|osteoporosis_m81",
      "falls_00_09|falls_10_19", "head_face_injury",
      "nc_ingrowing_nail", "nc_chalazion", "nc_onychomycosis",
      "nc_benign_skin_neoplasm", "nc_presbyopia", "nc_intestinal_polyp",
      "nc_skin_scar", "nc_seborrhoeic_keratosis", "nc_viral_conjunctivitis",
      "nc_acute_tonsillitis"
    ),
    family = c(
      "primary", "secondary_bone", "secondary_bone", "contextual", "contextual",
      rep("negative_control", 10)
    ),
    stringsAsFactors = FALSE
  )
}

epibone_contrast_plan <- function() {
  data.frame(
    contrast_id = c(
      "V1_EPI_VS_MIG_PRIMARY", "V1_EPI_VS_SYN_PRIMARY",
      "V1_EPI_VS_MIG_STRICT90", "V1_EPI_VS_SYN_STRICT90",
      "V2_MULTIPLE_VS_ONE", "V3_EIA_VS_SELECTED_NEIA",
      "V3_EIA_VS_SELECTED_NEIA_2CODE", "V3_EIA_VS_SELECTED_NEIA_730OBS",
      "V3_EIA_VS_SELECTED_NEIA_NONACUTE", "V3_EIA_PRM_VS_SELECTED_NEIA",
      "V3_EIA_VS_LEV", "V4_LTG_VS_LEV", "V4_VPA_VS_LTG"
    ),
    module = c(rep("V1", 4), "V2", rep("V3", 6), rep("V4", 2)),
    role = c(
      "co_primary", "comparator_sensitivity", "phenotype_sensitivity",
      "phenotype_sensitivity", "secondary_associational", "exploratory_medication",
      "exploratory_phenotype_sensitivity", "exploratory_observation_sensitivity",
      "exploratory_setting_sensitivity", "exploratory_composition_sensitivity",
      "exploratory_composition_sensitivity",
      "exploratory_pair", "exploratory_pair"
    ),
    exposure = c(
      "epilepsy", "epilepsy", "epilepsy", "epilepsy",
      "multiple_ingredients", "canonical_eia", "canonical_eia", "canonical_eia",
      "canonical_eia", "canonical_eia_plus_primidone", "canonical_eia",
      "LTG", "VPA"
    ),
    comparator = c(
      "migraine", "syncope", "migraine", "syncope",
      "one_ingredient", "selected_neia", "selected_neia", "selected_neia",
      "selected_neia", "selected_neia", "LEV", "LEV", "LTG"
    ),
    scenario = c(
      "primary30", "primary30", "strict90", "strict90", "primary365",
      "primary30", "two_code", "obs730", "nonacute", "include_primidone",
      "lev_reference", "pair", "pair"
    ),
    estimand = c(
      rep("overlap_population_association", 4),
      "overlap_population_treatment_complexity_association",
      rep("overlap_population_treatment_policy", 8)
    ),
    stringsAsFactors = FALSE
  )
}

epibone_hard_stops <- function() {
  c("PHB_VS_PHT", "CBZ_VS_OXC", "LCM_VS_LEV", "LCM_VS_LTG", "TPM_VS_ZNS",
    "PRM_VS_PHB", "BRV_VS_LEV", "CNB_VS_LCM", "PER_VS_BRV")
}
