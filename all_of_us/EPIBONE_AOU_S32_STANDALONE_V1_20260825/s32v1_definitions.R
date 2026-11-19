# Frozen definitions for the self-contained EPI-BONE All of Us S32 v1.0 rebuild.

S32V1_VERSION <- "EPIBONE_AOU_S32_STANDALONE_v1.0_2026-08-25"
S32V1_PHENOTYPE <- "G40_R56_two_dates_7to180d_new_nonGP_ASM180_washout365_LM180"
S32V1_MIN_CELL <- 21L
S32V1_LANDMARK_DAYS <- 180L
S32V1_WASHOUT_DAYS <- 365L

# Maintenance ASMs used to qualify the directional new-user phenotype. Acute
# rescue benzodiazepines are deliberately absent. Gabapentin and pregabalin are
# deliberately absent because non-epilepsy prescribing is common.
S32V1_ASM_INGREDIENTS <- c(
  "brivaracetam", "carbamazepine", "cannabidiol", "cenobamate", "clobazam",
  "eslicarbazepine", "ethosuximide", "felbamate",
  "fosphenytoin", "lacosamide", "lamotrigine", "levetiracetam",
  "oxcarbazepine", "perampanel", "phenobarbital", "phenytoin", "primidone",
  "rufinamide", "stiripentol", "topiramate", "valproic acid", "valproate",
  "vigabatrin", "zonisamide"
)

S32V1_EIA_INGREDIENTS <- c("carbamazepine", "phenytoin", "phenobarbital")
S32V1_BONE_RX_INGREDIENTS <- c(
  "alendronate", "risedronate", "ibandronate", "zoledronic acid",
  "denosumab", "teriparatide", "abaloparatide", "romosozumab", "calcitonin"
)

# Exact ICD-10-CM status leaves. G41 is not used: US ICD-10-CM status modifiers
# are encoded in the final character of G40 codes.
S32V1_STATUS_CODES <- c(
  "G40.001", "G40.011", "G40.101", "G40.111", "G40.201", "G40.211",
  "G40.301", "G40.311", "G40.401", "G40.411", "G40.501", "G40.801",
  "G40.803", "G40.811", "G40.813", "G40.821", "G40.823", "G40.833",
  "G40.841", "G40.843", "G40.901", "G40.911", "G40.A01", "G40.A11",
  "G40.B01", "G40.B11", "G40.C01", "G40.C11"
)

S32V1_OUTCOMES <- list(
  major_fracture = c("S12", "S22", "S32", "S42", "S62", "S72", "S82"),
  osteoporosis = c("M80", "M81"),
  osteopenia = "M85",
  vitamin_d_deficiency = "E55",
  falls = paste0("W", sprintf("%02d", 0:19))
)

# Bias-diagnostic negative-control outcomes. They are not assumed exchangeable
# with bone outcomes; they estimate residual systematic error under the same
# cohort, clock, exposure, and adjustment machinery.
S32V1_NEGATIVE_CONTROLS <- list(
  acute_appendicitis = "K35",
  inguinal_hernia = "K40",
  age_related_cataract = c("H25", "H26"),
  otitis_externa = "H60",
  kidney_stone = "N20",
  gallstone_disease = "K80",
  hemorrhoids = "K64",
  diverticular_disease = "K57",
  atopic_dermatitis = "L20",
  nonspecific_dermatitis = "L30"
)

S32V1_BASELINE_CONDITIONS <- list(
  diabetes = paste0("E", 10:14),
  chronic_kidney_disease = "N18",
  cancer = paste0("C", sprintf("%02d", 0:96)),
  cerebrovascular_disease = paste0("I", 60:69),
  cardiovascular_disease = paste0("I", 20:52),
  depression = c("F32", "F33"),
  alcohol_disorder = "F10",
  tobacco = c("F17", "Z72.0"),
  obesity = "E66"
)

S32V1_EXPOSURE_PLAN <- data.frame(
  exposure = c(
    "bone_disease_any", "bone_active_therapy", "osteoporosis_baseline",
    "osteopenia_baseline", "metabolic_bone_baseline", "enzyme_inducing_asm",
    "asm_polytherapy2", "status_epilepticus_baseline",
    "convulsive_seizure_baseline", "seizure_rate"
  ),
  exposure_group = c(
    rep("coded_bone_disease_treatment", 5), rep("asm_exposure", 2),
    rep("seizure_severity", 3)
  ),
  type = c(rep("binary", 9), "continuous"),
  stringsAsFactors = FALSE
)

S32V1_FIXED_COVARIATE_PRIORITY <- c(
  "age_index", "sex", "index_year", "log_prior_visits", "race", "ethnicity",
  "diabetes", "chronic_kidney_disease", "cancer", "cerebrovascular_disease",
  "cardiovascular_disease", "depression", "alcohol_disorder", "tobacco", "obesity"
)

s32v1_prefix_match <- function(code, prefixes) {
  code <- as.character(code)
  if (!length(prefixes)) return(rep(FALSE, length(code)))
  Reduce(`|`, lapply(prefixes, function(prefix) startsWith(code, prefix)))
}

s32v1_sql_strings <- function(x) {
  paste(sprintf("'%s'", gsub("'", "''", x, fixed = TRUE)), collapse = ",")
}

message("s32v1_definitions.R loaded: standalone phenotype and analysis contract frozen.")
