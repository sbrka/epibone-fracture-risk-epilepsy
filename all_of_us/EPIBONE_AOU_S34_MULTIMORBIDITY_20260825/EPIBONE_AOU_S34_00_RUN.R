# EPI-BONE All of Us S34 v1.0 guarded entry point.
# Reuses the private S32 standalone v1.0 checkpoint; submits no BigQuery query.

S34V1_HOME <- path.expand("~")
if (!identical(tolower(Sys.getenv("EPIBONE_S34_V1_APPROVED")), "true")) {
  stop(
    "S34 is locked. After reviewing the execution contract, set ",
    "EPIBONE_S34_V1_APPROVED='true' and re-source.", call. = FALSE
  )
}

checkpoint_path <- file.path(
  S34V1_HOME, "private_enclave_only", "epibone_s32_v1_stage_a.rds"
)
if (!file.exists(checkpoint_path)) {
  stop("The frozen S32 v1 Stage A checkpoint is absent; do not rebuild silently.", call. = FALSE)
}
CHECKPOINT <- readRDS(checkpoint_path)
if (!identical(CHECKPOINT$version, "EPIBONE_AOU_S32_STANDALONE_v1.0_2026-08-25") ||
    !identical(
      CHECKPOINT$phenotype_version,
      "G40_R56_two_dates_7to180d_new_nonGP_ASM180_washout365_LM180"
    )) {
  stop("The private checkpoint is not the frozen S32 standalone v1.0 checkpoint.", call. = FALSE)
}

CDR <- CHECKPOINT$cdr
BILLING <- CHECKPOINT$billing
source(file.path(S34V1_HOME, "s32v1_common.R"), local = FALSE, encoding = "UTF-8")
source(file.path(S34V1_HOME, "s32v1_definitions.R"), local = FALSE, encoding = "UTF-8")

required <- c(
  "s32v1_stage_b_models.R", "s34v1_multimorbidity_models.R",
  "S34V1_READ_ME.txt"
)
missing <- required[!file.exists(file.path(S34V1_HOME, required))]
if (length(missing)) {
  stop("Missing S34/S32 model files: ", paste(missing, collapse = ", "), call. = FALSE)
}

source(
  file.path(S34V1_HOME, "s34v1_multimorbidity_models.R"),
  local = FALSE, encoding = "UTF-8"
)
