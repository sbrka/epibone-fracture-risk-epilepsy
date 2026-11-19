# EPI-BONE All of Us S32 standalone v1.0: resume after personal Stage A audit.

S32V1_HOME <- path.expand("~")
old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
setwd(S32V1_HOME)
source("~/s32v1_common.R", local = FALSE, encoding = "UTF-8")
source("~/s32v1_definitions.R", local = FALSE, encoding = "UTF-8")

approved <- tolower(trimws(Sys.getenv("EPIBONE_S32_V1_AUDIT_APPROVED", unset = "")))
if (!approved %in% c("true", "yes", "1")) {
  stop(
    "Stage B is locked. Personally review all ~/results/s32v1_01 through s32v1_08 audits, then set EPIBONE_S32_V1_AUDIT_APPROVED=true.",
    call. = FALSE
  )
}

checkpoint_path <- file.path(S32V1_HOME, "private_enclave_only", "epibone_s32_v1_stage_a.rds")
if (!file.exists(checkpoint_path)) {
  stop("Stage A private checkpoint is absent; run EPIBONE_AOU_S32_V1_00_STAGE_A.R first.",
       call. = FALSE)
}
CHECKPOINT <- readRDS(checkpoint_path)
if (!identical(CHECKPOINT$version, S32V1_VERSION) ||
    !identical(CHECKPOINT$phenotype_version, S32V1_PHENOTYPE)) {
  stop("Stage A checkpoint version/phenotype does not match the frozen standalone v1.0 contract.",
       call. = FALSE)
}
if (!identical(CHECKPOINT$cdr, CDR) || !identical(CHECKPOINT$billing, BILLING)) {
  stop("Current CDR/billing differs from the audited Stage A checkpoint.", call. = FALSE)
}
if (!all(c("cohort", "events", "fitbit", "primary_outcomes", "negative_controls") %in%
         names(CHECKPOINT))) {
  stop("Stage A checkpoint is incomplete.", call. = FALSE)
}
if (nrow(CHECKPOINT$cohort) < 100L ||
    !all(CHECKPOINT$cohort$phenotype_version == S32V1_PHENOTYPE)) {
  stop("Stage A cohort failed size or phenotype-version gates.", call. = FALSE)
}
if (any(S32V1_ASM_INGREDIENTS %in% c("gabapentin", "pregabalin"))) {
  stop("Frozen qualifying ASM list unexpectedly contains gabapentin/pregabalin.", call. = FALSE)
}
if (length(S32V1_STATUS_CODES) != 28L || any(startsWith(S32V1_STATUS_CODES, "G41"))) {
  stop("Frozen exact status-code list failed the 28-leaf/no-G41 gate.", call. = FALSE)
}

required_audits <- file.path(
  S32V1_HOME, "results",
  sprintf("s32v1_%02d_%s.csv", 1:8, c(
    "attrition", "cohort_composition", "condition_code_audit", "endpoint_support",
    "exposure_support", "fitbit_feasibility", "fitbit_schema", "drug_ingredient_audit"
  ))
)
if (any(!file.exists(required_audits))) {
  stop("One or more reviewed Stage A aggregate audit files are absent.", call. = FALSE)
}

source("~/s32v1_stage_b_models.R", local = FALSE, encoding = "UTF-8")
