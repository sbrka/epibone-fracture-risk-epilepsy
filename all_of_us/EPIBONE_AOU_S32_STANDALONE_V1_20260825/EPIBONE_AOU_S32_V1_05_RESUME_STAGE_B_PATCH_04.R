# EPI-BONE All of Us S32 standalone v1.0: final multiplicity patch 04.
# This reads the existing private Stage A checkpoint. It does not rerun Stage A
# and does not submit any BigQuery extraction.

S32V1_HOME <- path.expand("~")
model_path <- file.path(S32V1_HOME, "s32v1_stage_b_models.R")
resume_path <- file.path(S32V1_HOME, "EPIBONE_AOU_S32_V1_01_RESUME_AFTER_AUDIT.R")

if (!file.exists(model_path) || !file.exists(resume_path)) {
  stop("The patched model file or the original Stage B resume entry point is absent.",
       call. = FALSE)
}

model_text <- readLines(model_path, warn = FALSE, encoding = "UTF-8")
required_markers <- c(
  "S32V1_STAGE_B_PATCH_04",
  "S32V1_PRIMARY_FAMILY_SIZE",
  "multiplicity_family_n",
  "n = S32V1_PRIMARY_FAMILY_SIZE",
  "variance_diagonal",
  "args = c(\"-q\", \"-j\""
)
if (any(!vapply(required_markers, function(marker) {
  any(grepl(marker, model_text, fixed = TRUE))
}, logical(1)))) {
  stop("The uploaded s32v1_stage_b_models.R is not patch level 04.", call. = FALSE)
}

Sys.setenv(EPIBONE_S32_V1_AUDIT_APPROVED = "true")
source(resume_path, local = FALSE, encoding = "UTF-8")
