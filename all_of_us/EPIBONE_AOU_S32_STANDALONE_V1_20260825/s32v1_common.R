# Shared guarded runtime for the self-contained EPI-BONE S32 v1.0 rebuild.

S32V1_HOME <- path.expand("~")
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  for (.locale in c("C.UTF-8", "en_US.UTF-8", "en_GB.UTF-8")) {
    suppressWarnings(try(Sys.setlocale("LC_CTYPE", .locale), silent = TRUE))
    if (isTRUE(l10n_info()[["UTF-8"]])) break
  }
  rm(.locale)
}
options(encoding = "UTF-8", gargle_oauth_cache = FALSE)

s32v1_find_file <- function(name) {
  candidates <- unique(c(
    file.path(S32V1_HOME, name),
    file.path(getwd(), name),
    file.path(S32V1_HOME, "scripts", name)
  ))
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("Missing required file: ", name, call. = FALSE)
  hit[1]
}

source(s32v1_find_file("00_aou_ducc_guard.R"), local = FALSE, encoding = "UTF-8")

.s32_need <- c("bigrquery", "data.table", "glue", "survival", "cmprsk")
.s32_missing <- .s32_need[!vapply(.s32_need, requireNamespace, logical(1), quietly = TRUE)]
if (length(.s32_missing)) {
  stop(
    "Missing R packages: ", paste(.s32_missing, collapse = ", "),
    ". Review package licenses and install them inside the Workbench before continuing.",
    call. = FALSE
  )
}
suppressMessages({
  library(bigrquery)
  library(data.table)
  library(glue)
  library(survival)
})

if (!exists("CDR") || !nzchar(CDR)) CDR <- Sys.getenv("WORKSPACE_CDR")
if (!exists("BILLING") || !nzchar(BILLING)) BILLING <- Sys.getenv("GOOGLE_PROJECT")
if (!nzchar(CDR) || !nzchar(BILLING)) {
  try(print(system("wb resource list --type=BQ_DATASET", intern = TRUE)), silent = TRUE)
  try(print(system("wb workspace describe", intern = TRUE)), silent = TRUE)
  stop(
    "Set CDR and BILLING to the current resolved project.dataset and billing project, then re-source.",
    call. = FALSE
  )
}
aou_ducc_preflight(CDR, BILLING)

s32v1_bq <- function(sql) {
  job <- bigrquery::bq_project_query(BILLING, sql)
  out <- bigrquery::bq_table_download(job, bigint = "integer64")
  as.data.frame(out)
}

s32v1_tbl <- function(name) sprintf("`%s.%s`", CDR, name)

s32v1_fix_int64 <- function(df) {
  df[] <- lapply(df, function(x) if (inherits(x, "integer64")) as.numeric(x) else x)
  df
}

s32v1_id_in <- function(ids, col = "person_id") {
  ids <- unique(ids[!is.na(ids)])
  if (!length(ids)) stop("Empty cohort: refusing to construct an unrestricted query.", call. = FALSE)
  rendered <- format(ids, scientific = FALSE, trim = TRUE)
  sprintf("%s IN (%s)", col, paste(rendered, collapse = ","))
}

S32V1_BIG_TABLES <- c(
  "visit_occurrence", "condition_occurrence", "measurement", "observation",
  "drug_exposure", "activity_summary", "sleep_daily_summary", "steps_intraday"
)

s32v1_assert_restricted <- function(sql, label = "query", allow_cohort_build = FALSE) {
  if (isTRUE(allow_cohort_build)) return(invisible(TRUE))
  hit <- S32V1_BIG_TABLES[vapply(
    S32V1_BIG_TABLES, function(tab) grepl(tab, sql, fixed = TRUE), logical(1)
  )]
  restricted <- grepl("person_id IN (", sql, fixed = TRUE) ||
    grepl("JOIN cohort", sql, fixed = TRUE) || grepl("JOIN eligible", sql, fixed = TRUE)
  if (length(hit) && !restricted) {
    stop(
      "Unrestricted person-scale pull blocked in ", label, ": ",
      paste(hit, collapse = ", "), call. = FALSE
    )
  }
  invisible(TRUE)
}

s32v1_safe_identifier <- function(x) {
  if (length(x) != 1L || is.na(x) || !grepl("^[A-Za-z_][A-Za-z0-9_]*$", x)) {
    stop("Unsafe or invalid BigQuery identifier: ", x, call. = FALSE)
  }
  sprintf("`%s`", x)
}

s32v1_resolve_column <- function(schema, table, candidates) {
  available <- schema$column_name[schema$table_name == table]
  hit <- candidates[candidates %in% available]
  if (length(hit)) hit[1] else NA_character_
}

s32v1_collapse_factor <- function(x, minimum = 21L) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(trimws(x))] <- "Unknown"
  tab <- table(x)
  rare <- names(tab)[tab < minimum]
  x[x %in% rare] <- "Other/Unknown"
  factor(x)
}

s32v1_safe_attrition <- function(tab) {
  tab <- as.data.frame(tab)
  n <- as.numeric(tab$n)
  if (length(n) > 1L) {
    delta <- n[-length(n)] - n[-1L]
    unsafe <- which(is.finite(delta) & delta >= 1 & delta <= AOU_MAX_SUPPRESSED_COUNT)
    if (length(unsafe)) n[unique(c(unsafe, unsafe + 1L))] <- NA_real_
  }
  n[is.finite(n) & n >= 1 & n <= AOU_MAX_SUPPRESSED_COUNT] <- NA_real_
  tab$n <- n
  tab
}

s32v1_write_safe <- function(x, filename, count_cols = aou_count_columns(x),
                             support_cols = character(), complementary_sets = list()) {
  path <- file.path(S32V1_HOME, "results", filename)
  aou_write_aggregate_csv(
    x, path, count_cols = count_cols, support_cols = support_cols,
    complementary_sets = complementary_sets
  )
}

s32v1_write_private <- function(x, filename) {
  aou_write_private_rds(x, file.path(S32V1_HOME, "private_enclave_only", filename))
}

dir.create(file.path(S32V1_HOME, "results"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(S32V1_HOME, "private_enclave_only"), recursive = TRUE, showWarnings = FALSE)

message("S32 v1 runtime ready. CDR: ", CDR, "; billing project: ", BILLING, ".")
