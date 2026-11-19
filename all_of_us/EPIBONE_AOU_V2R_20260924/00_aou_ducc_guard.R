# EPI-BONE All of Us disclosure and governance guard.
# Policy baseline: DUCC V6 reviewed 2026-08-18; re-review due 2026-11-14.

AOU_DUCC_VERSION <- "V6-2026-08-14"
AOU_POLICY_REVIEW_DUE_ON <- as.Date("2026-11-14")
AOU_MAX_SUPPRESSED_COUNT <- 20L
AOU_MIN_PUBLISHABLE_COUNT <- 21L

.aou_true <- function(x) tolower(trimws(x)) %in% c("1", "true", "yes")

.aou_date <- function(name, value) {
  out <- suppressWarnings(as.Date(value))
  if (length(out) != 1L || is.na(out)) {
    stop(name, " must be an ISO date (YYYY-MM-DD).", call. = FALSE)
  }
  out
}

aou_assert_no_external_ai_credentials <- function() {
  keys <- c(
    "OPENAI_API_KEY", "ANTHROPIC_API_KEY", "AZURE_OPENAI_API_KEY",
    "GOOGLE_API_KEY", "GEMINI_API_KEY", "COHERE_API_KEY",
    "MISTRAL_API_KEY", "TOGETHER_API_KEY"
  )
  active <- keys[nzchar(Sys.getenv(keys, unset = ""))]
  if (length(active)) {
    stop(
      "External AI/API credentials are active: ", paste(active, collapse = ", "),
      ". Remove them before participant-level AoU work.", call. = FALSE
    )
  }
  invisible(TRUE)
}

aou_ducc_preflight <- function(cdr = Sys.getenv("WORKSPACE_CDR"),
                               billing = Sys.getenv("GOOGLE_PROJECT"),
                               today = Sys.Date()) {
  if (today > AOU_POLICY_REVIEW_DUE_ON) {
    stop(
      "The local AoU policy review expired on ", AOU_POLICY_REVIEW_DUE_ON,
      ". Re-review current official policies before running.", call. = FALSE
    )
  }
  if (!nzchar(cdr) || !grepl("^[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+$", cdr)) {
    stop("WORKSPACE_CDR must be one resolved current project.dataset.", call. = FALSE)
  }
  if (!nzchar(billing)) {
    stop("GOOGLE_PROJECT must be the active workspace project.", call. = FALSE)
  }
  tier <- tolower(trimws(Sys.getenv("AOU_ACTIVE_DATA_TIER", unset = "")))
  if (!identical(tier, "controlled")) {
    stop("EPI-BONE requires exactly the Controlled Tier for unshifted dates.", call. = FALSE)
  }
  if (!identical(Sys.getenv("AOU_DUCC_ATTESTED_VERSION"), AOU_DUCC_VERSION)) {
    stop("Set AOU_DUCC_ATTESTED_VERSION=", AOU_DUCC_VERSION,
         " after personal review.", call. = FALSE)
  }
  if (!.aou_true(Sys.getenv("AOU_DATASET_CURRENT_CONFIRMED"))) {
    stop("Confirm that the newest applicable dataset is selected.", call. = FALSE)
  }
  if (!.aou_true(Sys.getenv("AOU_EXTERNAL_TOOLS_REVIEWED"))) {
    stop("Confirm that imported code has no identifiers or outbound participant-data calls.",
         call. = FALSE)
  }
  renewed <- .aou_date("AOU_ACCOUNT_RENEWED_ON", Sys.getenv("AOU_ACCOUNT_RENEWED_ON"))
  if (renewed > today || as.integer(today - renewed) > 366L) {
    stop("The Authorized Data User renewal date is invalid or older than one year.",
         call. = FALSE)
  }
  described <- .aou_date(
    "AOU_WORKSPACE_DESCRIPTION_REVIEWED_ON",
    Sys.getenv("AOU_WORKSPACE_DESCRIPTION_REVIEWED_ON")
  )
  if (described > today || as.integer(today - described) > 90L) {
    stop("The workspace-description review must be current within 90 days.", call. = FALSE)
  }
  aou_assert_no_external_ai_credentials()
  invisible(list(cdr = cdr, billing = billing, tier = tier))
}

aou_count_columns <- function(x) {
  names(x)[grepl(
    paste0(
      "(^|_)(n|count|counts|case|cases|event|events|death|deaths|person|persons|",
      "participant|participants|record|records)($|_)"
    ),
    names(x), ignore.case = TRUE, perl = TRUE
  )]
}

aou_prepare_aggregate_export <- function(x, count_cols = aou_count_columns(x),
                                         support_cols = character()) {
  x <- as.data.frame(x)
  count_cols <- intersect(count_cols, names(x))
  protected <- rep(FALSE, nrow(x))
  for (name in count_cols) {
    value <- suppressWarnings(as.numeric(x[[name]]))
    unsafe <- is.finite(value) & value >= 1 & value <= AOU_MAX_SUPPRESSED_COUNT
    protected <- protected | unsafe
    value[unsafe] <- NA_real_
    x[[name]] <- value
  }

  if (any(protected)) {
    descriptor <- grepl(
      "(^|_)(module|stage|arm|group|ingredient|set|label|name|status|note|route)($|_)",
      names(x), ignore.case = TRUE, perl = TRUE
    )
    blank <- union(count_cols, names(x)[!descriptor])
    for (name in blank) x[[name]][protected] <- NA
    if ("status" %in% names(x)) x$status[protected] <- "SUPPRESSED"
    if ("note" %in% names(x)) x$note[protected] <- "SUPPRESSED"
  }

  forbidden <- names(x)[grepl(
    "(^|_)(person|participant|research|sample|visit|encounter)_?id$|(^|_)(date|datetime|timestamp)$|_(date|datetime|timestamp)$",
    tolower(names(x)), perl = TRUE
  )]
  if (length(forbidden)) {
    stop("Export blocked: identifier/date columns: ", paste(forbidden, collapse = ", "),
         call. = FALSE)
  }
  for (name in count_cols) {
    value <- suppressWarnings(as.numeric(x[[name]]))
    if (any(is.finite(value) & value >= 1 & value <= AOU_MAX_SUPPRESSED_COUNT)) {
      stop("Export blocked: unsuppressed small count in ", name, call. = FALSE)
    }
  }
  for (name in intersect(support_cols, names(x))) {
    value <- suppressWarnings(as.numeric(x[[name]]))
    if (any(!is.na(value) & value <= AOU_MAX_SUPPRESSED_COUNT)) {
      stop("Export blocked: estimate supported by 20 or fewer observations.", call. = FALSE)
    }
  }
  x
}

aou_write_aggregate_csv <- function(x, path, count_cols = aou_count_columns(x),
                                    support_cols = character()) {
  safe <- aou_prepare_aggregate_export(x, count_cols, support_cols)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(safe, path, row.names = FALSE, na = "SUPPRESSED")
  invisible(path)
}

aou_write_private_rds <- function(x, path) {
  normalized <- normalizePath(dirname(path), mustWork = FALSE)
  if (!grepl("private_enclave_only", normalized, fixed = TRUE)) {
    stop("Participant-level or unsuppressed derivatives must stay in private_enclave_only/.",
         call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, path, compress = TRUE)
  invisible(path)
}

# Explicit writer for the modelling phase. Count columns must be named by the
# caller so vocabulary counts, gate labels and other non-participant quantities
# are never mistaken for participant cells. Rows are prespecified in code; their
# descriptors may remain visible when a participant count is suppressed.
aou_prepare_explicit_export <- function(x, count_cols = character(),
                                        support_cols = character(),
                                        dependent_cols = character()) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  count_cols <- intersect(count_cols, names(x))
  support_cols <- intersect(support_cols, names(x))
  dependent_cols <- intersect(dependent_cols, names(x))

  unsafe_by_row <- rep(FALSE, nrow(x))
  for (name in count_cols) {
    value <- suppressWarnings(as.numeric(x[[name]]))
    unsafe <- is.finite(value) & value >= 1 & value <= AOU_MAX_SUPPRESSED_COUNT
    unsafe_by_row <- unsafe_by_row | unsafe
    value[unsafe] <- NA_real_
    x[[name]] <- value
  }

  unsupported <- rep(FALSE, nrow(x))
  for (name in support_cols) {
    value <- suppressWarnings(as.numeric(x[[name]]))
    unsupported <- unsupported | (is.finite(value) & value <= AOU_MAX_SUPPRESSED_COUNT)
  }
  if (length(dependent_cols) && any(unsafe_by_row | unsupported)) {
    for (name in dependent_cols) x[[name]][unsafe_by_row | unsupported] <- NA
  }

  forbidden <- names(x)[grepl(
    paste0(
      "(^|_)(person|participant|research|sample|visit|encounter)_?id$|",
      "(^|_)(date|datetime|timestamp)$|_(date|datetime|timestamp)$"
    ),
    tolower(names(x)), perl = TRUE
  )]
  if (length(forbidden)) {
    stop("Export blocked: identifier/date columns: ", paste(forbidden, collapse = ", "),
         call. = FALSE)
  }
  for (name in count_cols) {
    value <- suppressWarnings(as.numeric(x[[name]]))
    if (any(is.finite(value) & value >= 1 & value <= AOU_MAX_SUPPRESSED_COUNT)) {
      stop("Export blocked: unsuppressed small participant count in ", name,
           call. = FALSE)
    }
  }
  x
}

aou_write_explicit_csv <- function(x, path, count_cols = character(),
                                   support_cols = character(),
                                   dependent_cols = character()) {
  safe <- aou_prepare_explicit_export(
    x, count_cols = count_cols, support_cols = support_cols,
    dependent_cols = dependent_cols
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(safe, path, row.names = FALSE, na = "SUPPRESSED")
  invisible(path)
}

message(
  "AoU guard loaded: publishable exact counts are 0 or >20; policy review due ",
  AOU_POLICY_REVIEW_DUE_ON, "."
)
