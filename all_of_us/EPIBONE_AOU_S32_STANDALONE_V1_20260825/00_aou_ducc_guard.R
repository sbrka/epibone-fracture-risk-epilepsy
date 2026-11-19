# All of Us Data User Code of Conduct (DUCC) V6 runtime guard.
# Policy reviewed: 2026-08-18; official DUCC page updated 2026-08-14.
# This guard reduces preventable disclosure errors. It does not replace the
# Authorized Data User's annual renewal, workspace review, or disclosure review.

AOU_DUCC_VERSION <- "V6-2026-08-14"
AOU_POLICY_REVIEWED_ON <- as.Date("2026-08-18")
AOU_POLICY_REVIEW_DUE_ON <- as.Date("2026-11-14")
AOU_MAX_SUPPRESSED_COUNT <- 20L
AOU_MIN_PUBLISHABLE_COUNT <- 21L

.aou_true <- function(x) tolower(trimws(x)) %in% c("1", "true", "yes")

.aou_parse_date <- function(name, value) {
  parsed <- suppressWarnings(as.Date(value))
  if (length(parsed) != 1L || is.na(parsed)) {
    stop(name, " must be an ISO date (YYYY-MM-DD).", call. = FALSE)
  }
  parsed
}

aou_assert_no_external_ai_credentials <- function() {
  external_ai_keys <- c(
    "OPENAI_API_KEY", "ANTHROPIC_API_KEY", "AZURE_OPENAI_API_KEY",
    "GOOGLE_API_KEY", "GEMINI_API_KEY", "COHERE_API_KEY",
    "MISTRAL_API_KEY", "TOGETHER_API_KEY"
  )
  active <- external_ai_keys[nzchar(Sys.getenv(external_ai_keys, unset = ""))]
  if (length(active)) {
    stop(
      "External AI/API credentials are active (", paste(active, collapse = ", "),
      "). Remove them before participant-level work; outgoing AI/API transfer is prohibited.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

aou_ducc_preflight <- function(cdr = Sys.getenv("WORKSPACE_CDR"),
                               billing = Sys.getenv("GOOGLE_PROJECT"),
                               require_attestations = TRUE,
                               today = Sys.Date()) {
  if (today > AOU_POLICY_REVIEW_DUE_ON) {
    stop(
      "AoU policy review expired on ", AOU_POLICY_REVIEW_DUE_ON,
      ". Re-check the official DUCC/policies and update 00_aou_ducc_guard.R.",
      call. = FALSE
    )
  }
  if (length(cdr) != 1L || !nzchar(cdr) ||
      !grepl("^[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+$", cdr)) {
    stop("WORKSPACE_CDR must name exactly one resolved current dataset (project.dataset).",
         call. = FALSE)
  }
  if (length(billing) != 1L || !nzchar(billing)) {
    stop("GOOGLE_PROJECT must be the active workspace billing project.", call. = FALSE)
  }

  tier <- tolower(trimws(Sys.getenv("AOU_ACTIVE_DATA_TIER", unset = "")))
  if (!tier %in% c("registered", "controlled", "partner")) {
    stop(
      "Set AOU_ACTIVE_DATA_TIER to exactly one of: registered, controlled, partner. ",
      "Never access more than one tier simultaneously.", call. = FALSE
    )
  }

  if (require_attestations) {
    attested <- Sys.getenv("AOU_DUCC_ATTESTED_VERSION", unset = "")
    if (!identical(attested, AOU_DUCC_VERSION)) {
      stop("Set AOU_DUCC_ATTESTED_VERSION=", AOU_DUCC_VERSION,
           " after personally reviewing/signing the current DUCC.", call. = FALSE)
    }
    if (!.aou_true(Sys.getenv("AOU_DATASET_CURRENT_CONFIRMED", unset = ""))) {
      stop("Set AOU_DATASET_CURRENT_CONFIRMED=true after selecting the newest applicable dataset.",
           call. = FALSE)
    }
    if (!.aou_true(Sys.getenv("AOU_EXTERNAL_TOOLS_REVIEWED", unset = ""))) {
      stop(
        "Set AOU_EXTERNAL_TOOLS_REVIEWED=true after confirming imported files/software contain ",
        "no PII/PHI/IPI and make no participant-level outbound calls.", call. = FALSE
      )
    }
    renewed <- .aou_parse_date(
      "AOU_ACCOUNT_RENEWED_ON", Sys.getenv("AOU_ACCOUNT_RENEWED_ON", unset = "")
    )
    if (renewed > today || as.integer(today - renewed) > 366L) {
      stop("Authorized Data User renewal is missing, future-dated, or older than one year.",
           call. = FALSE)
    }
    described <- .aou_parse_date(
      "AOU_WORKSPACE_DESCRIPTION_REVIEWED_ON",
      Sys.getenv("AOU_WORKSPACE_DESCRIPTION_REVIEWED_ON", unset = "")
    )
    if (described > today || as.integer(today - described) > 90L) {
      stop("Workspace description confirmation is future-dated or older than 90 days.",
           call. = FALSE)
    }
  }

  aou_assert_no_external_ai_credentials()
  invisible(list(
    ducc_version = AOU_DUCC_VERSION,
    tier = tier,
    cdr = cdr,
    billing = billing,
    policy_review_due = AOU_POLICY_REVIEW_DUE_ON
  ))
}

aou_is_publishable_count <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  is.na(x) | x == 0 | x > AOU_MAX_SUPPRESSED_COUNT
}

aou_count_columns <- function(x) {
  names(x)[grepl(
    paste0(
      "(^|_)(n|count|counts|case|cases|event|events|death|deaths|person|persons|",
      "participant|participants|sample|samples|record|records|respondent|respondents)($|_)"
    ),
    names(x), ignore.case = TRUE, perl = TRUE
  )]
}

aou_suppress_counts <- function(x, count_cols = aou_count_columns(x),
                                complementary_sets = list()) {
  x <- as.data.frame(x)
  count_cols <- intersect(count_cols, names(x))
  for (name in count_cols) {
    if (!is.numeric(x[[name]]) && !inherits(x[[name]], "integer64")) next
    value <- as.numeric(x[[name]])
    unsafe <- is.finite(value) & value >= 1 & value <= AOU_MAX_SUPPRESSED_COUNT
    value[unsafe] <- NA_real_
    x[[name]] <- value
  }

  # Secondary suppression: when one member is hidden, hide every member so a
  # small complement cannot be reconstructed from totals or percentages.
  for (set in complementary_sets) {
    set <- intersect(set, names(x))
    if (length(set) < 2L) next
    hidden <- Reduce(`|`, lapply(set, function(name) is.na(x[[name]])))
    for (name in set) x[[name]][hidden] <- NA
  }
  x
}

aou_assert_aggregate_export <- function(x, count_cols = aou_count_columns(x),
                                        support_cols = character(),
                                        complementary_sets = list()) {
  x <- as.data.frame(x)
  lower <- tolower(names(x))
  forbidden <- names(x)[
    grepl("(^|_)(person|participant|research|sample|specimen|visit|encounter)_?id$", lower) |
      grepl("(^|_)(date|datetime|timestamp)$|_(date|datetime|timestamp)$", lower)
  ]
  if (length(forbidden)) {
    stop("Export blocked: participant identifier/date columns: ",
         paste(forbidden, collapse = ", "), call. = FALSE)
  }

  count_cols <- intersect(count_cols, names(x))
  bad <- list()
  for (name in count_cols) {
    if (!is.numeric(x[[name]]) && !inherits(x[[name]], "integer64")) next
    value <- as.numeric(x[[name]])
    rows <- which(is.finite(value) & value >= 1 & value <= AOU_MAX_SUPPRESSED_COUNT)
    if (length(rows)) bad[[name]] <- rows
  }
  if (length(bad)) {
    stop(
      "Export blocked: unsuppressed count(s) from 1 through 20 in ",
      paste(names(bad), collapse = ", "), call. = FALSE
    )
  }

  support_cols <- intersect(support_cols, names(x))
  if (length(support_cols)) {
    unsupported <- Reduce(`|`, lapply(support_cols, function(name) {
      value <- suppressWarnings(as.numeric(x[[name]]))
      !is.na(value) & value <= AOU_MAX_SUPPRESSED_COUNT
    }))
    if (any(unsupported)) {
      stop("Export blocked: an estimate is supported by 20 or fewer participants/events.",
           call. = FALSE)
    }
  }

  for (set in complementary_sets) {
    set <- intersect(set, names(x))
    if (length(set) >= 2L) {
      partial <- rowSums(is.na(x[set])) > 0 & rowSums(is.na(x[set])) < length(set)
      if (any(partial)) {
        stop("Export blocked: incomplete secondary suppression in set: ",
             paste(set, collapse = ", "), call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

aou_prepare_aggregate_export <- function(x, count_cols = aou_count_columns(x),
                                         support_cols = character(),
                                         complementary_sets = list()) {
  x <- as.data.frame(x)
  count_cols <- intersect(count_cols, names(x))
  protected_row <- rep(FALSE, nrow(x))
  for (name in count_cols) {
    value <- suppressWarnings(as.numeric(x[[name]]))
    protected_row <- protected_row |
      (is.finite(value) & value >= 1 & value <= AOU_MAX_SUPPRESSED_COUNT)
  }

  safe <- aou_suppress_counts(x, count_cols, complementary_sets)
  if (any(protected_row)) {
    descriptor_name <- grepl(
      "(^|_)(group|label|name|category|stratum|analysis|contrast|outcome|exposure|term|metric|variable|feature)($|_)",
      names(safe), ignore.case = TRUE, perl = TRUE
    )
    # Keep only non-identifying descriptor columns. Hide counts and every other
    # value, including character-formatted estimates/CIs, on a protected row.
    blank <- union(count_cols, names(safe)[!descriptor_name])
    for (name in blank) safe[[name]][protected_row] <- NA
    for (name in intersect(c("note", "status", "disclosure_note"), names(safe))) {
      safe[[name]][protected_row] <- "SUPPRESSED"
    }
  }

  aou_assert_aggregate_export(
    safe, count_cols = count_cols, support_cols = support_cols,
    complementary_sets = complementary_sets
  )
  safe
}

aou_write_aggregate_csv <- function(x, path, count_cols = aou_count_columns(x),
                                    support_cols = character(),
                                    complementary_sets = list()) {
  safe <- aou_prepare_aggregate_export(
    x, count_cols = count_cols, support_cols = support_cols,
    complementary_sets = complementary_sets
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(safe, path, row.names = FALSE, na = "SUPPRESSED")
  invisible(path)
}

aou_write_private_rds <- function(x, path) {
  normalized <- normalizePath(dirname(path), mustWork = FALSE)
  allowed <- any(vapply(
    c("private_enclave_only", "derived/person_level", "/private"),
    function(token) grepl(token, normalized, fixed = TRUE),
    logical(1)
  ))
  if (!allowed) {
    stop("Participant-level derivatives must stay in an enclave-only private path.",
         call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, path, compress = TRUE)
  invisible(path)
}

aou_refuse_participant_trained_model_export <- function(...) {
  stop(
    "Export of a model trained on participant-level All of Us data is blocked. ",
    "Share it only within an approved All of Us analytical platform.", call. = FALSE
  )
}

message(
  "AoU DUCC guard loaded: V6; publishable counts are 0 or >20; policy review due ",
  AOU_POLICY_REVIEW_DUE_ON, "."
)
