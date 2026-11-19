epibone_root <- function() {
  configured <- Sys.getenv("AOU_EPIBONE_ROOT", unset = "")
  if (nzchar(configured)) return(normalizePath(configured, mustWork = TRUE))
  normalizePath(getwd(), mustWork = TRUE)
}

EPIBONE_ROOT <- epibone_root()

epibone_required_packages <- c(
  "bigrquery", "digest", "MatchIt", "survival"
)

epibone_check_packages <- function(offer_install = interactive()) {
  missing <- epibone_required_packages[
    !vapply(epibone_required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) && offer_install) {
    answer <- readline(paste0(
      "Missing packages: ", paste(missing, collapse = ", "),
      ". Type YES to install from CRAN inside Workbench: "
    ))
    if (identical(toupper(trimws(answer)), "YES")) install.packages(missing)
  }
  missing <- epibone_required_packages[
    !vapply(epibone_required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop("Install missing packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

epibone_wb <- function(args) {
  out <- system2("wb", args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("Workbench command failed: wb ", paste(args, collapse = " "), "\n",
         paste(out, collapse = "\n"), call. = FALSE)
  }
  out
}

epibone_resolve_environment <- function() {
  cdr <- Sys.getenv("WORKSPACE_CDR", unset = "")
  billing <- Sys.getenv("GOOGLE_PROJECT", unset = "")

  if (!nzchar(cdr)) {
    resources <- epibone_wb(c("resource", "list", "--type=BQ_DATASET"))
    cat(paste(resources, collapse = "\n"), "\n")
    ids <- unique(unlist(regmatches(
      resources, gregexpr("C[0-9]{4}Q[0-9]+R[0-9]+", resources, perl = TRUE)
    )))
    ids <- ids[nzchar(ids)]
    if (length(ids) != 1L) {
      stop(
        "Could not select exactly one current BQ_DATASET resource. Resolve it manually with ",
        "wb resource resolve --id=RESOURCE_ID --bq-path=FULL_PATH and set WORKSPACE_CDR.",
        call. = FALSE
      )
    }
    resolved <- epibone_wb(c(
      "resource", "resolve", paste0("--id=", ids), "--bq-path=FULL_PATH"
    ))
    candidates <- unique(unlist(regmatches(
      resolved,
      gregexpr("[A-Za-z0-9_-]+\\.[A-Za-z0-9_]+", resolved, perl = TRUE)
    )))
    candidates <- candidates[nzchar(candidates)]
    if (length(candidates) < 1L) {
      stop("Could not parse project.dataset from wb resource resolve output.", call. = FALSE)
    }
    cdr <- tail(candidates, 1L)
  }

  if (!nzchar(billing)) {
    description <- epibone_wb(c("workspace", "describe"))
    line <- grep("google[ _-]*project|billing[ _-]*project", description,
                 ignore.case = TRUE, value = TRUE)
    candidates <- unique(unlist(regmatches(
      line, gregexpr("[A-Za-z][A-Za-z0-9_-]{5,}", line, perl = TRUE)
    )))
    candidates <- setdiff(candidates, c("Google", "project", "Project", "billing", "Billing"))
    if (length(candidates) < 1L) {
      cat(paste(description, collapse = "\n"), "\n")
      stop("Could not parse GOOGLE_PROJECT. Set it from wb workspace describe.", call. = FALSE)
    }
    billing <- tail(candidates, 1L)
  }

  Sys.setenv(WORKSPACE_CDR = cdr, GOOGLE_PROJECT = billing)
  message("Resolved WORKSPACE_CDR=", cdr)
  message("Resolved GOOGLE_PROJECT=", billing)
  invisible(list(cdr = cdr, billing = billing))
}

epibone_collect_attestations <- function() {
  needed <- c(
    "AOU_DUCC_ATTESTED_VERSION", "AOU_ACCOUNT_RENEWED_ON",
    "AOU_WORKSPACE_DESCRIPTION_REVIEWED_ON", "AOU_DATASET_CURRENT_CONFIRMED",
    "AOU_EXTERNAL_TOOLS_REVIEWED", "AOU_ACTIVE_DATA_TIER"
  )
  if (all(nzchar(Sys.getenv(needed, unset = "")))) return(invisible(TRUE))
  if (!interactive()) {
    stop("Required AoU attestations are unset; run interactively or set them explicitly.",
         call. = FALSE)
  }
  answer <- readline(paste0(
    "Type YES only after confirming: current DUCC reviewed; newest Controlled Tier CDR; ",
    "workspace description covers epilepsy, ASM and bone outcomes; imported code has no ",
    "identifiers or outbound participant-data calls: "
  ))
  if (!identical(toupper(trimws(answer)), "YES")) stop("Preflight not attested.", call. = FALSE)
  renewed <- readline("Authorized Data User account renewed on (YYYY-MM-DD): ")
  described <- readline(paste0(
    "Workspace description reviewed today or on (YYYY-MM-DD; Enter for ", Sys.Date(), "): "
  ))
  if (!nzchar(trimws(described))) described <- as.character(Sys.Date())
  Sys.setenv(
    AOU_DUCC_ATTESTED_VERSION = "V6-2026-08-14",
    AOU_ACCOUNT_RENEWED_ON = renewed,
    AOU_WORKSPACE_DESCRIPTION_REVIEWED_ON = described,
    AOU_DATASET_CURRENT_CONFIRMED = "true",
    AOU_EXTERNAL_TOOLS_REVIEWED = "true",
    AOU_ACTIVE_DATA_TIER = "controlled"
  )
  invisible(TRUE)
}

epibone_bq <- function(sql) {
  billing <- Sys.getenv("GOOGLE_PROJECT")
  if (!nzchar(billing)) stop("GOOGLE_PROJECT is unset.", call. = FALSE)
  job <- bigrquery::bq_project_query(billing, sql, use_legacy_sql = FALSE)
  bigrquery::bq_table_download(job, bigint = "integer64")
}

epibone_table <- function(name) {
  cdr <- Sys.getenv("WORKSPACE_CDR")
  if (!nzchar(cdr) || !grepl("^[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+$", cdr)) {
    stop("Invalid WORKSPACE_CDR.", call. = FALSE)
  }
  paste0("`", cdr, ".", name, "`")
}

epibone_sql_string <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")

epibone_struct_rows <- function(df, columns) {
  stopifnot(all(columns %in% names(df)))
  apply(df[, columns, drop = FALSE], 1L, function(row) {
    fields <- paste0(
      vapply(row, epibone_sql_string, character(1)),
      " AS ", columns
    )
    paste0("STRUCT(", paste(fields, collapse = ", "), ")")
  })
}

epibone_write_manifest <- function(stage, outputs) {
  info <- data.frame(
    stage = stage,
    run_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    cdr = Sys.getenv("WORKSPACE_CDR"),
    billing_project = Sys.getenv("GOOGLE_PROJECT"),
    code_version = "EPIBONE_AOU_MODELLING_20260823_v1_PATCH07",
    output = outputs,
    stringsAsFactors = FALSE
  )
  aou_write_aggregate_csv(
    info,
    file.path(EPIBONE_ROOT, "results", stage, paste0("manifest_", stage, ".csv")),
    count_cols = character()
  )
}

epibone_initialize <- function(offer_install = interactive()) {
  epibone_check_packages(offer_install = offer_install)
  epibone_resolve_environment()
  epibone_collect_attestations()
  source(file.path(EPIBONE_ROOT, "00_aou_ducc_guard.R"), local = FALSE)
  aou_ducc_preflight()
  dir.create(file.path(EPIBONE_ROOT, "results"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(EPIBONE_ROOT, "private_enclave_only"), recursive = TRUE,
             showWarnings = FALSE)
  invisible(TRUE)
}
