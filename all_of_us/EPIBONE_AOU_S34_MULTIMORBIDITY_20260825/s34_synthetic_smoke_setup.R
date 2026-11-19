# Local synthetic-data smoke-test setup for S34. Never upload this file to All of Us.
# Usage: Rscript s34_synthetic_smoke_setup.R /private/tmp/epibone_s34_smoke

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Pass exactly one synthetic test-home path.")
test_home <- normalizePath(args[1], mustWork = FALSE)
dir.create(file.path(test_home, "private_enclave_only"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(test_home, "results"), recursive = TRUE, showWarnings = FALSE)

set.seed(3401)
n <- 2500L
person_id <- seq_len(n)
lm <- as.Date("2022-01-01") + sample(0:365, n, replace = TRUE)
component_names <- c(
  "diabetes", "chronic_kidney_disease", "cancer", "cardiovascular_disease",
  "cerebrovascular_disease", "depression", "alcohol_disorder", "tobacco", "obesity"
)
components <- replicate(length(component_names), rbinom(n, 1L, 0.22))
colnames(components) <- component_names
count <- rowSums(components)

D <- data.frame(
  person_id = person_id, lm = lm, op_end = lm + 1095L,
  death_date = as.Date(NA), age_index = sample(40:85, n, replace = TRUE),
  sex = sample(c("Female", "Male"), n, replace = TRUE),
  index_year = factor(format(lm, "%Y")), log_prior_visits = log1p(rpois(n, 8)),
  race = factor(sample(c("White", "Black", "Asian", "Other"), n, replace = TRUE)),
  ethnicity = factor(sample(c("Hispanic", "Not Hispanic"), n, replace = TRUE)),
  osteoporosis_baseline = rbinom(n, 1L, plogis(-2.2 + 0.22 * count)),
  components, check.names = FALSE
)

primary <- c("major_fracture", "osteoporosis", "osteopenia", "vitamin_d_deficiency", "falls")
negative <- c(
  "acute_appendicitis", "inguinal_hernia", "age_related_cataract", "otitis_externa",
  "kidney_stone", "gallstone_disease", "hemorrhoids", "diverticular_disease",
  "atopic_dermatitis", "nonspecific_dermatitis"
)
event_rows <- list()
make_events <- function(endpoint, base_probability, count_effect = 0.08) {
  probability <- plogis(qlogis(base_probability) + count_effect * count)
  hit <- which(runif(n) < probability)
  data.frame(endpoint = endpoint, person_id = person_id[hit],
             dt = lm[hit] + sample(30:1000, length(hit), replace = TRUE))
}
for (endpoint in primary) {
  probability <- if (endpoint == "falls") 0.018 else 0.13
  event_rows[[length(event_rows) + 1L]] <- make_events(endpoint, probability)
}
for (endpoint in negative) {
  event_rows[[length(event_rows) + 1L]] <- make_events(endpoint, 0.09, count_effect = 0)
}
EVENTS <- do.call(rbind, event_rows)

checkpoint <- list(
  version = "EPIBONE_AOU_S32_STANDALONE_v1.0_2026-08-25",
  phenotype_version = "G40_R56_two_dates_7to180d_new_nonGP_ASM180_washout365_LM180",
  cdr = "synthetic-project.synthetic_dataset", billing = "synthetic-billing",
  cohort = D, events = EVENTS, fitbit = data.frame(), exposure_plan = data.frame(),
  primary_outcomes = primary, negative_controls = negative
)
saveRDS(checkpoint, file.path(test_home, "private_enclave_only", "epibone_s32_v1_stage_a.rds"))
cat("SYNTHETIC_CHECKPOINT_READY", test_home, "\n")
