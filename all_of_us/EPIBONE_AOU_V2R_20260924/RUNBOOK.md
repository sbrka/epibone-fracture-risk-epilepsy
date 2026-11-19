# Enclave runbook

## Placement and preflight

Place this directory beside `EPIBONE_AOU_MODELLING_20260823_v1` in the existing
EPI-BONE All of Us workspace. Use the current Controlled Tier CDR. The expected
2026-09-24 environment was CDR `C2025Q4R6`; if the CDR differs,
the built-in LOINC audit must be reviewed before Stage B.

Do not activate external AI/API credentials. Confirm that the workspace
description covers epilepsy and bone-health outcomes. Complete the governance
prompts when asked.

## U1-only fast path

For the requested age and percentage-women summary only, run this from the
parent directory in RStudio:

```r
source("EPIBONE_AOU_V2R_20260924/00_U1_ONLY.R")
```

Review `results/u1_descriptives.csv`. It must contain exactly 8 rows: 2
contrasts × unweighted/overlap-weighted × 2 arms. Then create the
disclosure-checked archive:

```r
source("EPIBONE_AOU_V2R_20260924/02_PACKAGE_SAFE_RESULTS.R")
```

Download only the timestamped safe-results ZIP and matching `.sha256` file.
Do not run Stage B for U1.

## U2-only measured-bone-density check

To run the optional counts-only U2 check without querying or analysing U3, run:

```r
source("EPIBONE_AOU_V2R_20260924/00_U2_ONLY.R")
```

Review:

- `results/feasibility/u2_loinc_vocabulary_audit.csv`
- `results/feasibility/u2_measurement_counts.csv`
- `results/feasibility/u2_model_gate.csv`
- `results/validation/u2_validation_report.csv`

Then create the disclosure-checked archive:

```r
source("EPIBONE_AOU_V2R_20260924/02_PACKAGE_SAFE_RESULTS.R")
```

This path exports counts only and does not execute U2 models or U3.
The corrected LOINCs and the reason for the terminology amendment are recorded
in `LOINC_TERMINOLOGY_AMENDMENT_20260925.md`; do not reuse the superseded
candidate strings from the original execution sheet.

## Stage A: descriptives and feasibility only

From the parent directory in RStudio:

```r
source("EPIBONE_AOU_V2R_20260924/00_STAGE_A_REPAIR_BUILD_DESIGN.R")
```

Review:

- `results/u1_descriptives.csv`
- `results/feasibility/u2_loinc_vocabulary_audit.csv`
- `results/feasibility/u2_measurement_counts.csv`
- `results/feasibility/u2_model_gate.csv`
- `results/feasibility/u3_dxa_replication_gate.csv`
- `results/validation/01_validation_report.csv`

All LOINCs must have an acceptable audited disposition. U2 must meet both the
200-epilepsy and 100-migraine baseline T-score thresholds. U3 is an exploratory
direction check and must have at least 21 broad-fracture events in each arm.

## Stage B: owner-authorised models

Only after reviewing Stage A:

```r
Sys.setenv(AOU_EPIBONE_V2R_REVIEWED = "YES")
source("EPIBONE_AOU_V2R_20260924/01_STAGE_B_OUTCOMES.R")
```

The default Aalen-Johansen bootstrap is 300 replicates. A larger final run may
be requested with `Sys.setenv(AOU_EPIBONE_BOOTSTRAP_REPS="1000")`; never use
fewer than 100.

## Disclosure-safe package

```r
source("EPIBONE_AOU_V2R_20260924/02_PACKAGE_SAFE_RESULTS.R")
```

Download only the timestamped `EPIBONE_AOU_V2R_SAFE_RESULTS_*.zip` and matching
`.sha256` file. Never download `private_enclave_only/`. True zeroes may remain;
all exact participant/event/death counts from 1 through 20 are suppressed.

## Local synthetic check

```r
source("EPIBONE_AOU_V2R_20260924/tests/test_synthetic.R")
```

This check uses no Workbench data and does not replace in-enclave SQL and gate
review.
