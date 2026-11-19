# EPI-BONE All of Us S32 standalone v1.0

This package is self-contained. It does not use scripts or RDS objects of earlier projects. Run it in the credentialed All of Us R Analysis
Environment after the upload ZIP is automatically unpacked flat into `~/`.

## First command: Stage A extraction and audit

```r
source("~/EPIBONE_AOU_S32_V1_00_STAGE_A.R", encoding = "UTF-8")
```

Stage A performs the billable BigQuery work, writes only aggregate CSV audits to `~/results`, writes
one participant-level checkpoint to `~/private_enclave_only`, and then stops deliberately.

The frozen cohort requires:

- first direct ICD-10-CM G40/R56 record at age 18-120;
- a second qualifying record 7-180 days later;
- at least 365 days of prior observation and observation through a common day-180 landmark;
- first recorded qualifying maintenance ASM within days 0-180 after index;
- no qualifying maintenance ASM during the prior 365 days;
- survival through the common day-180 landmark.

Gabapentin, pregabalin, and acute rescue benzodiazepines do not qualify the new-user phenotype.
Status epilepticus uses the exact 28 prespecified G40 status leaves; G41 is not used.

Personally review these files before Stage B:

1. `~/results/s32v1_01_attrition.csv`
2. `~/results/s32v1_02_cohort_composition.csv`
3. `~/results/s32v1_03_condition_code_audit.csv`
4. `~/results/s32v1_04_endpoint_support.csv`
5. `~/results/s32v1_05_exposure_support.csv`
6. `~/results/s32v1_06_fitbit_feasibility.csv`
7. `~/results/s32v1_07_fitbit_schema.csv`
8. `~/results/s32v1_08_drug_ingredient_audit.csv`

Confirm directionality, concept names, outcome support, exposure support, and the current Fitbit
schema and RxNorm mappings. Counts 1-20 inclusive are suppressed. Do not interpret a suppressed
cell as zero.

## Second command: resume after personal audit

```r
Sys.setenv(EPIBONE_S32_V1_AUDIT_APPROVED = "true")
source("~/EPIBONE_AOU_S32_V1_01_RESUME_AFTER_AUDIT.R", encoding = "UTF-8")
```

Stage B does not repeat the extraction. It fits exposure-specific cause-specific Cox models,
Fine-Gray competing-risk models, and a prespecified negative-control panel. Empirical-null
calibration requires at least five estimable negative controls per exposure. Association-based PAFs
are emitted only after calibration, Benjamini-Hochberg, and proportional-hazards gates.

Fitbit activity/sleep summaries use the current CDR's official `activity_summary` and
`sleep_daily_summary` schemas. They are descriptive only and never enter outcome models.

The final reviewed export candidate is:

```text
~/results/EPIBONE_AOU_S32_STANDALONE_V1_SAFE_AGGREGATES.zip
```

Participant-level files and fitted model objects must remain inside the enclave. Stop the compute VM
when finished or while waiting for audit decisions.
