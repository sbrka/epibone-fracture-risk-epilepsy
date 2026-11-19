# Enclave runbook

## Before running

- Use the current Controlled Tier CDR in the existing All of Us workspace.
- Confirm that the workspace description covers epilepsy, ASM exposure and bone
  outcomes.
- Do not activate external AI/API credentials during participant-level work.
- Keep the earlier packages
  `EPIBONE_AOU_ENCLAVE_20260822/` and
  `EPIBONE_AOU_ENCLAVE_20260822_v2/` in the same parent folder. Their private
  aggregate checkpoints prevent repeated feasibility queries.

## Stage A: design only

From the parent folder in RStudio:

```r
source("EPIBONE_AOU_MODELLING_20260823_v1/HOTFIX_07_DESIGN_ONLY_ROUTE_AND_MATCHING_REPAIR.R")
```

The script may prompt for the same governance attestations used previously. It
reads the exact Patch05 person-level cohort checkpoint directly. It submits no
BigQuery query, does not rebuild a cohort and fits no outcome model. It reruns
Stage A after adding medication ascertainment route to V3/V4 propensity models
and repairing MatchIt position recovery. Review:

- `results/checkpoint_repair/`
- `results/model_vocabulary/`
- `results/model_vocabulary/03_condition_source_code_registry.csv`
- `results/model_vocabulary/05_condition_source_pattern_coverage.csv`
- `results/design/01_design_gates.csv`
- `results/design/02_weighted_balance.csv`
- `results/design/03_ingredient_composition.csv`
- `results/design/07_covariate_actions.csv`
- `results/design/04_matching_sensitivity_design.csv`
- `results/design/08_medication_epilepsy_phenotype_routes.csv`
- `results/validation/01_validation_report.csv`

Hotfix 07 validates and creates a new safe review archive automatically.

Do not download `private_enclave_only/`.

Do not use an older Stage A entry script for the definitive run. Hotfix 07 is
required so the exploratory broad phenotype, route adjustment, balanced V1
matching sensitivity and Patch07 provenance are all active.

## Stage B: outcomes only after owner review

```r
Sys.setenv(AOU_EPIBONE_DESIGN_REVIEWED = "YES")
source("EPIBONE_AOU_MODELLING_20260823_v1/01_STAGE_B_OUTCOMES.R")
source("EPIBONE_AOU_MODELLING_20260823_v1/02_PACKAGE_SAFE_RESULTS.R")
```

The default absolute-risk bootstrap uses 300 replicates. A larger final run can
be requested before Stage B, for example:

```r
Sys.setenv(AOU_EPIBONE_BOOTSTRAP_REPS = "1000")
```

Never lower it below 100. Archives are timestamped and never overwritten.
