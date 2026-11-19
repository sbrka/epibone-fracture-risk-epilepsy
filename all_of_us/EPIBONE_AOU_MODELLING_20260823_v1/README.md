# EPI-BONE All of Us modelling package v1

This package implements the prespecified All of Us validation phase after the
2026-08-22 concept audit and feasibility screen. It is deliberately staged:

Hotfix 07 is the current Stage A entry point. It retains direct recorded
ICD-9-CM/ICD-10-CM source concepts and adds the owner-approved exploratory
medication phenotype: one direct G40/ICD-9-CM 345 diagnosis, or R56/ICD-9-CM
780.3 followed by the new epilepsy-specific ASM on day 0-180. V3/V4 are not
co-primary analyses.

1. `HOTFIX_07_DESIGN_ONLY_ROUTE_AND_MATCHING_REPAIR.R` reads the exact frozen
   Patch05 cohort checkpoint without BigQuery, includes ascertainment route in
   V3/V4 propensity models, recovers MatchIt positions from its input-order
   weight vector, reruns design diagnostics and creates a safe review archive.
   It fits no outcome model.
2. The owner reviews `results/design/`. Only contrasts marked `PROCEED` can enter
   Stage B.

To avoid BigQuery's optimizer-stage ceiling, the logically identical cohort CTE
graph is executed as one multi-statement job containing 44 job-scoped temporary
tables followed by one final download. No persistent BigQuery staging table is
created, and the temporary tables cannot be shared outside the script.
3. `01_STAGE_B_OUTCOMES.R` requires the explicit environment gate
   `AOU_EPIBONE_DESIGN_REVIEWED=YES` and then fits disclosure-supported outcomes.
4. `02_PACKAGE_SAFE_RESULTS.R` validates and packages only safe aggregate files.

Participant-level rows, dates, propensity scores, weights, fitted objects and
unsuppressed aggregates remain in `private_enclave_only/`. Never download that
directory. Exact participant counts from 1 through 20 are suppressed; a true
zero can remain visible.

The package does not estimate AoU LCM versus LEV. Feasibility failed that pair,
so the TriNetX A03 estimate remains the programme's primary LCM–LEV causal
headline. AoU is a direction/transportability validation and is never pooled
with TriNetX.

Condition membership is based on `condition_source_concept_id` joined to exact
ICD-9-CM/ICD-10-CM codes. The condition phenotype does not use `Maps to`,
`condition_concept_id`, SNOMED categories or `concept_ancestor`. The pre-outcome
vocabulary audit retains the VPA drug-label non-substitution exception and the
documented C2025Q4R6 absence of an executable ICD-10-CM I64 source concept.
The latter under-ascertains unspecified prior stroke in adjustment but does not
alter eligibility or outcomes.

The medication-epilepsy ascertainment route is a pre-index design covariate:
direct G40/ICD-9-CM 345 versus supported R56/ICD-9-CM 780.3 followed by the
index ASM. Passing V3/V4 designs must contain its weighted-balance diagnostic.
The V1 1:1 matching sensitivity must complete with maximum ASMD at most 0.10
before a safe Patch07 archive can be created.

See `RUNBOOK.md`, `ICD_SOURCE_CODE_LOCK.md` and `MODEL_LOCK.md`.
