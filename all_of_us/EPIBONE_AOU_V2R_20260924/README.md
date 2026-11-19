# EPI-BONE All of Us reviewer-response package

This Controlled Tier package implements only the prespecified reviewer analyses
U1-U3 dated 2026-09-24. U4 is intentionally absent and remains on hold.

- U1 reports age and women descriptives, unweighted and overlap-weighted, for
  epilepsy versus migraine and epilepsy versus syncope.
- U2 first verifies the five candidate DXA LOINCs in the active CDR, reports
  baseline/follow-up numeric-value availability, and models the lowest baseline
  T-score only when at least 200 epilepsy and 100 migraine participants qualify.
  The five codes were terminology-corrected after the first audit; see
  `LOINC_TERMINOLOGY_AMENDMENT_20260925.md`.
- U3 restricts the frozen V1 epilepsy-migraine contrast to baseline DXA and no
  prior M80/M81/M85 (or ICD-9-CM 733.0x). Models run only when broad-fracture
  events are at least 21 in each arm; any smaller cell is reported as suppressed.

The package reuses the V1 cohort and accepted overlap-weight design from the
sibling `EPIBONE_AOU_MODELLING_20260823_v1` directory. If the cohort checkpoint
is absent, the frozen direct-source V1 builder is used once. Condition phenotypes
join `condition_source_concept_id` to explicit ICD-10-CM/ICD-9-CM source codes;
there is no SNOMED descendant or Maps-to expansion.

All participant rows, dates, measurements, weights and fitted objects remain in
`private_enclave_only/`. Never download that directory. Only aggregates in
`results/` may enter the safe ZIP, after exact counts 1-20 are suppressed.

See `RUNBOOK.md` for execution and `MODEL_LOCK.md` for estimands and gates.
Use `00_U1_ONLY.R` when only the reviewer-requested U1 table is needed; it does
not execute the U2/U3 feasibility queries or any Stage B model.

Use `00_U2_ONLY.R` for the optional measured-bone-density counts check. It
audits the five candidate LOINCs and reports cohort-level numeric-value counts
and the prespecified gate, without querying U3 events or fitting any model. A
semantic guard rejects a code whose active-CDR concept name does not match its
locked T-score versus mass/area classification.
