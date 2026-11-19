# Fracture risk in epilepsy: analysis and figure code

Code for the manuscript **"Fracture risk in epilepsy: contributions of seizures, falls, skeletal health, and medications"** (Schubert et al.; submitted to *Nature Communications*, November 2026).

Corresponding author: Kai Michael Schubert, Department of Neurology, University Hospital Zurich, Switzerland (contact details in the manuscript).

The repository holds the custom code of the study and the aggregate values behind the main figures. It holds no patient-level or participant-level data.

## 1. Who computed what

The study used two data sources, and most of the estimation happened inside their platforms.

**TriNetX (web platform, no custom code).** Cohort definition by diagnosis, procedure and medication codes; 1:1 propensity score matching; covariate balance; risks, risk differences and risk ratios in fixed windows; Kaplan-Meier curves and Cox hazard ratios. TriNetX returns aggregate tables only. Cohort, outcome and matching specifications are given in Supplementary Notes 1–10 and Supplementary Tables 1, 3 and 8 of the manuscript.

**This repository.**

| Part | Folder | Language |
|---|---|---|
| Processing of the TriNetX aggregate tables: collection of exported results, multiplicity corrections, negative-control summaries, consistency checks | `trinetx/` | Python |
| All of Us analyses: cohort construction, overlap weighting, cause-specific Cox and competing-risk models, negative controls, empirical-null calibration | `all_of_us/` | R |
| Main figures and the values plotted in them | `figures/` | Python |

## 2. Where each result comes from

| Manuscript item | Computed by | Code here |
|---|---|---|
| Burden of fracture and injury by window (Fig. 2a–c; Supplementary Tables 13, 14) | TriNetX | Headline estimates checked against the export archives by `trinetx/burden_headline_check/reconcile_compare_exports.py` |
| Bone-enriched matching, sex strata, adults with prior densitometry, negative controls (Fig. 3; Supplementary Tables 6, 9) | TriNetX | `trinetx/sensitivity_analyses/v2r_extract_trinetx.py` (reads exports; Benjamini-Hochberg q values; geometric mean of the 10 negative controls) |
| Enzyme inducers vs levetiracetam or lamotrigine (Fig. 4a,b; Supplementary Table 15) | TriNetX | `trinetx/class_comparison/class_comparison_multiplicity.py` (Benjamini-Hochberg across the 2 primary outcomes) |
| Each medication vs the pooled reference (Fig. 4c; Supplementary Table 16) | TriNetX | `trinetx/individual_medications/` (reads exports; Holm correction across the 4 hypothesis medications; negative-control means; balance; window check) |
| 45 pairwise medication comparisons and the corrected lacosamide design (Fig. 4d; Supplementary Notes 7, 8; Supplementary Tables 17, 18) | TriNetX | `trinetx/pairwise_matrix/` (rebuilds the outcome table from exports; Benjamini-Hochberg across the planned family of 45) |
| Replication of the burden comparison (Fig. 2d; Supplementary Table 7) | All of Us, this code | `all_of_us/EPIBONE_AOU_MODELLING_20260823_v1/` |
| Determinants within epilepsy (Fig. 5; Supplementary Table 19) | All of Us, this code | `all_of_us/EPIBONE_AOU_S32_STANDALONE_V1_20260825/` and `all_of_us/EPIBONE_AOU_S34_MULTIMORBIDITY_20260825/` |
| Feasibility of comparing recorded bone-density T-scores (Supplementary Table 7) | All of Us, this code | `all_of_us/EPIBONE_AOU_V2R_20260924/` with `all_of_us/EPIBONE_AOU_V2R_U2_CORRECTED_STANDALONE_20260925.R` |
| Figs. 1–5 | this code | `figures/code/` |

Other derived quantities, such as the empirical-null calibration of TriNetX estimates, are described in Supplementary Note 5.

`PROVENANCE.csv` lists every analysis file with its original location in the project, the SHA-256 checksum of the original, and whether it was changed for this repository. Three files were changed, only to remove a local path or the name of a private workspace. `class_comparison_multiplicity.py` and the README files were written for the repository.

## 3. TriNetX processing (`trinetx/`)

The scripts read the result archives exported from TriNetX (aggregate tables: measures of association, Kaplan-Meier tables, baseline characteristics) and the run ledgers of the study. These archives are not part of the repository (see section 6). The scripts therefore document the processing steps; they do not run without the archives, and their paths refer to the layout of the original project folder (see `PROVENANCE.csv`).

- `sensitivity_analyses/v2r_extract_trinetx.py`: one table of estimates for the sensitivity analyses, with q values and the negative-control summary of each cohort.
- `individual_medications/`: run `parse_all_exports.py`, then `synthesize_v10.py`, from the folder that holds the archives. `audit_windows.py` and `balance_from_exports.py` are checks.
- `pairwise_matrix/`: run `rebuild_pnu_outcomes_from_exports.py`, `reconcile_pnu_run_ledger.py`, then `analyse_pnu_matrix_final.py`.
- `burden_headline_check/reconcile_compare_exports.py`: set `EPIBONE_PROJECT_ROOT` to the folder that holds `04_trinetx_exports/` and `05_results/`.
- `class_comparison/class_comparison_multiplicity.py`: runs as is, from the table in `figures/data/`.

Requirements: Python 3.9 or later; `pandas`, `numpy` and `scipy` for `individual_medications/`; the standard library otherwise.

## 4. All of Us analyses (`all_of_us/`)

The R code runs only inside the All of Us Researcher Workbench (Controlled Tier), by a registered user, on the curated data repository named in each package. Participant-level rows, dates, propensity scores, weights and fitted models stay in the Workbench. Each package starts with `00_aou_ducc_guard.R`, which asks for the governance confirmations and provides the suppression of counts from 1 to 20 that the program requires. The original run instructions are kept in each package (`README.md`, `RUNBOOK.md`, `MODEL_LOCK.md`).

Run order:

1. **Burden replication** (`EPIBONE_AOU_MODELLING_20260823_v1/`): `HOTFIX_05_…R` builds the cohorts, `HOTFIX_07_…R` runs the design stage (propensity scores, overlap weights, balance, matching sensitivity), `01_STAGE_B_OUTCOMES.R` fits the outcome models, and `02_PACKAGE_SAFE_RESULTS.R` packages the aggregate results. The modules are in `R/`; `tests/test_synthetic.R` exercises them on synthetic data. A preceding concept audit and count-only feasibility step is not included. The package also defines exploratory medication contrasts (V2 to V4) that are not reported in the manuscript.
2. **Determinants** (`EPIBONE_AOU_S32_STANDALONE_V1_20260825/`): `EPIBONE_AOU_S32_V1_00_STAGE_A.R` extracts the cohort, `…_01_RESUME_AFTER_AUDIT.R` and `…_05_RESUME_STAGE_B_PATCH_04.R` run the models in `s32v1_stage_b_models.R`. The multimorbidity models (`EPIBONE_AOU_S34_MULTIMORBIDITY_20260825/`) reuse the same cohort.
3. **T-score feasibility**: `EPIBONE_AOU_V2R_U2_CORRECTED_STANDALONE_20260925.R`, with the modules in `EPIBONE_AOU_V2R_20260924/R/`. It needs the cohort and weights of step 1.

File names such as "HOTFIX" and "PATCH" record the order in which corrections were made during the runs; the files listed above are the final versions. Earlier entry scripts were left out.

Requirements: R 4.4.1; `bigrquery`, `digest`, `MatchIt`, `survival`; for the determinants also `data.table`, `glue`, `cmprsk`.

## 5. Figures (`figures/`)

```bash
python figures/code/build_all.py
```

This rebuilds Figs. 1–5 (`figures/main/`, as SVG, PDF and PNG), the values of every panel (`figures/source_data/`) and a list of every printed number with its source (`figures/INVENTORY_printed_numbers.csv`). Inputs are the aggregate tables in `figures/data/`. `prepare_km_data.py` shows how the daily curves of Fig. 2a were taken from the TriNetX export; its output is included.

Requirements: Python 3.9 or later with `Pillow` and `cairosvg`, and the Source Sans 3 fonts (SIL Open Font License). Set `EPIBONE_FONT_DIR` to the folder with the font files if they are not in `~/Library/Fonts`.

## 6. Data availability

Patient-level TriNetX data are available only to organizations with a TriNetX license. The exported aggregate result archives and the frozen study specifications are available from the corresponding author on request. All of Us data are available to registered researchers through the Researcher Workbench (https://www.researchallofus.org) under the program's data access policies. The aggregate values behind the main figures are in `figures/data/` and `figures/source_data/`; further aggregate results are in the Supplementary Information of the manuscript.

## 7. License

MIT License (see `LICENSE`).
