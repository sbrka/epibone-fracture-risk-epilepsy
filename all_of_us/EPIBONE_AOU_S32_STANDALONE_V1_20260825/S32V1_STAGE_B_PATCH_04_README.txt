EPI-BONE All of Us S32 standalone v1.0 — final multiplicity patch 04

Why this patch is needed
- Patch 03 completed all 27 supported Cox/Fine-Gray pairs.
- BH adjustment was applied to the 27 estimable raw rows and 16 calibratable rows.
- The frozen execution lock requires the full 5-outcome x 10-exposure family (n=50),
  including unsupported cells, to define multiplicity.

What changes
- p.adjust(..., method="BH", n=50) is used for raw and calibrated p-values.
- multiplicity_family_n=50 is exported on every primary row.
- The two calibrated ASM-polytherapy findings remain below 0.05; their q-value
  changes from 0.0081 to approximately 0.0254.
- No cohort, coefficient, standard error, confidence interval, Fine-Gray result,
  negative control, empirical-null parameter, support gate, or disclosure rule changes.
- The existing Stage A private checkpoint is reused; no BigQuery extraction is repeated.

Upload
- Upload EPIBONE_AOU_S32_V1_STAGE_B_PATCH_04_20260825_UPLOAD.zip to the RStudio home folder.
- Allow it to replace s32v1_stage_b_models.R.

Run
source("~/EPIBONE_AOU_S32_V1_05_RESUME_STAGE_B_PATCH_04.R", encoding = "UTF-8")

Expected output
~/results/EPIBONE_AOU_S32_STANDALONE_V1_SAFE_AGGREGATES.zip
