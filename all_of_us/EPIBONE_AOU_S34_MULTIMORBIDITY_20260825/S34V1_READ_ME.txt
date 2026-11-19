EPI-BONE All of Us S34 multimorbidity v1.0

This patch reuses the frozen private S32 standalone v1.0 checkpoint and submits no BigQuery
extraction. It must be run by the Authorized Data User inside the credentialed All of Us R Analysis
Environment. No participant-level file, date, RDS, fitted model, or private path may leave the
enclave.

The patch fits the nine-condition multimorbidity count continuously and as a disclosure-safe ordered
category. Component conditions are excluded from the adjustment candidates because their sum is the
exposure. Age, binary-known sex, and index year remain forced; prior visits, race, and ethnicity
follow the frozen priority.

Systemic glucocorticoid exposure is not present in the frozen S32 drug audit/checkpoint. S34 forbids
a new extraction, so the five prespecified glucocorticoid cells remain in the multiplicity denominator
as explicit support failures. Inhaled or topical corticosteroids are not substituted.

The multiplicity denominator is frozen at 65 cells: the existing S32 50-cell family plus five
continuous-count endpoints, five categorical global tests, and five reserved glucocorticoid cells.
Falls remains descriptive. If the osteoporosis-to-fracture positive control is not estimable, every
S34 result is labelled exploratory.

After this ZIP is automatically unpacked flat into ~/, run:

Sys.setenv(EPIBONE_S34_V1_APPROVED = "true")
source("~/EPIBONE_AOU_S34_00_RUN.R", encoding = "UTF-8")

Then personally review and download only:

~/results/EPIBONE_AOU_S34_MULTIMORBIDITY_SAFE_AGGREGATES.zip
