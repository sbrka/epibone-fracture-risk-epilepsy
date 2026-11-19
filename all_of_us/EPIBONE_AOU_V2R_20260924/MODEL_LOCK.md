# Model lock: V2R U1-U3

## Frozen population and weights

Reuse the V1 primary incident-recorded epilepsy-versus-migraine cohort and its
syncope comparator. Reuse the accepted V1 overlap weights. No outcome-informed
propensity refitting is permitted. U3 retains those weights after the
prespecified DXA restriction and is explicitly exploratory.

## U1

Report age at index as mean (SD) and median (IQR), and women as n (%), for each
arm. Report unweighted and overlap-weighted summaries for epilepsy versus
migraine and epilepsy versus syncope.

## U2

The initial contract strings were treated as candidates rather than assumed
definitions. Their first active-CDR audit on 2026-09-25 showed that three did
not resolve and one resolved to mass/area BMD rather than a T-score. The
terminology-only amendment in `LOINC_TERMINOLOGY_AMENDMENT_20260925.md` therefore
locks the official active codes 38263-0 (femur T-score), 38267-1 (lumbar-spine
T-score), 38264-8 (hip T-score), 24701-5 (femur mass/area BMD), and 24966-4
(lumbar-spine mass/area BMD). The in-CDR audit must also verify that the returned
concept name has the expected T-score or mass/area semantic property.

Availability windows are days -1095 to -1 and 1 to 1095 relative to V1 index.
The model gate is at least 200 epilepsy and 100 migraine participants with a
numeric baseline T-score. The participant-level summary is the lowest plausible
T-score (-10 to 10). Estimate the overlap-weighted mean difference and weighted
risk differences for T <= -2.5 and -2.5 < T <= -1.0 using robust sandwich
variance.

## U3

Require CPT4/HCPCS source code 77080, 77081, 77085 or 77086 in days -1095 to -1
and no direct-source M80/M81/M85 or ICD-9-CM 733.0x before index. Outcomes over
three years from the frozen V1 landmark are broad fracture, M81 osteoporosis,
M85.8 osteopenia and falls. Fit cause-specific Cox models with robust variance
and overlap weights; report weighted Aalen-Johansen absolute risks with at least
300 bootstrap replicates. Fine-Gray is optional only when each arm has more than
20 deaths and outcome events. No U3 model is fitted unless broad-fracture events
are at least 21 per arm; each secondary outcome also requires at least 21 events
per arm.

U4 is not implemented. No All of Us estimate is pooled with TriNetX.
