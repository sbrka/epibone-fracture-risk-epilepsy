# Prespecified model lock

## Scientific role

All of Us is an external, lower-power validation analysis. It addresses the
broad causal chain—epilepsy burden, treatment complexity and older
enzyme-inducing medication groups—but cannot directly replicate the sparse
LCM–LEV comparison. TriNetX A03 remains the primary modern-drug causal estimate.

## Frozen modules

- V1: incident-recorded epilepsy versus migraine (primary neurologic comparator)
  and versus syncope (comparator sensitivity). A two-code/730-day/90-day
  phenotype is a sensitivity analysis.
- V2: multiple versus one recorded oral ASM ingredient during the fixed first
  year after confirmed epilepsy. This is an associational treatment-complexity
  analysis, not a treatment effect.
- V3: exploratory oral new users of CBZ/PHT/PHB versus LEV/LTG/LCM. Medication
  eligibility requires either one direct G40/ICD-9-CM 345 diagnosis on or before
  index, or R56/ICD-9-CM 780.3 followed by the new index ASM on day 0-180.
  Sensitivities require two direct epilepsy dates, 730-day observation,
  non-acute index, inclusion of primidone, or LEV alone as comparator.
- V4: LTG versus LEV and VPA versus LTG are exploratory.

AoU comparisons PHB–PHT, CBZ–OXC, LCM–LEV, LCM–LTG, TPM–ZNS, PRM–PHB,
BRV–LEV, CNB–LCM and PER–BRV are hard-stopped after feasibility. They cannot be
rescued by regrouping, longer follow-up or query loosening.

## Design gates

- Every condition phenotype uses directly recorded ICD-9-CM/ICD-10-CM
  `condition_source_concept_id` values. SNOMED category expansion and standard
  `condition_concept_id` membership are prohibited. `Maps to` is not executable
  for conditions. The exact resolved code registry is exported before cohort
  construction.
- Pre-outcome vocabulary exceptions are audited explicitly. The duplicate VPA label
  `valproic acid`, which is absent while the prespecified `valproate` Ingredient
  maps, is omitted without substitution. ICD-10-CM I64 has no executable source
  concept in C2025Q4R6, so prior stroke uses I60-I63 and unspecified stroke may
  be under-ascertained. Eligibility and outcomes do not use I64.
- One record per person per contrast. If a person qualifies for both arms at
  different times, the earliest qualifying index is used; ambiguous same-day
  cross-arm ties are removed. This avoids conditioning comparator status on a
  future epilepsy record.
- The common calendar window is selected from five-year bands using participant
  counts only. Each included band must have more than 20 people in each arm.
- Propensity scores use prespecified demographics, history, comorbidity,
  medication and healthcare-utilisation variables. Supported sparse clinical
  terms remain in the PS. Administrative nonresponse categories are collapsed,
  exact collinearity is pruned deterministically by QR, and all actions remain
  in the audit table.
- V3/V4 propensity scores additionally include the pre-outcome medication
  epilepsy-ascertainment route: direct G40/ICD-9-CM 345 versus supported
  R56/ICD-9-CM 780.3 followed by the index ASM on day 0-180. This addresses
  treatment-arm differences in phenotype certainty without changing eligibility.
- A binary/dummy level recorded in only one arm defines empirical nonoverlap;
  affected participants are trimmed before fitting. Combined empirical-level
  and propensity common-support trimming must retain at least 90% of each arm.
- Overlap weighting is primary. Each weighted arm must have ESS at least 100 and
  every diagnostic ASMD must be at most 0.10.
- A deterministic richer propensity specification is tried only when the main
  effects model fails balance. No outcome is accessed for model selection.
- One-to-one nearest-neighbour matching with a 0.2 SD logit-PS caliper is a
  sensitivity for the primary V1 design, not a separate cohort rebuild. MatchIt
  positions are recovered from its input-order weight vector; the sensitivity
  is retained only when maximum post-match ASMD is at most 0.10.

## Outcomes and inference

- Broad fracture is primary; MOF and osteoporosis are secondary bone outcomes.
  Falls and head/face injury are contextual outcomes. Ten frozen diagnoses are
  negative controls.
- New events are analysed from the design-specific landmark. People with prior
  records of the relevant outcome are excluded from that outcome analysis.
- Three-year cause-specific Cox regression with overlap weights and robust
  variance is the primary relative model.
- Weighted Aalen–Johansen risks at one and three years provide absolute risk,
  risk difference and risk ratio with a stratified bootstrap.
- Fine–Gray is secondary and is attempted only when each arm has more than 20
  deaths and more than 20 outcome events.
- No estimate is fitted unless each arm has more than 20 events. Suppressed
  values are never interpreted as zero.
- The V1 epilepsy-versus-migraine broad-fracture contrast is the sole AoU
  primary test. V3/V4 are exploratory. BH adjustment is applied to secondary
  bone outcomes within each contrast. Negative controls are qualitative
  diagnostics only.
- Hazard-ratio E-values are sensitivity summaries, explicitly treated as
  approximations rather than proof against residual confounding.

## Interpretation gates

No AoU result is pooled with TriNetX. Directional consistency can strengthen
transportability; disagreement triggers design and measurement review. V2 is
never described causally. Liberal MOF or osteoporosis findings remain
hypothesis-generating. Component decomposition, clone-censor-weight analyses,
exact-query reverse engineering and further sparse pair rescue remain outside
this phase.
