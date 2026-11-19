# Direct ICD source-code phenotype lock

## Executable rule

All diagnosis, covariate and outcome records are selected by joining
`condition_occurrence.condition_source_concept_id` to `concept.concept_id`, then
requiring the explicitly listed `ICD10CM` or `ICD9CM` code prefix. The code does
not use `condition_concept_id`, `Maps to`, SNOMED categories or
`concept_ancestor` for any condition phenotype.

The complete executable prefix registry is defined in
`R/01_definitions.R` by `epibone_condition_requests()`,
`epibone_covariate_condition_requests()` and
`epibone_icd9_condition_requests()`. Before cohort construction the enclave
exports every concrete code and concept name resolved from those prefixes to
`results/model_vocabulary/03_condition_source_code_registry.csv`.

## Core neurologic and bone families

- Epilepsy: ICD-10-CM G40; ICD-9-CM 345.
- Convulsion: ICD-10-CM R56; ICD-9-CM 780.3.
- Migraine: ICD-10-CM G43; ICD-9-CM 346.
- Syncope: ICD-10-CM R55; ICD-9-CM 780.2.
- Broad fracture: ICD-10-CM S12, S22, S32, S42, S52, S62, S72 and S82;
  explicit ICD-9-CM site families 805-808, 810-817, 820-821 and 823-824.
- Major osteoporotic fracture: ICD-10-CM S72.0-S72.2, S22.0, S32.0,
  S52.5-S52.6 and S42.2; explicit ICD-9-CM hip, vertebral, distal forearm and
  proximal humerus families.
- Osteoporosis: ICD-10-CM M80/M81; ICD-9-CM 733.0.
- Falls: ICD-10-CM W00-W19; ICD-9-CM E880-E888.
- Head/face injury: ICD-10-CM S00-S09; explicit ICD-9-CM skull, intracranial,
  open-wound, superficial, contusion, crushing and cranial-nerve injury groups.
- Prior stroke: executable ICD-10-CM I60-I63 and ICD-9-CM 430-436. I64 is a
  prespecified but documented C2025Q4R6 source-registry gap.
- Traumatic brain injury: ICD-10-CM S06; ICD-9-CM 850-854.

The same direct-source rule applies to all confounders and negative controls.
RxNorm drug ingredients continue to use the standard ingredient containment
hierarchy; this restriction is specific to condition phenotypes.

## Frozen QC gates

- Every requested prefix is exported with an executable/known-gap disposition;
  any unapproved gap is a hard failure.
- The generated condition CTE must contain no `concept_ancestor` reference.
- The cohort SQL must contain the direct source join and no standard-condition
  join.
- The exact resolved registry is reviewed before Stage B.
