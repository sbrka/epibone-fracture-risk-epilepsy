# U2 LOINC terminology amendment (2026-09-25)

## Reason

The execution sheet deliberately described its five U2 strings as candidates
to verify before use. The first active-CDR audit found that three strings did
not resolve and that LOINC 46278-8 resolved to left-hip mass/area BMD, not a
T-score. Therefore, that first counts archive is a terminology-discovery run and
is not the final U2 feasibility result.

This amendment repairs the terminology before the definitive counts-only run.
It does not use an outcome result, change either analysis window, change the
cohort, or relax the prespecified gate.

## Locked corrected set

| Class | LOINC | Official long common name |
|---|---|---|
| T-score | 38263-0 | DXA Femur [T-score] Bone density |
| T-score | 38267-1 | DXA Lumbar spine [T-score] Bone density |
| T-score | 38264-8 | DXA Hip [T-score] Bone density |
| Mass/area BMD | 24701-5 | DXA Femur [Mass/Area] Bone density |
| Mass/area BMD | 24966-4 | DXA Lumbar spine [Mass/Area] Bone density |

Official terminology records:

- <https://loinc.org/38263-0>
- <https://loinc.org/38267-1>
- <https://loinc.org/38264-8>
- <https://loinc.org/24701-5>
- <https://loinc.org/24966-4>

The active-CDR audit additionally fails if a resolved concept name does not
contain the expected `T-score` or `Mass/Area` semantic label.

## Superseded candidate interpretations

- `38263-6` and `38267-7` do not identify the intended official T-score terms;
  the intended codes are `38263-0` and `38267-1`.
- `46278-8` is DXA Hip - left [Mass/Area] Bone density and must not be treated as
  a femoral-neck T-score.
- `38269-3` is not the intended lumbar-spine mass/area term; the intended code
  is `24966-4`.

The original safe archive remains immutable for audit provenance. Only the
corrected rerun may be used for the final U2 feasibility conclusion.
