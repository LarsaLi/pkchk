# pkchk checklist mapping (M4)

This document maps the provided NONMEM PK checklist to implemented `pkchk` checks.

| # | Checklist item | Function | Status | Notes |
|---|---|---|---|---|
| 1 | CDISC ADPPK required/conditional vars + name/label limits | `check_required_vars()`, `check_name_label_len()` | Implemented | Conditional rules can be expanded by study design |
| 2 | PK records but no dosing records | `check_pk_no_dose()` | Implemented | Uses ADDOSE or derived dosing subset |
| 3 | Subjects not in POPPK + reasons | `check_poppk_consistency()` | Implemented | Subject-level count included in message |
| 4 | PK records not in POPPK + reasons | `check_poppk_consistency()` | Implemented | Record-level table returned |
| 5 | 1:1 char/num mapping (RACE/RACEN) | `check_char_num_mapping()` | Implemented | Extendable to more pair mappings |
| 6 | No character truncation | `check_char_truncation()` | Implemented | Heuristic-based detection |
| 7 | Fixed covariates stable within subject(/period) | `check_fixed_covariates()` | Implemented | Period-specific rule can be added |
| 8 | Predose actual time <= 0 | `check_predose_time()` | Implemented | Uses ATPT/TIME conventions |
| 9 | Actual sampling deviation >10% from nominal | `check_sampling_deviation()` | Implemented | Threshold configurable via YAML |
|10 | Unexpected dose/concentration values | `check_unexpected_values()` | Implemented | IQR-based extreme detection |
|11 | Covariate outliers | `check_covariate_outliers()` | Implemented | IQR-based extreme detection |
|12 | Large nominal/actual relative time deviations | `check_nominal_actual_deviation()` | Implemented | Absolute threshold configurable |
|13 | Nominal and actual time consistency | `check_nominal_actual_consistency()` | Implemented | Order consistency check |
|14 | Missingness by EVID | `check_missing_by_evid()` | Implemented | Full table returned |
|15 | Duplicate records | `check_duplicates()` | Implemented | Key-based duplicate logic |
|16 | IDs/dose/time/age expected ranges | `check_expected_ranges()` | Implemented | Range rules can be study-specific |
|17 | BLOQ in middle of profile | `check_bloq_middle()` | Implemented | Per subject-param profile |
|18 | Abnormally high predose concentrations | `check_high_predose()` | Implemented | Threshold configurable |
|19 | AMT 0 or missing for dosing records | `check_amt_for_dose()` | Implemented | Requires EVID/AMT |
|20 | MDV assigned by DV presence | `check_mdv_assignment()` | Implemented | Requires MDV/DV |
|21 | EVID=4 once per dosing period | `check_evid4_once()` | Implemented | Needs period variable or ADY fallback |
|22 | Actual times increase sequentially | `check_time_sequential()` | Implemented | Per subject monotonicity |
|23 | Names/labels/formats align across studies | `check_cross_study_alignment()` | Implemented | Type alignment baseline |
|24 | Standardize categorical/numeric values | `check_standardized_values()` | Implemented | Baseline normalization checks |

## Config-driven behavior (YAML)

Use `inst/config/checks_default.yml` as template.

- Enable/disable checks by ID
- Override severity (`error/warn/info`)
- Set selected thresholds (where supported)
