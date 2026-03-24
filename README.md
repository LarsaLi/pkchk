# pkchk

<!-- badges: start -->
<!-- badges: end -->

`pkchk` is an R package for NONMEM PK data review and quality checks with a Shiny app interface.

## Current MVP

- Generate dummy ADPPK data (`SAD` / `MAD`)
- Upload ADPPK (`.csv` / `.xlsx`) or load built-in example data
- Run modular checks (each check is an independent function)
- View summary and basic visualization
- Export checklist summary report (`.csv`)
- Export checklist report (`.html`) with severity levels (`error` / `warn` / `info`)
- YAML-driven check profile (`inst/config/checks_default.yml`)
- Checklist-to-function mapping doc (`inst/extdata/checklist_mapping.md`)

## Installation

```r
# install.packages("remotes")
remotes::install_github("LarsaLi/pkchk")
```

## Run app

```r
library(pkchk)

run_app()          # existing dashboard app
# optional teal UI (install teal + teal.data + teal.widgets + teal.modules.general + teal.reporter)
# run_teal_app()
```

## Teal quick profiles

The teal check module supports built-in quick profiles:

- `default`
- `sad_strict`
- `mad_sparse`
- `poppk_pool`

You can also upload a custom YAML config to override the selected profile.


## Core functions

- `generate_dummy_adppk()`
- `check_required_vars()`
- `check_name_label_len()`
- `check_pk_no_dose()`
- `check_poppk_consistency()`
- `check_char_num_mapping()`
- `run_checks()`
