# Changelog

All notable changes are documented here. The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and the project uses [Semantic Versioning](https://semver.org/).

## [1.0.0] — 2026-11-09

First versioned release. The original scripts by Stephanie Ngai are preserved unchanged in the first commit (`Import original analysis pipeline`) for traceability.

### Added

- **Analysis plan report** (`R/07_analysis_plan.R`, `reports/analysis_plan_report.qmd`) implementing the plan of Dr A. S. Mbengue: key figures, health-care workers, socio-demographics, residence, epidemic curves, clinical and haemorrhagic signs, mortality, laboratory, delay to care, bivariate analyses (crude OR, χ²/Fisher), missing-data assessment. All tables are exported to Excel.
- Single configuration file `R/00_config.R`; data/output folders can be overridden with `EBOLA_DATA_DIR` / `EBOLA_OUTPUT_DIR`.
- `config/manual_corrections.csv` and `config/qa_exceptions.csv`, replacing patient IDs hard-coded in scripts.
- Helper functions previously missing from the repository (`R/funcs/`): `vis_epicurve()`, `bin_numbers()`, `format_tab()`, `format_tab_by2_header()`, plus `cfr_wilson()`, `plot_age_pyramid()`, `bivariate_table()`, `missing_summary()`, `format_date_fr()`, ….
- `scripts/render.R`: one command to render any report (`Rscript scripts/render.R weekly [date]`), with automatic use of the organisational Word template when it is present locally.
- Synthetic data generator, 55 unit and end-to-end tests, GitHub Actions CI (data guard, lint, tests, rendering on synthetic data), pre-commit hook against data leaks.
- Documentation: README, analysis plan variable mapping, QA rules, pipeline definitions, data folder layout, security policy.

### Changed

- Sitrep text: increase/decrease wording ("augmentation/réduction") and percentages are computed automatically and are safe when the previous week is zero; dates are displayed in French whatever the system locale.
- Sitrep: the alerts section is skipped gracefully when surveillance data is not available; empty tables show a message instead of failing.
- Excel files are read with `readxl` and a large `guess_max`, so sparse columns (e.g. CT values of later samples) are no longer mistyped and lost.
- `rio`, `apyramid`, `epitabulate`, `plyr` and `pacman` are no longer required for the routine pipeline; packages are not installed silently at run time.
- Output file names no longer contain spaces or accents (`Sitrep_Hebdomadaire_Epidemio_S45_20261108.docx`).

### Fixed

- **QA — CT value checks never ran**: `matches("^lab_NP_")` did not match the cleaned names (`lab_np_n`).
- QA — lower CT threshold for sample 2 was 14 instead of 15.
- CTE — non-cases transferred to another facility were silently dropped from the delay plots (`"Transféré (non cas)"` label did not exist).
- CTE — health area table showed `ZS NA - NA` for patients with unknown residence.
- CTE — the secondary diagnosis was not shown when the principal diagnosis was missing.
- Weekly table — children aged 5+ with a non-integer age in years were classified as "Inconnu".
- DHIS2 script — `pacman::p_load(gghighlight())` error; the epicontacts GitHub package was reinstalled at every run.
