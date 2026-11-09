# Pipeline and definitions

## Execution model

The scripts are plain R scripts sourced in order, which suits a field epidemiologist working interactively in RStudio. Each script documents what it requires and what it creates in its header.

| Entry point | Sources | Purpose |
|---|---|---|
| `reports/weekly_report.qmd` | 01 → 02_linelist → 04 (+ 02_daily → 05 if surveillance data exists) | Weekly sitrep |
| `reports/analysis_plan_report.qmd` | 01 → 02_linelist → 04 → 07 | Analysis plan report |
| `R/03_qa_linelist.R` | 01 | Daily QA checklist |
| `R/06_weekly_table.R` | 01 → 02_linelist | Monday table |
| `R/08_analysis_dhis.R` | 01 → 02_dhis | Exploratory DHIS2 analyses |

`scripts/render.R` renders a report with Quarto in a fresh R session. It passes the report date as a Quarto parameter and moves the document to `output/reports/`.

## Configuration

All settings live in [`R/00_config.R`](../R/00_config.R) (`cfg` list):

| Setting | Default | Meaning |
|---|---|---|
| `data_dir` | `data/` (env `EBOLA_DATA_DIR`) | Input folder |
| `output_dir` | `output/` (env `EBOLA_OUTPUT_DIR`) | Output folder |
| `historic_exit_date` | 2026-07-01 | Patients admitted earlier are considered exited |
| `age_breaks_sitrep` | 0, 5, 15, 45 | Sitrep age groups |
| `age_breaks_plan` | 0, 5, 15, 45, 50 | Analysis plan age groups |
| `delay_breaks` / `delay_labels` | < 1, 1-2, 3-5, > 5 days | Delay to care classes |
| `serial_interval` | mean 11.61, sd 5.2 days | Rt estimation (Bundibugyo/Beni literature) |
| `ct_range` | 15–40 | Plausible CT values (QA) |

## Key definitions

**Reporting period.** `report_date` is the end (Sunday) of the last complete epidemiological week, unless set explicitly. Weeks start on Monday and are labelled `YYYY-Sww` (French notation, e.g. `2026-S45`). `week1`, `week2` and `week3` are the last three weeks; `date_start_3w` is the Monday 20 days before the report date.

**Population of the CTE indicators (`cte`).** Patients admitted up to the report date, **excluding deaths on arrival** (reported separately).

**Case status.** Final classification (`evd_status`): Confirmé, Probable, Suspect (awaiting classification), Non cas.

**Exits.** A patient has exited if an exit type or exit date is recorded, or if they were admitted before `historic_exit_date`. `type_of_exit3` groups the two "return home (non-case)" categories.

**Dates.** Admission and exit indicators use effective dates (`*_eff`). Weekly series `week_adm` / `week_exit` use reported dates (`*_rep`), as in the original pipeline.

**Case fatality ratio.** Deaths / (deaths + recoveries) among confirmed cases with an outcome, with a Wilson score 95% confidence interval. Dropouts and unknown outcomes are excluded from the denominator.

**Length of stay and delays.** Exit − admission; admission − onset; exit − onset. Negative values are treated as data errors and set to missing.

## Extending the pipeline

- Add an indicator to the sitrep: compute it in `04_analysis_cte.R` (or `05_…`) and reference the object in `weekly_report.qmd`.
- Add a reusable function: put it in `R/funcs/`, document it with roxygen-style comments and add a test in `tests/testthat/`.
- Correct a record: add a line to `config/manual_corrections.csv` (never edit the raw export).
- Silence a verified QA flag: add a line to `config/qa_exceptions.csv`.

Run `Rscript tests/testthat.R` before opening a pull request.
