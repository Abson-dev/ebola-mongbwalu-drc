# Contributing

Thank you for helping improve the Mongbwalu Ebola analysis pipeline. Please read [`SECURITY.md`](SECURITY.md) first: **no patient data in commits, issues or pull requests.**

## Set-up

```bash
git clone https://github.com/Abson-dev/ebola-mongbwalu-drc.git
cd ebola-mongbwalu-drc
git config core.hooksPath .githooks        # data-protection pre-commit hook
```

```r
source("scripts/install_packages.R")      # once per machine
source("scripts/make_synthetic_data.R"); make_synthetic_data()
Sys.setenv(EBOLA_DATA_DIR = here::here("data-synthetic"))
```

## Workflow

1. Create a branch from `main`: `feat/<short-name>`, `fix/<short-name>` or `docs/<short-name>`.
2. Make focused commits with imperative messages (`Add bed occupancy table`, `Fix CT value parsing`).
3. Run the checks locally:

   ```bash
   Rscript tests/testthat.R
   Rscript -e "lintr::lint_dir('R'); lintr::lint_dir('scripts'); lintr::lint_dir('tests')"
   ```

4. Open a pull request using the template. CI must pass (data guard, lint, tests, report rendering on synthetic data).
5. Changes that affect an indicator must be described in [`CHANGELOG.md`](CHANGELOG.md), with the expected effect on the numbers.

## Code style

- [tidyverse style guide](https://style.tidyverse.org), native pipe `|>`, lines ≤ 120 characters (`.lintr`).
- Scripts in `R/` are numbered by execution order and start with a header block: purpose, inputs, outputs, author, date.
- Reusable logic goes into `R/funcs/` as documented functions (roxygen-style comments) with tests.
- Paths are built with `path_data()` / `path_output()`; never hard-code a personal path.
- Labels displayed in reports are in French; code, comments and documentation are in English.
- Patient-specific decisions go to `config/*.csv`, never into code.

## Tests

- Unit tests: one file per helper module in `tests/testthat/`.
- End-to-end tests (`test-pipeline.R`) generate synthetic data and run the pipeline in a separate R process.
- When fixing a bug, add a test that would have caught it.
