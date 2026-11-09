## Summary

<!-- What does this change and why? -->

## Effect on indicators

<!-- Does any number in the sitrep / analysis report change? Which ones, and by how much (synthetic data)? -->

## Checklist

- [ ] **No patient data** in the diff, description, screenshots or logs
- [ ] `Rscript tests/testthat.R` passes; tests added for new logic
- [ ] Lint clean (`lintr::lint_dir("R")`)
- [ ] Reports render on synthetic data
- [ ] `CHANGELOG.md` updated
- [ ] Documentation updated (`docs/`, `README.md`) if behaviour changed
