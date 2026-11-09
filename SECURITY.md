# Security and data protection policy

This repository supports the medical humanitarian response to an Ebola outbreak. The **line list contains identifying and clinical information** about patients and, in free-text fields, about their relatives and contacts. Protecting this information is a **hard requirement**.

## What may be committed

| ✅ Allowed | ❌ Never |
|---|---|
| R code, Quarto sources, documentation | Line lists, DHIS2 exports, daily surveillance files (any format) |
| `config/*.csv` (pseudonymous site IDs + reason only) | Rendered reports (`.docx`, `.pdf`, `.html`) and Excel outputs |
| The neutral Word template `reports/templates/reference.docx` | Organisational templates containing past sitrep content, logos |
| Synthetic data **generators** | Screenshots or console output that show patient records |
| Aggregate numbers in issues only when strictly necessary | Names, phone numbers, villages, free-text clinical notes |

## Safeguards

1. **`.gitignore`** excludes `data/`, `output/`, `private/`, `data-synthetic/`, and all spreadsheet, document and serialised R formats.
2. **Pre-commit hook.** [`.githooks/pre-commit`](.githooks/pre-commit) blocks staged data files and obvious identifiers. Enable it once per clone:

   ```bash
   git config core.hooksPath .githooks
   ```

3. **CI data guard.** The `data-guard` job fails the build if a data or document file is tracked.
4. **Synthetic data only** for tests, CI and demonstrations ([`scripts/make_synthetic_data.R`](scripts/make_synthetic_data.R)).
5. **Private repository.** Access is limited to the people who need it. Review collaborators regularly.

## If patient data was committed or pushed

Act immediately, and **do not** simply delete the file in a new commit: it would remain in the history.

1. Make the repository inaccessible (restrict access) and inform the MSF epidemiology/data protection focal point.
2. Rewrite history to remove the file (e.g. [`git filter-repo`](https://github.com/newren/git-filter-repo) `--path <file> --invert-paths`), force-push, and ask every collaborator to re-clone.
3. Contact GitHub Support to purge cached views and pull request references.
4. Document the incident according to MSF data protection procedures.

## Reporting a vulnerability

Please report security or data protection concerns privately to the maintainers (see [`CITATION.cff`](CITATION.cff)) rather than through a public issue.
