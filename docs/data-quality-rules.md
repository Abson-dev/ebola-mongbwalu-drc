# Data quality rules

Implemented in [`R/03_qa_linelist.R`](../R/03_qa_linelist.R). The script flags records of the CTE line list and exports a checklist (`patient_site_id`, admission date, errors, empty `reponse` column for the data team) for records admitted during the last 7 days:

```bash
Rscript R/03_qa_linelist.R          # cut-off = today
```

To use another cut-off, set `cutoff <- as.Date("2026-11-01")` (and optionally `err_cutoff`) before sourcing the script.

Records verified by the clinical team that legitimately trigger a rule are listed in [`config/qa_exceptions.csv`](../config/qa_exceptions.csv) (`rule`, `patient_site_id`, `reason`).

| Rule | Check | Message (exported) |
|---|---|---|
| `err0` | Duplicated `patient_site_id` | ID doublon |
| `err1` | Exit date without exit type, or the reverse | Date de sortie mais pas type de sortie / Type de sortie mais pas date de sortie |
| `err2` | Non-case without exit date; no exit after > 14 days of hospitalisation | Non cas sans date de sortie / Pas de sortie mais hospitalisé 14+j, confirmer |
| `err3a`–`err3b` | Missing or future admission date (effective / reported) | Date d'admission manquante / dans le futur |
| `err3c`–`err3d` | Exit date in the future or before admission | Date de sortie dans le futur / avant la date d'admission |
| `err3e` | Onset more than 30 days before admission | Vérifier date de début des symptomes |
| `err4` | Exit type inconsistent with case status (e.g. confirmed with a non-case exit) | Confirmé avec sortie non cas, … |
| `err5` | Lab results inconsistent with status: confirmed without (positive) result; positive result but not confirmed; non-case without two negative results | Cas confirmé sans resultat positif, … |
| `err6a`–`err6e` | Missing sex, age, age unit; age > 12 in months; missing pregnancy status for women 13–50 years | Sexe manquant, … |
| `err7` | Probable case admitted after 1 June 2026 | Cas probable, confirmer si le cas correspond a la definition |
| `err8a`–`err8d` | Missing health zone / area; free-text area instead of the drop-down value | Zone de Santé manquante, … |
| `err9` | Suspected case who died (post-mortem swab result to check) | Décès suspect, vérifier résultat swab |
| `err10` | Suspected case with 2 results; probable case with a result | Cas suspect avec 2 resultats, … |
| `err13_1`–`err13_5` | CT value outside [15, 40] for a positive result; CT value reported for a negative/inconclusive result | Verifier valeur CTn (plus de 40 / moins de 15), CTn present mais resultat neg/inc |
| `err15` | Suspected case still waiting for lab result 1 (> 1 day) or 2 (> 2 days) | Chercher resultat labo 1 / 2 |
| `err17a`–`err17d` | Missing sample date, sample before admission, sample 2 before or < 2 days after sample 1 | Date prélèvement n manquante, … |
| `epi_err1`–`epi_err2` | Case investigation form not filled; vaccination status missing | Vérifier fiche d'investigation, … |

## Optional exports

Set `export_extra <- TRUE` in the script to also export:

- lab-related errors (`_ll_qa_lab.xlsx`),
- case investigation form errors (`_ll_qa_investigation.xlsx`),
- older records with exit or sample 1 errors (`_ll_qa_ancien_cas.xlsx`),
- positive results without CT value (`_ll_qa_CT.xlsx`).

## Changes compared to the original script

- CT checks (`err13`) did not run: the regular expression `^lab_NP_` never matched the names produced by `janitor::clean_names()` (`lab_np_1`). The CT values therefore stayed as text and the rule was silently ignored. Fixed.
- The lower CT threshold for sample 2 was 14 while the message said 15; it is now 15 for every sample, set in `cfg$ct_range`.
- `err13` used to report only the first CT problem of a record; it now reports one per sample.
- `exit_no_result` is normalised (`"oui"` → `"Oui"`).
- Patient-specific exemptions are no longer hard-coded; they live in `config/qa_exceptions.csv`.
