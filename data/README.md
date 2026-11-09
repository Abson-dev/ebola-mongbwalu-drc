# Input data — NOT version-controlled

> [!CAUTION]
> Everything in this folder except this file is ignored by git. Never commit, upload or share patient-level data through GitHub. See [`../SECURITY.md`](../SECURITY.md).

```
data/
├── linelist/      CTE line list export(s)        REQUIRED
├── daily/         Daily data entry.xlsx          optional (alerts section of the sitrep)
├── population/    Population_AS2026_Ituri_dhis2.xlsx   optional (alert rates)
└── dhis2/         DHIS2 line list export(s)      R/08_analysis_dhis.R only
```

When several files are present in `linelist/` or `dhis2/`, the **most recently modified** `.xlsx` is used. Close Excel before running the pipeline: lock files (`~$…`) are ignored, but an open workbook may not be fully saved.

## `linelist/*.xlsx` — MSF EVD line list template (v4.4)

Export the line list as `.xlsx`, **without password protection**.

| Sheet | Used columns (original names) |
|---|---|
| `linelist` | `patient_site_id`, `sex`, `age`, `age_unit`, `pregnant`, `breastfeeding`, `job`, `HF_name_job`, `adm2_name__res`, `adm3_name__res`, `adm4_name__res`, `date_symptom_onset`, `dead_upon_arrival`, `date_admission_rep`, `date_admission_eff`, `date_notification`, `date_exit_rep`, `date_exit_eff`, `type_of_exit`, `exit_no_result`, `EVD_status`, `diagnostic_principal`, `diagnostic_secondary`, `diagnostic_other_detail`, symptom columns (`fever` … `bleeding_other`), `lab_result_n`, `date_lab_sample_n`, `lab_NP_n` (CT), `contact_EVD_case`, `vaccination_rvsv_yn` |
| `occupancy` | `date`, `occupied_S`, `occupied_C`, `occupied_total`, `dispo_SP`, *(unnamed column 9 = confirmed beds)*, `occupation_SP`, `occupation_C`, `dispo_total`, `occupation_total` |

Expected categories: `EVD_status` ∈ {Confirmé, Probable, Suspect, Non cas}; `age_unit` ∈ {Ans, Mois, Jour}; yes/no fields ∈ {Oui, Non, Inc.}.

## `daily/Daily data entry.xlsx`

Sheet `Alerts_Det_Data`: `date`, `AS`, `reports_vivants`, `alertes_vivants`, `report_deces`, `alertes_deces`, `vivants_validees`, `cas_suspect_cte`, `cas_suspects_non_transferes`.

## `population/Population_AS2026_Ituri_dhis2.xlsx`

Columns `zone_de_sante`, `aire_de_sante`, `population_de_l_aire_de_sante`.

## `dhis2/*.xlsx`

DHIS2 tracker line list export (the first row is a title row and is skipped).

## Synthetic data

`Rscript scripts/make_synthetic_data.R` creates `data-synthetic/` with the same structure and randomly generated, fictitious patients. Use it for demos, development and tests:

```r
Sys.setenv(EBOLA_DATA_DIR = here::here("data-synthetic"))
```
