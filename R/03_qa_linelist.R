################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   3. Line list data quality (QA)
#
#   Standalone script. Flags inconsistent / missing records and exports a
#   checklist for the data team:
#       <output_dir>/data quality/YYYYMMDD_ll_qa.xlsx
#
#   Rules are documented in docs/data-quality-rules.md. Records verified by
#   the clinical team are exempted through config/qa_exceptions.csv.
#
#   Author: Stephanie Ngai (original), refactored by Aboubacar Hema
#   Last updated: 9 November 2026
#
################################################################################


# Cut-off dates -----------------------------------------------------------
#  Data cut-off: today by default (set `cutoff` before sourcing to override)
#  Error cut-off: only records admitted in the last 7 days are exported

if (!exists("cutoff")) cutoff <- Sys.Date()
if (!exists("err_cutoff")) err_cutoff <- cutoff - 7


# Setup and import --------------------------------------------------------

report_date <- cutoff
source(here::here("R", "01_setup.R"), encoding = "UTF-8")

filepath <- latest_file(path_data("linelist"), cfg$linelist_pattern)
message("Line list: ", basename(filepath))

dat <- read_sheet(filepath, sheet = "linelist") |>
  remove_empty(which = "rows") |>
  # Placeholder rows (ID only)
  filter(!(is.na(age) & is.na(sex) & is.na(date_admission_eff)))


##### Dates and derived variables

DATEVARS <- grep("date", names(dat), value = TRUE, ignore.case = TRUE)

dat <- dat |>
  mutate(
    across(all_of(DATEVARS), as.Date),
    date_death = case_when(type_of_exit == "Décédé" ~ date_exit_rep),
    time_since_adm = as.numeric(cutoff - date_admission_eff),
    # CT values (names are lower case after janitor::clean_names())
    across(matches("^lab_np_"), ~ suppressWarnings(as.numeric(.x))),
    exit_no_result = str_to_title(exit_no_result)
  )


##### Combine lab results

lab_cols <- grep("^lab_result_", names(dat), value = TRUE)

dat <- dat |>
  tidyr::unite("lab_results", all_of(lab_cols), sep = "; ", na.rm = TRUE, remove = FALSE) |>
  mutate(lab_results = na_if(lab_results, ""),
         n_neg = str_count(replace_na(lab_results, ""), "Nég"))


# Error codes -------------------------------------------------------------

ct_lo <- cfg$ct_range[1]
ct_hi <- cfg$ct_range[2]

# Exit flag
dat <- dat |>
  mutate(exited = case_when(!is.na(type_of_exit)  ~ 1,
                            !is.na(date_exit_eff) ~ 1,
                            !is.na(date_exit_rep) ~ 1,
                            date_admission_eff < cfg$historic_exit_date ~ 1,
                            TRUE ~ 0))

# err0: duplicated IDs
dat <- dat |>
  add_count(patient_site_id, name = ".n_id") |>
  mutate(err0 = case_when(!is.na(patient_site_id) & .n_id > 1 ~ "ID doublon")) |>
  select(-.n_id)

# err1: exit date without exit type, or the reverse
dat <- dat |>
  mutate(err1 = case_when(
    !is.na(date_exit_eff) & is.na(type_of_exit) ~ "Date de sortie mais pas type de sortie",
    is.na(date_exit_eff) & !is.na(type_of_exit) ~ "Type de sortie mais pas date de sortie"
  ))

# err2: no exit although non-case, or hospitalised > 14 days
dat <- dat |>
  mutate(err2 = case_when(
    evd_status == "Non cas" & is.na(date_exit_eff) ~ "Non cas sans date de sortie",
    is.na(date_exit_eff) & time_since_adm > 14 ~ "Pas de sortie mais hospitalisé 14+j, confirmer"
  ))

# err3: invalid dates
dat <- dat |>
  mutate(
    err3a = case_when(is.na(date_admission_eff) ~ "Date d'admission manquante",
                      date_admission_eff > cutoff ~ "Date d'admission dans le futur"),
    err3b = case_when(is.na(date_admission_rep) ~ "Date d'admission manquante",
                      date_admission_rep > cutoff ~ "Date d'admission dans le futur"),
    err3c = case_when(date_exit_eff > cutoff ~ "Date de sortie dans le futur",
                      date_exit_eff < date_admission_eff ~ "Date de sortie avant la date d'admission"),
    err3d = case_when(date_exit_rep > cutoff ~ "Date de sortie dans le futur",
                      date_exit_rep < date_admission_eff ~ "Date de sortie avant la date d'admission"),
    err3e = case_when(date_admission_eff - date_symptom_onset > 30 ~ "Vérifier date de début des symptomes")
  )

# err4: exit type inconsistent with case status
dat <- dat |>
  mutate(err4 = case_when(
    is.na(type_of_exit) ~ NA,
    evd_status == "Confirmé" & grepl("non cas", type_of_exit) ~ "Confirmé avec sortie non cas",
    evd_status == "Non cas" & type_of_exit == "Guéri" ~ "Non cas avec sortie cas",
    evd_status == "Suspect" & grepl("non cas", type_of_exit) ~ "Suspect avec sortie non cas",
    evd_status == "Suspect" & type_of_exit == "Guéri" ~ "Suspect avec sortie confirmé"
  ))

# err5: lab results inconsistent with case status
dat <- dat |>
  mutate(has_pos = grepl("Pos", lab_results, ignore.case = TRUE),
         err5 = case_when(
           evd_status == "Confirmé" & is.na(lab_results) ~ "Cas confirmé sans resultat",
           evd_status == "Confirmé" & !has_pos ~ "Cas confirmé sans resultat positif",
           evd_status != "Confirmé" & has_pos ~ "Resultat positif, verifier statut EVD",
           evd_status == "Non cas" & is.na(lab_results) ~ "Non cas sans resultats",
           evd_status == "Non cas" & n_neg < 2 ~ "Non cas sans 2 resultats negatifs"
         )) |>
  select(-has_pos)

# err6: key demographic variables
dat <- dat |>
  mutate(
    err6a = case_when(is.na(sex) ~ "Sexe manquant"),
    err6b = case_when(is.na(age) ~ "Age manquant"),
    err6c = case_when(patient_site_id %in% qa_exempt("err6c") ~ NA,
                      is.na(age_unit) ~ "Unité d'age manquante"),
    err6d = case_when(age_unit == "Mois" & age > 12 ~ "Confirmer l'unité d'age"),
    err6e = case_when(is.na(pregnant) & sex == "F" & age_unit == "Ans" & age >= 13 & age <= 50 ~
                        "Statut de grossesse/allaitante manquant")
  )

# err7: probable cases after June 2026
dat <- dat |>
  mutate(err7 = case_when(evd_status == "Probable" & date_admission_eff > as.Date("2026-06-01") ~
                            "Cas probable, confirmer si le cas correspond a la definition"))

# err8: geography
dat <- dat |>
  mutate(
    err8a = case_when(is.na(adm2_name_res) ~ "Zone de Santé manquante"),
    err8b = case_when(is.na(adm3_name_res) ~ "Aire de Santé manquante"),
    err8c = case_when(adm3_name_res %in% c("Saio", "Sayo") ~ "Selectionner Saïo dans le menu deroulant"),
    err8d = case_when(adm3_name_res == "Nzebi" ~ "Selectionner AS dans le menu deroulant")
  )

# err9: suspected case who died
dat <- dat |>
  mutate(err9 = case_when(evd_status == "Suspect" & type_of_exit == "Décédé" ~
                            "Décès suspect, vérifier résultat swab"))

# err10: suspected/probable with too many results
dat <- dat |>
  mutate(err10 = case_when(
    patient_site_id %in% qa_exempt("err10") ~ NA,
    evd_status == "Suspect" & !is.na(lab_result_1) & !is.na(lab_result_2) ~ "Cas suspect avec 2 resultats",
    evd_status == "Probable" & !is.na(lab_results) ~ "Cas probable avec resultat"
  ))

# err13: CT values out of range, or CT reported with negative result
ct_rule <- function(result, ct, i) {
  case_when(
    result == "Positif" & ct > ct_hi ~ sprintf("Verifier valeur CT%d (plus de %d)", i, ct_hi),
    result == "Positif" & ct < ct_lo ~ sprintf("Verifier valeur CT%d (moins de %d)", i, ct_lo),
    result %in% c("Négatif", "Inconclusif") & !is.na(ct) ~ sprintf("CT%d present mais resultat neg/inc", i)
  )
}
n_ct <- sum(paste0("lab_np_", 1:20) %in% names(dat))
for (i in seq_len(min(n_ct, 5))) {
  dat[[paste0("err13_", i)]] <- ct_rule(dat[[paste0("lab_result_", i)]],
                                        dat[[paste0("lab_np_", i)]], i)
}

# err15: suspected case waiting for lab results
dat <- dat |>
  mutate(err15 = case_when(
    exit_no_result == "Oui" ~ NA,
    evd_status == "Suspect" & time_since_adm > 1 & is.na(lab_result_1) ~ "Chercher resultat labo 1",
    patient_site_id %in% qa_exempt("err15") ~ NA,
    evd_status == "Suspect" & time_since_adm > 2 & is.na(lab_result_2) ~ "Chercher resultat labo 2"
  ))

# err17: lab sample dates
dat <- dat |>
  mutate(
    err17a = case_when(
      patient_site_id %in% qa_exempt("err17a") ~ NA,
      is.na(lab_result_1) ~ NA,
      is.na(date_lab_sample_1) ~ "Date prélèvement 1 manquante",
      date_lab_sample_1 < date_admission_eff ~ "Date prélèvement 1 avant admission"),
    err17b = case_when(
      patient_site_id %in% qa_exempt("err17b") ~ NA,
      is.na(lab_result_2) ~ NA,
      is.na(date_lab_sample_2) ~ "Date prélèvement 2 manquante",
      date_lab_sample_2 < date_admission_eff ~ "Date prélèvement 2 avant admission",
      date_lab_sample_2 < date_lab_sample_1 ~ "Date prélèvement 2 avant prélèvement 1",
      date_lab_sample_2 < date_lab_sample_1 + 2 ~ "Date prélèvement 2 trop tot après prélèvement 1"),
    err17c = case_when(
      is.na(lab_result_3) ~ NA,
      is.na(date_lab_sample_3) ~ "Date prélèvement 3 manquante",
      date_lab_sample_3 < date_admission_eff ~ "Date prélèvement 3 avant admission"),
    err17d = case_when(
      is.na(lab_result_4) ~ NA,
      is.na(date_lab_sample_4) ~ "Date prélèvement 4 manquante",
      date_lab_sample_4 < date_admission_eff ~ "Date prélèvement 4 avant admission")
  )

# Epi errors (case investigation form) - kept separate, many missing values
dat <- dat |>
  mutate(epi_err1 = case_when(is.na(contact_evd_case) ~ "Vérifier fiche d'investigation"),
         epi_err2 = case_when(is.na(vaccination_rvsv_yn) ~ "Vérifier statut de vaccination"))


# Review in the console ---------------------------------------------------

qa_summary <- dat |>
  mutate(period = case_when(date_admission_eff > cutoff - 7  ~ "< 7 jours",
                            date_admission_eff > cutoff - 30 ~ "7-30 jours",
                            TRUE ~ "> 30 jours")) |>
  select(period, matches("^err")) |>
  pivot_longer(-period, names_to = "rule", values_to = "error", values_drop_na = TRUE) |>
  count(period, error) |>
  pivot_wider(names_from = period, values_from = n, values_fill = 0) |>
  select(error, any_of(c("< 7 jours", "7-30 jours", "> 30 jours"))) |>
  arrange(desc(across(any_of(c("< 7 jours", "7-30 jours", "> 30 jours")))))

print(qa_summary, n = Inf)


# Export ------------------------------------------------------------------

qa_export <- dat |>
  collapse_errors("^err") |>
  select(patient_site_id, date_admission_eff, errors) |>
  mutate(reponse = "")

qa_export_cut <- qa_export |>
  filter(date_admission_eff >= err_cutoff)

write_xlsx_table(qa_export_cut,
                 path_output("data quality", paste0(format(cutoff, "%Y%m%d"), "_ll_qa.xlsx")))


# Optional exports (set to TRUE as needed) --------------------------------

export_extra <- FALSE

qa_export_lab <- dat |>
  filter(!is.na(err5) | !is.na(err10) | !is.na(err15)) |>
  collapse_errors("^err") |>
  select(patient_site_id, date_admission_eff, errors) |>
  mutate(reponse = "") |>
  filter(date_admission_eff >= as.Date("2026-09-01")) |>
  arrange(desc(patient_site_id))

qa_export_epi <- dat |>
  collapse_errors("^epi_err") |>
  select(patient_site_id, date_admission_eff, errors) |>
  mutate(reponse = "") |>
  filter(date_admission_eff >= err_cutoff)

qa_export_old <- dat |>
  filter(!is.na(err1) | !is.na(err2) | err17a %in% "Date prélèvement 1 manquante") |>
  select(-err6e) |>
  collapse_errors("^err") |>
  select(patient_site_id, date_admission_eff, errors) |>
  mutate(reponse = "")

qa_export_ct <- dat |>
  filter((lab_result_1 %in% "Positif" & is.na(lab_np_1)) |
           (lab_result_2 %in% "Positif" & is.na(lab_np_2))) |>
  select(patient_site_id, date_admission_eff) |>
  mutate(errors = "Valeur CT manquante", reponse = "")

if (export_extra) {
  stamp <- format(cutoff, "%Y%m%d")
  write_xlsx_table(qa_export_lab, path_output("data quality", paste0(stamp, "_ll_qa_lab.xlsx")))
  write_xlsx_table(qa_export_epi, path_output("data quality", paste0(stamp, "_ll_qa_investigation.xlsx")))
  write_xlsx_table(qa_export_old, path_output("data quality", paste0(stamp, "_ll_qa_ancien_cas.xlsx")))
  write_xlsx_table(qa_export_ct,  path_output("data quality", paste0(stamp, "_ll_qa_CT.xlsx")))
}
