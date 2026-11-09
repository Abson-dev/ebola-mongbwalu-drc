################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   Linelist data quality
#
#   Author: Stephanie Ngai
#   Last updated: 22 September 2026
#
################################################


# Set cutoff dates --------------------------------------------------------

##### Data cutoff dates
#     By default, system date, or can change manually based on when linelist was updated

cutoff <- Sys.Date()
# cutoff <- as.Date("2026-09-28")


##### Error cutoff date
#     By default, one week of data, or can change manually

err_cutoff <- cutoff-7
# err_cutoff <- as.Date("2026-09-01")



# Source setup file -------------------------------------------------------

source(here::here("R", "01_setup.R"))



# Import linelist ---------------------------------------------------------

##### Filepath

filepath <- file.info(list.files(here::here("data", "linelist"), full.names = TRUE, pattern = "*.xlsx$")) %>%
  rownames_to_column() %>%
  filter(grepl("linelist", rowname)) %>%
  filter(mtime == max(mtime)) %>%
  pull(rowname)


##### Import and remove empty rows

dat_raw <- rio::import(filepath, sheet = "linelist") %>%
  janitor::clean_names()

dat <- dat_raw %>%
  remove_empty(which = "rows")


##### Remove placeholder rows

dat <- dat %>%
  filter(!(is.na(age) & is.na(sex) & is.na(date_admission_eff)))


##### Dates

DATEVARS <- names(dat)[grep("date", names(dat), ignore.case = TRUE)]

dat <- dat %>%
  mutate(
    across(all_of(DATEVARS), as.Date),
    
    ## Deaths
    date_death = case_when(type_of_exit == "Décédé" ~ date_exit_rep),
    
    ## Time since admissin
    time_since_adm = as.numeric(difftime(cutoff, date_admission_eff, units = "days"))

  )


##### Convert lab values to numeric

dat <- dat %>%
  mutate(across(matches("^lab_NP_"), as.numeric))


##### Combine lab results

dat <- dat %>%
  rowwise() %>%
  mutate(lab_results = paste(na.omit(c_across(starts_with("lab_result_"))), collapse = "; ")) %>%
  mutate(lab_results = na_if(lab_results, "")) %>%
  mutate(n_neg = str_count(lab_results, "Nég"))


##### Collapse symptoms
# dat <- dat %>%
#   rowwise() %>%
#   mutate(sx = paste(na.omit(c_across(nausea:other_symptoms))), collapse = "; ")



# Error codes -------------------------------------------------------------

ids <- dat %>%
  group_by(patient_site_id) %>% 
  count() %>% arrange(n)


# Flag exits

dat <- dat %>%
  mutate(exited = case_when(!is.na(type_of_exit) ~ 1,
                            !is.na(date_exit_eff) ~ 1,
                            !is.na(date_exit_rep) ~ 1,
                            date_admission_eff < as.Date("2026-07-01") ~ 1,
                            TRUE ~ 0))


# err0: Check for duplicates

dat <- dat %>%
  group_by(patient_site_id) %>%
  mutate(err0 = case_when(is.na(patient_site_id) ~ NA,
                          n() > 1 ~ "ID doublon")) %>%
  ungroup()


# err1: missing exit date or type of exit

dat <- dat %>%
  mutate(err1 = case_when(!is.na(date_exit_eff) & is.na(type_of_exit) ~ "Date de sortie mais pas type de sortie",
                          is.na(date_exit_eff) & !is.na(type_of_exit) ~ "Type de sortie mais pas date de sortie"))


# err2: no exit info but patient a) non cas or b) greater than 14 days since admit

dat <- dat %>%
  mutate(
    err2 = case_when(evd_status == "Non cas" & is.na(date_exit_eff) ~ "Non cas sans date de sortie",
                     is.na(date_exit_eff) & time_since_adm > 14 ~ "Pas de sortie mais hospitalisé 14+j, confirmer")
  )


# err3: invalid dates

dat <- dat %>%
  mutate(err3a = case_when(is.na(date_admission_eff) ~ "Date d'admission manquante",
                           date_admission_eff > cutoff ~ "Date d'admission dans le futur"),
         err3b = case_when(is.na(date_admission_rep) ~ "Date d'admission manquante",
                           date_admission_rep > cutoff ~ "Date d'admission dans le futur"),
         err3c = case_when(!is.na(date_exit_eff) & (date_exit_eff > cutoff) ~ "Date de sortie dans le futur",
                           !is.na(date_exit_eff) & (date_exit_eff < date_admission_eff) ~ "Date de sortie avant la date d'admission"),
         err3d = case_when(!is.na(date_exit_rep) & (date_exit_rep > cutoff) ~ "Date de sortie dans le futur",
                           !is.na(date_exit_rep) & (date_exit_rep < date_admission_eff) ~ "Date de sortie avant la date d'admission"),
         err3e = case_when(!is.na(date_symptom_onset) & date_admission_eff-date_symptom_onset > 30 ~ "Vérifier date de début des symptomes"))


# err4: exit types

dat <- dat %>%
  mutate(err4 = case_when(is.na(type_of_exit) ~ NA,
                          evd_status == "Confirmé" & grepl("non cas", type_of_exit) ~ "Confirmé avec sortie non cas",
                          evd_status == "Non cas" & type_of_exit == "Guéri" ~ "Non cas avec sortie cas",
                          evd_status == "Suspect" & grepl("non cas", type_of_exit) ~ "Suspect avec sortie non cas",
                          evd_status == "Suspect" & type_of_exit == "Guéri" ~ "Suspect avec sortie confirmé"))


# err5: lab results (confirmed and non case)

dat <- dat %>%
  mutate(err5 = case_when(
    evd_status == "Confirmé" & is.na(lab_results) ~ "Cas confirmé sans resultat", 
    evd_status == "Confirmé" & grepl("Pos", lab_results, ignore.case = TRUE) == FALSE ~ "Cas confirmé sans resultat positif",
    evd_status != "Confirmé" & grepl("Pos", lab_results, ignore.case = TRUE) == TRUE ~ "Resultat positif, verifier statut EVD",
    evd_status == "Non cas" & is.na(lab_results) ~ "Non cas sans resultats",
    evd_status == "Non cas" & n_neg < 2 ~ "Non cas sans 2 resultats negatifs"))


# err6: key demographic variables

dat <- dat %>%
  mutate(err6a = case_when(is.na(sex) ~ "Sexe manquante"),
         err6b = case_when(is.na(age) ~ "Age manquante"),
         err6c = case_when(patient_site_id %in% c("MGL1052") ~ NA,
                           is.na(age_unit) ~ "Unit d'age manquant"),
         err6d = case_when(age_unit == "Mois" & age > 12 ~ "Confirmer l'unit d'age"),
         err6e = case_when(is.na(pregnant) & sex == "F" & age_unit == "Ans" & age %in% c(13:50) ~ 
                             "Statut de grossese/alliante manquant"))


# err7: probable cases

dat <- dat %>%
  mutate(err7 = case_when(evd_status == "Probable" & date_admission_eff > as.Date("2026-06-01") ~
                            "Cas probable, confirmer si le cas correspond au definition"))


# err8: geography missing

dat <- dat %>%
  mutate(err8a = case_when(is.na(adm2_name_res) ~ "Zone de Santé manquante"),
         err8b = case_when(is.na(adm3_name_res) ~ "Aire de Santé manquante"),
         err8c = case_when(adm3_name_res %in% c("Saio", "Sayo") ~ "Selectionner Saïo dans le menu deroulant"),
         err8d = case_when(adm3_name_res == "Nzebi" ~ "Selectionner AS dans le menu deroulant"))

# err9: suspected death

dat <- dat %>%
  mutate(err9 = case_when(evd_status == "Suspect" & type_of_exit == "Décédé" ~ "Décès suspect, vérifier résultat swab"))


# err10: more lab results (suspected and probable)

dat <- dat %>%
  mutate(err10 = case_when(
    patient_site_id %in% c("MGL0664", "MGL0692", "MGL0699",
                           "MGL0700", "MGL0675", "MGL0692") ~ NA,
    evd_status == "Suspect" & !is.na(lab_result_1) & !is.na(lab_result_2) ~ "Cas suspect avec 2 resultats",
    evd_status == "Probable" & !is.na(lab_results) ~ "Cas probable avec resultat"))


# err11


# err12


# err13

dat <- dat %>%
  mutate(
    # err13: positive with out of range values OR negative / inconclusive with CT
    err13 = case_when(
      lab_result_1 == "Positif" & (lab_np_1 > 40) ~ "Verifier valeur CT1 (plus de 40)",
      lab_result_1 == "Positif" & (lab_np_1 < 15) ~ "Verifier valeur CT1 (moins de 15)",
      lab_result_2 == "Positif" & (lab_np_2 > 40) ~ "Verifier valeur CT2 (plus de 40)",
      lab_result_2 == "Positif" & (lab_np_2 < 14) ~ "Verifier valeur CT2 (moins de 15)",
      lab_result_3 == "Positif" & (lab_np_3 > 40) ~ "Verifier valeur CT3 (plus de 40)",
      lab_result_3 == "Positif" & (lab_np_3 < 15) ~ "Verifier valeur CT3 (moins de 15)",
      lab_result_4 == "Positif" & (lab_np_4 > 40) ~ "Verifier valeur CT4 (plus de 40)",
      lab_result_4 == "Positif" & (lab_np_4 < 15) ~ "Verifier valeur CT4 (moins de 15)",
      lab_result_5 == "Positif" & (lab_np_5 > 40) ~ "Verifier valeur CT5 (plus de 40)",
      lab_result_5 == "Positif" & (lab_np_5 < 15) ~ "Verifier valeur CT5 (moins de 15)",
      # lab_result_6 == "Positif" & (lab_np_6 > 40) ~ "Verifier valeur CT6 (plus de 40)",
      # lab_result_6 == "Positif" & (lab_np_6 < 15) ~ "Verifier valeur CT6 (moins de 15)",
      # lab_result_7 == "Positif" & (lab_np_7 > 40) ~ "Verifier valeur CT7 (plus de 40)",
      # lab_result_7 == "Positif" & (lab_np_7 < 15) ~ "Verifier valeur CT7 (moins de 15)",
      # lab_result_8 == "Positif" & (lab_np_8 > 40) ~ "Verifier valeur CT8 (plus de 40)",
      # lab_result_8 == "Positif" & (lab_np_8 < 15) ~ "Verifier valeur CT8 (moins de 15)",
      lab_result_1 %in% c("Négatif", "Inconclusif") & (!is.na(lab_np_1)) ~ "CT1 present mais resultat neg/inc",
      lab_result_2 %in% c("Négatif", "Inconclusif") & (!is.na(lab_np_2)) ~ "CT2 present mais resultat neg/inc",
      lab_result_3 %in% c("Négatif", "Inconclusif") & (!is.na(lab_np_3)) ~ "CT3 present mais resultat neg/inc",
      lab_result_4 %in% c("Négatif", "Inconclusif") & (!is.na(lab_np_4)) ~ "CT4 present mais resultat neg/inc",
      lab_result_5 %in% c("Négatif", "Inconclusif") & (!is.na(lab_np_5)) ~ "CT5 present mais resultat neg/inc"
      # lab_result_6 %in% c("Négatif", "Inconclusif") & (!is.na(lab_np_6)) ~ "CT6 present mais resultat neg/inc",
      # lab_result_7 %in% c("Négatif", "Inconclusif") & (!is.na(lab_np_7)) ~ "CT7 present mais resultat neg/inc",
      # lab_result_8 %in% c("Négatif", "Inconclusif") & (!is.na(lab_np_8)) ~ "CT8 present mais resultat neg/inc",
    ))
    

# err14: missing symptoms - NEED TO CHECK THIS

    # # missing symptoms
    # err14 = case_when(
    #   is.na(nausea) ~ "Symptomes manquantes"
    # ),

# err15: suspected case with late results

dat <- dat %>%
  mutate(err15 = case_when(exit_no_result == "Oui" ~ NA,
                           evd_status == "Suspect" & time_since_adm > 1 & is.na(lab_result_1) ~ "Chercher resultat labo 1",
                           patient_site_id %in% c("MGL1071", "MGL1073") ~ NA,
                           evd_status == "Suspect" & time_since_adm > 2 & is.na(lab_result_2) ~ "Chercher resultat labo 2"))


# err16: need to check how this is coded?

    # err16: missing id but has patient info
    # err16 = case_when(
    #   is.na(patient_site_id) & !is.na(patient_site_id) ~ "Missing patient ID"))


# err17: lab sample dates

dat <- dat %>%
  mutate(
    err17a = case_when(
      patient_site_id %in% c("MGL1065", "MGL1066") ~ NA,
      is.na(lab_result_1) ~ NA,
      is.na(date_lab_sample_1) ~ "Date prélèvement 1 manquante",
      date_lab_sample_1 < date_admission_eff ~ "Date prélèvement 1 avant admission"),
    
    err17b = case_when(
      patient_site_id %in% c("MGL1067") ~ NA,
      is.na(lab_result_2) ~ NA,
      is.na(date_lab_sample_2) ~ "Date prélèvement 2 manquante",
      date_lab_sample_2 < date_admission_eff ~ "Date prélèvement 2 avant admission",
      date_lab_sample_2 < date_lab_sample_1 & !is.na(date_lab_sample_1) ~ "Date prélèvement 2 avant prélèvement 1",
      date_lab_sample_2 < date_lab_sample_1+2 & !is.na(date_lab_sample_1) ~ "Date prélèvement 2 trop tot après prélèvement 1"),
    
    err17c = case_when(
      is.na(lab_result_3) ~ NA,
      is.na(date_lab_sample_3) ~ "Date prélèvement 3 manquante",
      date_lab_sample_3 < date_admission_eff ~ "Date prélèvement 3 avant admission"),
    
    err17d = case_when(
      is.na(lab_result_4) ~ NA,
      is.na(date_lab_sample_4) ~ "Date prélèvement 4 manquante",
      date_lab_sample_4 < date_admission_eff ~ "Date prélèvement 4 avant admission")
  )


# Epi errors - from case investigation forms
# coding separate as lots of missing data

dat <- dat %>%
  mutate(epi_err1 = case_when(is.na(contact_evd_case) ~ "Vérifier fiche d'investigation"),
         epi_err2 = case_when(is.na(vaccination_rvsv_yn) ~ "Vérifier statut de vaccination"))




# Review ------------------------------------------------------------------

##### Daily exits

dat %>%
  filter(date_admission_eff == cutoff | date_exit_eff == cutoff) %>%
  mutate(
    left = case_when(
      date_admission_eff == date_exit_eff & date_admission_eff == cutoff ~ "came and left today",
      date_exit_eff == cutoff ~ "left only",
      date_admission_eff == cutoff ~ "arrived only"
    )
  ) %>% 
  filter(!is.na(left)) %>%
  select(left, type_of_exit) %>% 
  tbl_summary(by = "left")


##### Recent errors

dat %>% 
  mutate(
    this_week = case_when(date_admission_eff > (today() - 7) ~ 0,
                          
                          date_admission_eff > (today() - 30) ~ 1,
                          TRUE ~ 2)
  ) %>% 
  select(starts_with("err"), this_week) %>% 
  # need to remove empty otherwise tbl summary throws error
  remove_empty() %>% 
  tbl_summary(
    by = this_week,
    missing = "no")




# Format and export -------------------------------------------------------

##### Collapse errors

qa_export <- dat %>%
  filter(if_any(starts_with("err"), ~ !is.na(.) & . != "")) %>%
  rowwise() %>%
  mutate(errors = paste(na.omit(c_across(starts_with("err"))), collapse = "; ")) %>%
  ungroup() %>%
  select(patient_site_id, date_admission_eff, errors) %>%
  mutate(reponse = "")


##### Filter by cutoff date

qa_export_cut <- qa_export %>%
  filter(date_admission_eff >= err_cutoff) %>%
  # filter(date_admission_eff >= today()-30) %>%
  mutate(reponse = "")


##### Export

write.xlsx(qa_export_cut,
           here::here("output", "data quality",
                      paste0(format(cutoff, "%Y%m%d"), "_ll_qa.xlsx")),
           asTable = TRUE,
           overwrite = TRUE)



# Manual exports ----------------------------------------------------------

##### Lab-related errors

qa_export_lab <- dat %>%
  filter(!is.na(err5) | !is.na(err10) | !is.na(err15)) %>%
  rowwise() %>%
  mutate(errors = paste(na.omit(c_across(starts_with("err"))), collapse = "; ")) %>%
  ungroup() %>%
  select(patient_site_id, date_admission_eff, errors) %>%
  mutate(reponse = "") %>%
  filter(date_admission_eff >= as.Date("2026-09-01")) %>%
  arrange(desc(patient_site_id))


# write.xlsx(qa_export_lab,
#            here::here("output", "data quality",
#                       paste0(format(cutoff, "%Y%m%d"), "_ll_qa_lab.xlsx")),
#            asTable = TRUE,
#            overwrite = TRUE)




##### Case investigation form

qa_export_epi <-  dat %>%
  filter(if_any(starts_with("epi_err"), ~ !is.na(.) & . != "")) %>%
  # filter(patient_site_id == "MGL1062" | !is.na(err1) | !is.na(err2) | 
  #          err17a == "Date prélèvement 1 manquante") %>%
  rowwise() %>%
  mutate(errors = paste(na.omit(c_across(starts_with("epi_err"))), collapse = "; ")) %>%
  ungroup() %>%
  select(patient_site_id, date_admission_eff, errors) %>%
  mutate(reponse = "") %>%
  filter(date_admission_eff >= err_cutoff)


# write.xlsx(qa_export_epi,
#            here::here("output", "data quality",
#                       paste0(format(cutoff, "%Y%m%d"), "_ll_qa_investigation.xlsx")),
#            asTable = TRUE,
#            overwrite = TRUE)



##### Older errors - to manually check

# dat %>%
#   filter(!is.na(err10)) %>%
#   View()


qa_export_ext <-  dat %>%
  filter(if_any(starts_with("err"), ~ !is.na(.) & . != "")) %>%
  filter(!is.na(err1) | !is.na(err2) | 
           err17a == "Date prélèvement 1 manquante") %>%
  select(-err6e) %>%
  rowwise() %>%
  mutate(errors = paste(na.omit(c_across(starts_with("err"))), collapse = "; ")) %>%
  ungroup() %>%
  select(patient_site_id, date_admission_eff, errors) %>%
  mutate(reponse = "")

# write.xlsx(qa_export_ext,
#            here::here("output", "data quality",
#                       paste0(format(cutoff, "%Y%m%d"), "_ll_qa_ancien cas.xlsx")),
#            asTable = TRUE,
#            overwrite = TRUE)


##### Missing CT values and DHIS2 IDs

qa_export_ct <-  dat %>%
  filter(if_any(starts_with("err"), ~ !is.na(.) & . != "")) %>%
  filter((lab_result_1 == "Positif" & is.na(lab_np_1)) | (lab_result_2 == "Positif" & is.na(lab_np_2))) %>%
  # select(patient_site_id, date_admission_eff, lab_result_1, lab_np_1, lab_result_2, lab_np_2) %>%
  select(patient_site_id, date_admission_eff) %>%
  mutate(errors = "Valeur CT manquante", reponse = "")


# write.xlsx(qa_export_ct,
#            here::here("output", "data quality",
#                       paste0(format(cutoff, "%Y%m%d"), "_ll_qa_CT.xlsx")),
#            asTable = TRUE,
#            overwrite = TRUE)

