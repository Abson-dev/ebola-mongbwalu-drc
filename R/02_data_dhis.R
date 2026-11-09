################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   2b. DHIS2 linelist - data ipmort and cleaning
#
#   Author: Stephanie Ngai
#   Last updated: 17 September 2026
#
################################################


# Filepath ----------------------------------------------------------------

filepath_dhis2 <- file.info(list.files(here::here("data", "dhis2"), full.names = TRUE, pattern = "*.xlsx$")) %>%
  rownames_to_column() %>%
  # filter(grepl("linelist", rowname)) %>%
  filter(mtime == max(mtime)) %>%
  pull(rowname)


# Import ------------------------------------------------------------------

dhis_raw <- rio::import(filepath_dhis2, skip = 1) %>%
  janitor::clean_names()

dhis <- dhis_raw %>%
  remove_empty()



# Data cleaning -----------------------------------------------------------

##### Dates

DATEVARS_DHIS <- names(dhis)[grep("date", names(dhis), ignore.case = TRUE)]

dhis <- dhis %>%
  mutate(
    across(all_of(DATEVARS_DHIS), as.Date),
    week_report = lubridate::floor_date(date_de_rapportage, unit = "weeks", 1),
    week_onset = lubridate::floor_date(mve_dds_date_de_debut_des_symptomes, "weeks", 1),
    pdate = case_when(
      !is.na(mve_dds_date_de_debut_des_symptomes) ~ mve_dds_date_de_debut_des_symptomes,
      # !is.na(s1_si_decede_date_du_deces) ~ s1_si_decede_date_du_deces - 10,
      !is.na(x208_mve_s6_date_de_deces) ~ x208_mve_s6_date_de_deces - 10,
      # mve_n_nature_de_l_alerte == "Décès" & !is.na(mpox_n_date_et_heure_de_notification_de_lalerte) ~
        # mpox_n_date_et_heure_de_notification_de_lalerte - 10,
      TRUE ~ date_de_rapportage
    ),
    pdate_week = lubridate::floor_date(pdate, "weeks", 1)
  ) %>%
  mutate(report_date = case_when(
    mve_lab_radi_one_ebola_resultat_resultat_laboratoire == "Positif" & !is.na(mve_lab_date_danalyse_resultat_laboratoire) ~
      mve_lab_date_danalyse_resultat_laboratoire,
    mve_lab_radi_one_ebola_resultat_resultat_laboratoire5 == "Positif" & !is.na(mve_lab_date_danalyse_resultat_laboratoire4) ~
      mve_lab_date_danalyse_resultat_laboratoire4,
    TRUE ~ date_de_rapportage
  ))


##### AS

dhis <- dhis %>%
  mutate(organisation_unit_name = str_replace(organisation_unit_name, "it ", ""),
         organisation_unit_name = case_when(
           str_detect(organisation_unit_name, "NDCM|CECA") ~ organisation_unit_name,
           TRUE ~ str_to_title(organisation_unit_name)
         )) %>%
  mutate(orgunit = str_replace(organisation_unit_name, " Aire De Santé", ""),
         orgunit = str_replace(orgunit, " Aire de Santé", ""))


# Filter ------------------------------------------------------------------

# dhis_conf <- dhis %>%
#   filter(mve_classification_finale_du_cas == "Cas confirmé")

dhis_conf <- dhis


