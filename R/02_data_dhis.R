################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   2b. DHIS2 line list (health zone surveillance): import and cleaning
#
#   Input : <data_dir>/dhis2/*.xlsx (latest file; first row is a title row)
#   Output: `dhis`, `dhis_conf`
#
#   Author: Stephanie Ngai (original), refactored by Aboubacar Hema
#   Last updated: 9 November 2026
#
################################################################################


# Import ------------------------------------------------------------------

filepath_dhis2 <- latest_file(path_data("dhis2"), cfg$dhis2_pattern)
message("DHIS2 line list: ", basename(filepath_dhis2))

dhis_raw <- read_sheet(filepath_dhis2, skip = 1)

dhis <- dhis_raw |>
  remove_empty(which = c("rows", "cols"))


# Data cleaning -----------------------------------------------------------

##### Dates

DATEVARS_DHIS <- grep("date", names(dhis), value = TRUE, ignore.case = TRUE)

dhis <- dhis |>
  mutate(
    across(all_of(DATEVARS_DHIS), as.Date),
    week_report = floor_date(date_de_rapportage, "week", week_start = 1),
    week_onset  = floor_date(mve_dds_date_de_debut_des_symptomes, "week", week_start = 1),
    # Proxy onset date: onset if known, else death - 10 days, else report date
    pdate = case_when(
      !is.na(mve_dds_date_de_debut_des_symptomes) ~ mve_dds_date_de_debut_des_symptomes,
      !is.na(x208_mve_s6_date_de_deces) ~ x208_mve_s6_date_de_deces - 10,
      TRUE ~ date_de_rapportage
    ),
    pdate_week = floor_date(pdate, "week", week_start = 1),
    # Date of first positive lab result, else report date
    report_date = case_when(
      mve_lab_radi_one_ebola_resultat_resultat_laboratoire == "Positif" &
        !is.na(mve_lab_date_danalyse_resultat_laboratoire) ~
        mve_lab_date_danalyse_resultat_laboratoire,
      mve_lab_radi_one_ebola_resultat_resultat_laboratoire5 == "Positif" &
        !is.na(mve_lab_date_danalyse_resultat_laboratoire4) ~
        mve_lab_date_danalyse_resultat_laboratoire4,
      TRUE ~ date_de_rapportage
    )
  )


##### Health areas (AS)

dhis <- dhis |>
  mutate(
    organisation_unit_name = str_replace(organisation_unit_name, "it ", ""),
    organisation_unit_name = if_else(str_detect(organisation_unit_name, "NDCM|CECA"),
                                     organisation_unit_name,
                                     str_to_title(organisation_unit_name)),
    orgunit = str_remove(organisation_unit_name, regex(" Aire De Santé", ignore_case = TRUE))
  )


# Filter ------------------------------------------------------------------
#  If the DHIS2 export contains all case classifications, keep confirmed only:
#  dhis_conf <- filter(dhis, mve_classification_finale_du_cas == "Cas confirmé")

dhis_conf <- dhis
