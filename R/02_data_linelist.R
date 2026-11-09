################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   2a. CTE line list: import and cleaning
#
#   Input : <data_dir>/linelist/*.xlsx (latest file), sheets "linelist" and
#           "occupancy" (MSF EVD line list template v4.4)
#   Output: `dat` (one row per admission), `bor`, `bor_long*` (bed occupancy)
#
#   Author: Stephanie Ngai (original), refactored by Aboubacar Hema
#   Last updated: 9 November 2026
#
################################################################################


# Import ------------------------------------------------------------------

filepath <- latest_file(path_data("linelist"), cfg$linelist_pattern)
message("Line list: ", basename(filepath))

dat_raw <- read_sheet(filepath, sheet = "linelist")
dat <- dat_raw


# Bed occupancy -----------------------------------------------------------

bor <- read_sheet(filepath, sheet = "occupancy") |>
  rename(dispo_c = x9) |>
  remove_empty(which = c("rows", "cols")) |>
  mutate(date = as.Date(date)) |>
  filter(!is.na(date), date <= report_date) |>
  mutate(week    = floor_date(date, "week", week_start = 1),
         epiweek = epiweek_label(date))

status_from_suffix <- function(x) {
  case_when(str_detect(x, "_s$|_sp$") ~ "Suspect",
            str_detect(x, "_c$")       ~ "Confirmé",
            str_detect(x, "_total$")   ~ "Total")
}

bor_long1 <- bor |>
  pivot_longer(c(occupied_s, occupied_c, occupied_total),
               names_to = "name", values_to = "occupied") |>
  mutate(name = status_from_suffix(name))

bor_long2 <- bor |>
  pivot_longer(c(occupation_sp, occupation_c, occupation_total),
               names_to = "name", values_to = "bor") |>
  mutate(name = status_from_suffix(name))

bor_long3 <- bor |>
  pivot_longer(c(dispo_sp, dispo_c, dispo_total),
               names_to = "name", values_to = "dispo") |>
  mutate(name = status_from_suffix(name))

bor_long <- bor_long1 |>
  select(date, name, occupied) |>
  left_join(select(bor_long3, date, name, dispo), by = c("date", "name")) |>
  left_join(select(bor_long2, date, name, bor), by = c("date", "name")) |>
  mutate(epiweek = epiweek_label(date))


# Data cleaning -----------------------------------------------------------

##### Remove placeholder rows (pre-filled IDs without patient data)

dat <- dat |>
  filter(!(is.na(age) & is.na(sex)))


##### Manual corrections (config/manual_corrections.csv)

dat <- apply_corrections(dat)


##### Dates
#  NB: effective (`_eff`) dates are used for admissions/exits; reported
#  (`_rep`) dates for week_adm/week_exit, as in the original pipeline.

DATEVARS <- grep("date", names(dat), value = TRUE, ignore.case = TRUE)

dat <- dat |>
  mutate(
    across(all_of(DATEVARS), as.Date),

    ## Week start (Monday)
    week_adm  = floor_date(date_admission_rep, "week", week_start = 1),
    week_exit = floor_date(date_exit_rep, "week", week_start = 1),
    week_sx   = floor_date(date_symptom_onset, "week", week_start = 1),

    ## Epi week labels ("2026-S44")
    epiweek_sx   = epiweek_label(date_symptom_onset),
    epiweek_adm  = epiweek_label(date_admission_eff),
    epiweek_exit = epiweek_label(date_exit_eff),

    ## Deaths
    date_death = case_when(type_of_exit == "Décédé" ~ date_exit_rep)
  )


##### Sex

dat <- dat |>
  mutate(sex_f = case_when(sex == "M" ~ "Masculin",
                           sex == "F" ~ "Féminin"))


##### Age

dat <- dat |>
  mutate(age = as.numeric(age),
         age_years = case_when(age_unit == "Ans"  ~ age,
                               age_unit == "Mois" ~ age / 12,
                               age_unit == "Jour" ~ age / 365),
         age_years = if_else(age_years > 100, NA_real_, age_years),
         age_group = bin_numbers(age_years, breaks = cfg$age_breaks_sitrep))


##### Case status

dat <- dat |>
  mutate(evd_status = factor(evd_status,
                             levels = c("Confirmé", "Probable", "Suspect", "Non cas")))


##### Exits

# Flag for exits (patients admitted before July 2026 are historic records)
dat <- dat |>
  mutate(exit = case_when(
    !is.na(type_of_exit)  ~ 1,
    !is.na(date_exit_eff) ~ 1,
    !is.na(date_exit_rep) ~ 1,
    date_admission_eff < cfg$historic_exit_date ~ 1,
    TRUE ~ 0
  ))

# Simplified exit types
dat <- dat |>
  mutate(
    exit_type_simp = case_when(
      grepl("non cas", type_of_exit) ~ "Sortie non-cas",
      !is.na(type_of_exit) ~ type_of_exit,
      exit == 1 ~ "Inconnu"
    ),
    type_of_exit2 = case_when(
      !is.na(type_of_exit) ~ type_of_exit,
      exit == 1 ~ "Inconnu"
    ),
    # Collapse the two "retour à la maison" categories
    type_of_exit3 = case_when(
      grepl("maison", type_of_exit) ~ "Retour à maison non-cas",
      !is.na(type_of_exit) ~ type_of_exit,
      exit == 1 ~ "Inconnu"
    )
  )


##### Reporting period flags

dat <- dat |>
  mutate(
    rep_1w_adm  = as.numeric(date_admission_eff >= date_start_1w & date_admission_eff <= report_date),
    rep_1w_exit = as.numeric(exit == 1 & date_exit_eff >= date_start_1w & date_exit_eff <= report_date),
    rep_3w_adm  = as.numeric(date_admission_eff >= date_start_3w & date_admission_eff <= report_date),
    rep_3w_exit = as.numeric(exit == 1 & date_exit_eff >= date_start_3w & date_exit_eff <= report_date),
    rep_exit = case_when(
      date_admission_eff < cfg$historic_exit_date ~ 1,
      exit == 1 & date_exit_eff <= report_date ~ 1,
      TRUE ~ 0
    ),
    across(starts_with("rep_"), ~ replace_na(.x, 0)),
    admission = 1
  )


##### Length of stay and delays (negative values are data errors -> NA)

dat <- dat |>
  mutate(
    los             = as.numeric(date_exit_eff - date_admission_eff),
    days_onset_exit = as.numeric(date_exit_eff - date_symptom_onset),
    days_onset_adm  = as.numeric(date_admission_eff - date_symptom_onset),
    across(c(los, days_onset_exit, days_onset_adm), ~ if_else(.x < 0, NA_real_, .x))
  )

rm(status_from_suffix)
