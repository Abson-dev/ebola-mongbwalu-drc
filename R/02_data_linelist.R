################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   2a. Linelist - data import and cleaning
#
#   Author: Stephanie Ngai
#   Last updated: 17 September 2026
#
################################################



# Filepath ----------------------------------------------------------------

filepath <- file.info(list.files(here::here("data", "linelist"), full.names = TRUE, pattern = "*.xlsx$")) %>%
  rownames_to_column() %>%
  filter(grepl("linelist", rowname)) %>%
  filter(mtime == max(mtime)) %>%
  pull(rowname)


# Import ------------------------------------------------------------------

dat_raw <- rio::import(filepath, sheet = "linelist") %>%
  janitor::clean_names()

# dat <- dat_raw %>%
#   remove_empty()

dat <- dat_raw


# Bed occupancy -----------------------------------------------------------

bor <- rio::import(filepath, sheet = "occupancy") %>%
  janitor::clean_names() %>%
  rename(dispo_c = x9) %>%
  remove_empty()


# Filter by report date

bor <- bor %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= report_date) %>%
  mutate(week = lubridate::floor_date(date, "week", 1),
         epiweek = date2week(date, factor = TRUE)) %>%
  mutate(epiweek = str_replace(epiweek, "W", "S"))


# Pivot

bor_long1 <- bor %>%
  pivot_longer(cols = c(occupied_s, occupied_c, occupied_total),
               names_to = "name", values_to = "occupied") %>%
  mutate(name = case_when(name == "occupied_s" ~ "Suspect", 
                          name == "occupied_c" ~ "Confirmé",
                          name == "occupied_total" ~ "Total"))

bor_long2 <- bor %>%
  pivot_longer(cols = c(occupation_sp, occupation_c, occupation_total),
               names_to = "name", values_to = "bor") %>%
  mutate(name = case_when(name == "occupation_sp" ~ "Suspect", 
                          name == "occupation_c" ~ "Confirmé",
                          name == "occupation_total" ~ "Total"))

bor_long3 <- bor %>%
  pivot_longer(cols = c(dispo_sp, dispo_c, dispo_total),
               names_to = "name", values_to = "dispo") %>%
  mutate(name = case_when(name == "dispo_sp" ~ "Suspect", 
                          name == "dispo_c" ~ "Confirmé",
                          name == "dispo_total" ~ "Total"))

bor_long <- bor_long1 %>%
  select(date, name, occupied) %>%
  left_join(bor_long3 %>%
              select(date, name, dispo),
            by = c("date", "name")) %>%
  left_join(bor_long2 %>%
              select(date, name, bor),
            by = c("date", "name")) %>%
  mutate(epiweek = date2week(date, factor = TRUE),
         epiweek = str_replace(epiweek, "W", "S"))




# Data cleaning -----------------------------------------------------------

##### Remove placeholder rows

dat <- dat %>%
  filter(!(is.na(age) & is.na(sex)))


##### Dates

DATEVARS <- names(dat)[grep("date", names(dat), ignore.case = TRUE)]


############################## TO CHECK: if report or effective dates should be used

dat <- dat %>%
  mutate(
    across(all_of(DATEVARS), as.Date),
    
    ## Epi week variables
    week_adm = lubridate::floor_date(date_admission_rep, "week", 1),
    week_exit = lubridate::floor_date(date_exit_rep, "week", 1),
    week_sx = lubridate::floor_date(date_symptom_onset, "week", 1),
    
    
    # Numeric value only
    # epiweek_adm = date2week(date_admission_rep, numeric = T),
    # epiweek_exit = date2week(date_exit_rep, numeric = T),
    # epiweek_sx = date2week(date_symptom_onset, numeric = T),
    
    # Factors
    epiweek_sx = date2week(date_symptom_onset, factor = T),
    
    # Report
    # epiweek_adm = date2week(date_admission_rep, factor = T),
    # epiweek_exit = date2week(date_exit_rep, factor = T),
    
    # Effective
    epiweek_adm = date2week(date_admission_eff, factor = T),
    epiweek_exit = date2week(date_exit_eff, factor = T),

    ## Deaths
    date_death = case_when(type_of_exit == "Décédé" ~ date_exit_rep)
    
  )

# Replace W with S in epi week variables
dat <- dat %>%
  mutate(across(c(epiweek_sx, epiweek_adm, epiweek_exit), str_replace, "W", "S"))


##### Sex

dat <- dat %>%
  mutate(sex_f = case_when(sex == "M" ~ "Masculin",
                           sex == "F" ~ "Féminin"))


##### Age

dat <- dat %>%
  mutate(age_years = case_when(age_unit == "Ans" ~ age,
                               age_unit == "Mois" ~ age/12,
                               age_unit == "Jour" ~ age/365),
         age_years = case_when(age_years > 100 ~ NA,
                               TRUE ~ age_years),
         age_group = bin_numbers(age_years, breaks = c(0, 5, 15, 45)))


##### Case status

# Set as factor
dat <- dat %>% 
  mutate(evd_status = factor(evd_status, levels = c("Confirmé", "Probable", "Suspect", "Non cas")))

# Probable previously combined with confirmed, do not actually want this

# dat <- dat %>%
#   mutate(evd_status2 = case_when(
#     evd_status == "Confirmé" ~ "Confirmé + Probable",
#     evd_status == "Probable" & date_admission_eff < as.Date("2026-07-01") ~ "Confirmé + Probable",
#     evd_status == "Probable" & date_admission_eff >= as.Date("2026-07-01") ~ "Suspect",
#     TRUE ~ evd_status
#     ),
#   evd_status2 = factor(evd_status2, levels = c(
#     "Confirmé + Probable", "Suspect", "Non cas"
#   )))


##### Exits

# Flag for exits

dat <- dat %>%
  mutate(exit = case_when(
    !is.na(type_of_exit) ~ 1,
    !is.na(date_exit_eff) ~ 1,
    !is.na(date_exit_rep) ~ 1,
    date_admission_eff < as.Date("2026-07-01") ~ 1,
    TRUE ~ 0
  ))


# Simplified exit type

dat <- dat %>%
  mutate(exit_type_simp = case_when(
    grepl("non cas", type_of_exit) ~ "Sortie non-cas",
    !is.na(type_of_exit) ~ type_of_exit,
    exit == 1 ~ "Inconnu"
  ))


# Unknown exit type

dat <- dat %>%
  mutate(type_of_exit2 = case_when(
    !is.na(type_of_exit) ~ type_of_exit,
    exit == 1 ~ "Inconnu"
  ))


# collapse retour à la maison and rephrase

dat <- dat %>%
  mutate(type_of_exit3 = case_when(
    grepl("maison", type_of_exit) ~ "Retour à maison non-cas",
    # grepl("Trans", type_of_exit) ~ "Transféré (non cas)",
    !is.na(type_of_exit) ~ type_of_exit,
    exit == 1 ~ "Inconnu"
  ))


##### Classify by reporting period

dat <- dat %>%
  mutate(
    rep_1w_adm = case_when(
      date_admission_eff >= date_start_1w & date_admission_eff <= report_date ~ 1,
      TRUE ~ 0
      ),
    
    rep_1w_exit = case_when(
      exit == 1 & date_exit_eff >= date_start_1w & date_exit_eff <= report_date ~ 1,
      TRUE ~ 0
    ),
    
    rep_3w_adm = case_when(
      date_admission_eff >= date_start_3w & date_admission_eff <= report_date ~ 1,
      TRUE ~ 0
    ),
    
    rep_3w_exit = case_when(
      exit == 1 & date_exit_eff >= date_start_3w & date_exit_eff <= report_date ~ 1,
      TRUE ~ 0
    ),
    
    rep_exit = case_when(
      date_admission_eff < as.Date("2026-07-01") ~ 1,
      exit == 1 & date_exit_eff <= report_date ~ 1,
      TRUE ~ 0
    )
  )


# Flag all as admission

dat <- dat %>%
  mutate(admission = 1)


##### Timing / LOS

# LOS
dat <- dat %>%
  mutate(los = as.numeric(difftime(date_exit_eff, date_admission_eff, units = "days")))

# Timing
dat <- dat %>%
  mutate(days_onset_exit = as.numeric(date_exit_eff - date_symptom_onset),
         # days_onset_notif = as.numeric(date_notif - date_ds),
         days_onset_adm = as.numeric(date_admission_eff - date_symptom_onset)) %>%
  mutate(across(c(days_onset_exit, days_onset_adm, los),
                ~ifelse(. < 0, NA, .)))





    