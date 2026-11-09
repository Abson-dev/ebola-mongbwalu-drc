################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   6. Weekly Monday update (two-week comparison table)
#
#   Standalone script. Exports:
#       <output_dir>/weekly summary/YYYYMMDD_weekly_summary.xlsx
#
#   Content requested by the coordination:
#     - Admissions, previous vs. last week: confirmed, <5 years
#     - Deaths: confirmed and suspected
#     - Exits by type (non-cases, cured, dropouts, ...)
#
#   Author: Stephanie Ngai (original), refactored by Aboubacar Hema
#   Last updated: 9 November 2026
#
################################################################################


# Setup -------------------------------------------------------------------
#  report_date defaults (in 01_setup.R) to the last Sunday, i.e. the end of
#  the latest complete epidemiological week. Set it before sourcing to override.

source(here::here("R", "01_setup.R"), encoding = "UTF-8")
source(here::here("R", "02_data_linelist.R"), encoding = "UTF-8")


# Data --------------------------------------------------------------------

cte <- dat |>
  filter(date_admission_eff <= report_date) |>
  filter(dead_upon_arrival != "Oui" | is.na(dead_upon_arrival)) |>
  mutate(age_group2 = case_when(age_years < 5 ~ "<5",
                                age_years >= 5 ~ "5+",
                                TRUE ~ "Inconnu"),
         age_group2 = factor(age_group2, levels = c("<5", "5+", "Inconnu")))

two_weeks <- c(week2, week1)


# Admissions --------------------------------------------------------------

t_adm_age <- cte |>
  filter(epiweek_adm %in% two_weeks) |>
  tabyl(age_group2, epiweek_adm) |>
  adorn_totals(where = "row") |>
  rename(name = age_group2)

t_adm_status <- cte |>
  filter(epiweek_adm %in% two_weeks) |>
  tabyl(evd_status, epiweek_adm) |>
  adorn_totals(where = "row", name = "Admissions") |>
  rename(name = evd_status)

t_adm <- t_adm_status |>
  filter(name %in% c("Confirmé", "Admissions")) |>
  arrange(desc(name)) |>
  bind_rows(filter(t_adm_age, name == "<5"))


# Exits and deaths --------------------------------------------------------

exits_2w <- cte |>
  filter(date_exit_eff <= report_date, epiweek_exit %in% two_weeks)

t_deaths <- exits_2w |>
  filter(type_of_exit == "Décédé") |>
  mutate(evd_status = fct_drop(evd_status)) |>
  tabyl(evd_status, epiweek_exit) |>
  adorn_totals(where = "row", name = "Décès") |>
  rename(name = evd_status) |>
  filter(name != "Non cas") |>
  arrange(desc(name))

t_exits <- exits_2w |>
  tabyl(type_of_exit, epiweek_exit) |>
  adorn_totals(where = "row", name = "Sorties") |>
  rename(name = type_of_exit)

t_exits_all <- t_exits |>
  filter(name == "Sorties") |>
  bind_rows(t_deaths) |>
  bind_rows(filter(t_exits, !name %in% c("Décédé", "Sorties")))


# Export ------------------------------------------------------------------

t_exp <- bind_rows(t_adm, t_exits_all) |>
  select(name, any_of(two_weeks)) |>
  mutate(across(any_of(two_weeks), ~ replace_na(.x, 0)))

write_xlsx_table(t_exp, path_output("weekly summary",
                                    paste0(format(report_date, "%Y%m%d"), "_weekly_summary.xlsx")))
