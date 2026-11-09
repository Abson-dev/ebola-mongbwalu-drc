################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   2c. Daily surveillance summary (alerts) and population
#
#   Input : <data_dir>/daily/Daily data entry.xlsx      (sheet Alerts_Det_Data)
#           <data_dir>/population/Population_AS2026_Ituri_dhis2.xlsx
#   Output: `alerts`, `alerts_week`, `alerts_week_as`, `alerts_week_as_rate`
#
#   Author: Stephanie Ngai (original), refactored by Aboubacar Hema
#   Last updated: 9 November 2026
#
################################################################################


# Import ------------------------------------------------------------------

alerts <- read_sheet(path_data("daily", cfg$daily_file), sheet = "Alerts_Det_Data") |>
  remove_empty(which = c("rows", "cols"))

pop <- read_sheet(path_data("population", cfg$population_file))


# Data cleaning -----------------------------------------------------------

##### Population of the health areas (AS) of the health zone

pop_mgl <- pop |>
  filter(zone_de_sante == cfg$health_zone)


##### Alerts

alerts <- alerts |>
  mutate(date = as.Date(date),
         cas_suspects_non_transferes = as.numeric(cas_suspects_non_transferes),
         epiweek_alert = epiweek_label(date),
         week = floor_date(date, "week", week_start = 1),
         as = if_else(as == "Ceca 20", "CECA 20", as)) |>
  filter(date <= report_date) |>
  filter(!as %in% c("Hors zone", "ZS Kilo"))

# Alive / dead / total per row
alerts_counts <- alerts |>
  mutate(Vivant  = rowSums(cbind(reports_vivants, alertes_vivants), na.rm = TRUE),
         `Décédé` = rowSums(cbind(report_deces, alertes_deces), na.rm = TRUE),
         Total   = Vivant + `Décédé`)

# By week
alerts_week <- alerts_counts |>
  group_by(epiweek_alert) |>
  summarise(across(c(Vivant, `Décédé`, Total), sum), .groups = "drop") |>
  pivot_longer(cols = -epiweek_alert)

# By week and health area
alerts_week_as <- alerts_counts |>
  group_by(week, as) |>
  summarise(across(c(Vivant, `Décédé`, Total), sum), .groups = "drop") |>
  pivot_longer(cols = -c(week, as))

# Alert rate per 10 000 population, by health area and week
alerts_week_as_rate <- alerts_counts |>
  group_by(week, as) |>
  summarise(alertes = sum(Total), .groups = "drop") |>
  left_join(select(pop_mgl, aire_de_sante, population_de_l_aire_de_sante),
            by = c("as" = "aire_de_sante")) |>
  mutate(rate = alertes / population_de_l_aire_de_sante * 10000)

rm(alerts_counts)
