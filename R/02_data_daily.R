################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   2c. Daily summary data
#
#   Author: Stephanie Ngai
#   Last updated: 21 September 2026
#
################################################


# Filepath ----------------------------------------------------------------

# filepath_alerts <- "C:/Users/CD524eEpidemio/OneDrive - MSF/Documents/2.CD524_EBOLA_MONGBWALU/SITREPS_MVEB_MONGBWALU/MSF_SITREPS/Daily data entry.xlsx"
# 
# filepath_pop <- "C:/Users/CD524eEpidemio/OneDrive - MSF/Documents/2.CD524_EBOLA_MONGBWALU/Data/Geodata/Population_AS2026_Ituri_dhis2.xlsx"


# Import ------------------------------------------------------------------

# alerts <- rio::import(filepath_alerts,
#                       sheet = "Alerts_Det_Data") %>%
#   janitor::clean_names() %>%
#   remove_empty()
# 
# pop <- rio::import(filepath_pop) %>%
#   janitor::clean_names()

alerts <- rio::import(here::here("data", "Daily data entry.xlsx"),
                      sheet = "Alerts_Det_Data") %>%
  janitor::clean_names() %>%
  remove_empty()

pop <- rio::import(here::here("data", "Population_AS2026_Ituri_dhis2.xlsx")) %>%
  janitor::clean_names()


# Data cleaning -----------------------------------------------------------

##### Population data

pop_mgl <- pop %>%
  filter(zone_de_sante == "Mongbwalu")


##### Alerts data

alerts <- alerts %>%
  mutate(date = as.Date(date),
         cas_suspects_non_transferes = as.numeric(cas_suspects_non_transferes),
         epiweek_alert = date2week(date, factor = TRUE),
         epiweek_alert = str_replace(epiweek_alert, "W", "S"),
         week = lubridate::floor_date(date, "week", 1)) %>%
  filter(date <= report_date) %>%
  filter(!as %in% c("Hors zone", "ZS Kilo"))

# Summarize by week
alerts_week <-
  alerts %>%
  rowwise() %>%
  mutate("Vivant" = sum(reports_vivants, alertes_vivants, na.rm = TRUE),
         "Décédé" = sum(report_deces, alertes_deces, na.rm = TRUE),
         "Total" = sum(Vivant, `Décédé`, na.rm = TRUE)) %>%
  group_by(epiweek_alert) %>%
  summarise(across(c(Vivant, `Décédé`, Total), sum)) %>%
  pivot_longer(cols = -epiweek_alert)

# By week and AS
alerts_week_as <-
  alerts %>%
  mutate(as = case_when(as == "Ceca 20" ~ "CECA 20",
                        TRUE ~ as)) %>%
  rowwise() %>%
  mutate("Vivant" = sum(reports_vivants, alertes_vivants, na.rm = TRUE),
         "Décédé" = sum(report_deces, alertes_deces, na.rm = TRUE),
         "Total" = sum(Vivant, `Décédé`, na.rm = TRUE)) %>%
  group_by(week, as) %>%
  summarise(across(c(Vivant, `Décédé`, Total), sum)) %>%
  pivot_longer(cols = -c(week, as))


# Alert rate by AS/week
alerts_week_as_rate <-
  alerts %>%
  mutate(as = case_when(as == "Ceca 20" ~ "CECA 20",
                        TRUE ~ as)) %>%
  rowwise() %>%
  mutate("Vivant" = sum(reports_vivants, alertes_vivants, na.rm = TRUE),
         "Décédé" = sum(report_deces, alertes_deces, na.rm = TRUE),
         "Total" = sum(Vivant, `Décédé`, na.rm = TRUE)) %>%
  group_by(week, as) %>%
  summarise(alertes = sum(Total)) %>%
  left_join(select(pop_mgl, aire_de_sante, population_de_l_aire_de_sante),
            by = c("as" = "aire_de_sante")) %>%
  mutate(rate = (alertes/population_de_l_aire_de_sante) * 10000)


