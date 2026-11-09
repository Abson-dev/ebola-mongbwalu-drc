################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   6. Weekly Monday update
#
#   Author: Stephanie Ngai
#   Last updated: 9 October 2026
#
################################################


##### Set cutoff date

date_report <- if(lubridate::wday(Sys.Date()) == 1) {
  Sys.Date()
} else {
  lubridate::floor_date(Sys.Date(), "weeks", 1)-1
}


##### Source files

source(here::here("R", "01_setup.R"))
source(here::here("R", "02_data_linelist.R"))


##### Data

cte <- dat %>%
  filter(date_admission_eff <= report_date) %>%
  filter(dead_upon_arrival != "Oui" | is.na(dead_upon_arrival)) %>%
  mutate(epiweek_exit = as.character(epiweek_exit),
         epiweek_adm = as.character(epiweek_adm)) %>%
  mutate(age_group2 = case_when(age_years < 5 ~ "<5",
                                age_years %in% c(5:100) ~ "5+", 
                                TRUE ~ "Inconnu"),
         age_group2 = factor(age_group2, levels = c("<5", "5+", "Inconnu")))


n_doa <- dat %>%
  filter(date_admission_eff <= report_date) %>%
  filter(dead_upon_arrival == "Oui") %>%
  nrow()


## Last week, this week
# Admissions
# Confirmed
# <5

# Deaths
#   Conf/susp
# Guéri
# Other exits




##### Summary figures

# Pour Mongwalu : 
#   Admissions :  comparaison de 2 semaines, la semaine précédente et écoulée cas confirmés et cas suspects : <5ans et >5ans
# Décès : pareil cas confirmés et suspect pour les 2 semaines : <5ans et >5ans
# Sorties : Non cas, sorties guéries 2 semaines consécutives : 
#   Abandon : pareil
# Provenance des cas confirmés se la semaine
# Nombres des patients sur les lits cas confirmés et suspect. 



##### Admissions
# Admissions: comparison of the previous week vs. the past week (confirmed and suspected cases: <5 years and >5 years)

# t_adm <-
#   cte %>%
#   group_by(epiweek_adm, evd_status, age_group2) %>%
#   count() %>% 
#   pivot_wider(names_from = epiweek_adm, values_from = n, values_fill = 0) %>%
#   arrange(evd_status, age_group2) %>%
#   rowwise() %>%
#   mutate(Cumul = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>% ungroup() %>%
#   select(evd_status, age_group2, any_of(c(week2, week1)), Cumul) %>%
#   flextable() %>%
#   set_header_labels(evd_status = "Classification", age_group2 = "Tranche d'age") %>%
#   merge_v(j = 1, part = "body") %>%
#   border_inner_h() %>%
#   add_header_row(top = TRUE, values = "Admissions", colwidths = 5) %>%
#   bold(i = c(1,2), part = "header")


t_adm_age <- 
  cte %>%
  filter(epiweek_adm %in% c(week1, week2)) %>%
  tabyl(age_group2, epiweek_adm) %>%
  adorn_totals(where = "row") %>%
  rename(name = age_group2)

t_adm_status <- 
  cte %>%
  filter(epiweek_adm %in% c(week1, week2)) %>%
  tabyl(evd_status, epiweek_adm) %>%
  adorn_totals(where = "row", name = "Admissions") %>%
  rename(name = evd_status)

t_adm <- t_adm_status %>%
  filter(name %in% c("Confirmé", "Admissions")) %>%
  arrange(desc(name)) %>%
  bind_rows(t_adm_age %>%
              filter(name == "<5"))



##### Exits

## Deaths
# Deaths: same breakdown (confirmed and suspected cases for both weeks: <5 years and >5 years)

# t_deaths <- 
#   cte %>%
#   filter(date_exit_eff <= report_date) %>%
#   filter(type_of_exit == "Décédé") %>%
#   group_by(epiweek_exit, evd_status, age_group2) %>%
#   count() %>% 
#   pivot_wider(names_from = epiweek_exit, values_from = n, values_fill = 0) %>%
#   arrange(evd_status, age_group2) %>%
#   rowwise() %>%
#   mutate(Cumul = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>% ungroup() %>%
#   select(evd_status, age_group2, any_of(c(week2, week1)), Cumul) %>%
#   flextable() %>%
#   set_header_labels(evd_status = "Classification", age_group2 = "Tranche d'age") %>%
#   merge_v(j = 1, part = "body") %>%
#   border_inner_h() %>%
#   add_header_row(top = TRUE, values = "Décès", colwidths = 5) %>%
#   bold(i = c(1,2), part = "header") 



# cte %>%
#   filter(date_exit_eff <= report_date,
#          type_of_exit == "Décédé",
#          epiweek_exit %in% c(week1, week2)) %>%
#   tabyl(age_group2, epiweek_exit) %>%
#   adorn_totals(where = "row")

t_exits_all <-
  cte %>%
  filter(date_exit_eff <= report_date,
         epiweek_exit %in% c(week1, week2)) %>%
  tabyl(type_of_exit, epiweek_exit) %>%
  adorn_totals(where = "row") %>%
  rename(name = type_of_exit)

t_deaths <-
  cte %>%
  filter(date_exit_eff <= report_date,
         type_of_exit == "Décédé",
         epiweek_exit %in% c(week1, week2)) %>%
  mutate(evd_status = fct_drop(evd_status)) %>%
  tabyl(evd_status, epiweek_exit) %>%
  adorn_totals(where = "row", name = "Décès") %>%
  rename(name = evd_status) %>%
  filter(name != "Non cas") %>%
  arrange(desc(name))

t_exits <- 
  cte %>%
  filter(date_exit_eff <= report_date,
         # type_of_exit != "Décédé",
         epiweek_exit %in% c(week1, week2)) %>%
  tabyl(type_of_exit, epiweek_exit) %>%
  adorn_totals(where = "row", name = "Sorties") %>%
  rename(name = type_of_exit)

t_exits_all <-
  t_exits %>%
  filter(name == "Sorties") %>%
  bind_rows(t_deaths) %>%
  bind_rows(t_exits %>%
              filter(!name %in% c("Décédé", "Sorties")))


## Other exits
# Discharges: non-cases, recovered patients (over 2 consecutive weeks)

# t_exits <- 
#   cte %>%
#   filter(date_exit_eff <= report_date) %>%
#   filter(type_of_exit != "Décédé") %>%
#   group_by(epiweek_exit, evd_status, type_of_exit3) %>%
#   count() %>% 
#   pivot_wider(names_from = epiweek_exit, values_from = n, values_fill = 0) %>%
#   arrange(evd_status, type_of_exit3) %>%
#   rowwise() %>%
#   mutate(Cumul = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>% ungroup() %>%
#   select(evd_status, type_of_exit3, any_of(c(week2, week1)), Cumul) %>%
#   flextable() %>%
#   set_header_labels(evd_status = "Classification", type_of_exit3 = "Type de sortie") %>%
#   merge_v(j = 1, part = "body") %>%
#   border_inner_h() %>%
#   add_header_row(top = TRUE, values = "Sorties", colwidths = 5) %>%
#   bold(i = c(1,2), part = "header")


# Dropouts: same breakdown

##### Patient origin
# Origin of confirmed cases for the week

# t_origin <- 
#   cte %>%
#   filter(epiweek_adm == week1) %>%
#   mutate(adm_print = case_when(adm2_name_res == "Mongbwalu" ~ adm3_name_res,
#                                TRUE ~ paste0("ZS ", adm2_name_res, " - ", adm3_name_res))) %>%
#   tabyl(adm_print, evd_status) %>%
#   adorn_totals(where = "col") %>%
#   adorn_percentages(denominator = "col") %>%
#   adorn_pct_formatting(digits = 0) %>%
#   adorn_ns(position = "front") %>%
#   mutate(adm_print = replace_na(adm_print, "Inconnu")) %>%
#   flextable() %>%
#   set_header_labels(adm_print = "Aire de Santé") %>%
#   bold(i = 1, part = "header")


# Number of patients currently occupying beds (confirmed and suspected cases)


##### Render Rmd

# rmarkdown::render(
#   input = here::here("R", "06_weekly_table.Rmd"),
#   # output_format = "docx",
#   output_dir = here::here("output", "weekly summary"),
#   output_file = paste0(format(Sys.Date(), "%Y%m%d"), "_point hebdo.docx"), 
#   envir = globalenv()
# )


##### Combine and export to Excel

t_exp <- t_adm %>%
  bind_rows(t_exits_all)

write.xlsx(t_exp,
           file = here::here("output", "weekly summary",
                             paste0(format(date_report, "%Y%m%d"), "_weekly summaryv2.xlsx")))









