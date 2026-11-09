################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   4. CTE data summary
#
#   Author: Stephanie Ngai
#   Last updated: 21 September 2026
#
################################################

## Manual fix

dat <- dat %>%
  mutate(evd_status = case_when(patient_site_id == "MGL1309" ~ "Non cas",
                                TRUE ~ evd_status))


# Data --------------------------------------------------------------------

## Filter data to include only reporting period

cte <- dat %>%
  filter(date_admission_eff <= report_date) %>%
  filter(dead_upon_arrival != "Oui" | is.na(dead_upon_arrival)) %>%
  mutate(epiweek_exit = as.character(epiweek_exit),
         epiweek_adm = as.character(epiweek_adm))

# Exit type as factor
#  consider moving this to data script
cte <- cte %>%
  mutate(
    type_of_exit3 = factor(type_of_exit3,
      levels = c("Guéri", "Décédé", "Retour à maison non-cas", 
                 "Transféré au CTE", "Transféré autre structure (non cas)", "Abandon", "Inconnu"
                                )))


# Summary figures ---------------------------------------------------------

n_adm <- sum(cte$admission)
n_adm_thisweek <- sum(cte$epiweek_adm == week1)
n_adm_lastweek <- sum(cte$epiweek_adm == week2)

n_exits <- sum(cte$rep_exit)
n_exits_thisweek <- sum(cte$epiweek_exit == week1, na.rm = TRUE)
n_exits_lastweek <- sum(cte$epiweek_exit == week2, na.rm = TRUE)

n_conf <- sum(cte$evd_status == "Confirmé", na.rm = TRUE)
n_prob <- sum(cte$evd_status == "Probable", na.rm = TRUE)

n_conf_dcd <- sum(cte$evd_status == "Confirmé" & cte$rep_exit == 1 & 
                    cte$type_of_exit == "Décédé", na.rm = TRUE)
n_conf_gueri <- sum(cte$evd_status == "Confirmé" & cte$rep_exit == 1 & 
                      cte$type_of_exit == "Guéri", na.rm = TRUE)

n_conf_exits <- sum(cte$evd_status == "Confirmé" & cte$rep_exit, na.rm = TRUE)
n_conf_exits_thisweek <- sum(cte$evd_status == "Confirmé" & 
                               cte$epiweek_exit == week1, na.rm = TRUE)
n_conf_exits_lastweek <- sum(cte$evd_status == "Confirmé" &
                               cte$epiweek_exit == week2, na.rm = TRUE)

n_conf_dcd_thisweek <- sum(cte$evd_status == "Confirmé" & cte$epiweek_exit == week1 & 
                    cte$type_of_exit == "Décédé", na.rm = TRUE)
n_conf_gueri_thisweek <- sum(cte$evd_status == "Confirmé" & cte$epiweek_exit == week1 & 
                      cte$type_of_exit == "Guéri", na.rm = TRUE)

n_doa <- dat %>%
  filter(date_admission_eff <= report_date) %>%
  filter(dead_upon_arrival == "Oui") %>%
  nrow()

n_doa_conf <- dat %>%
  filter(date_admission_eff <= report_date,
         dead_upon_arrival == "Oui",
         evd_status == "Confirmé") %>%
  nrow()

  
# Table 1 -----------------------------------------------------------------

## Summary table of admissions and exits, last 3 weeks and cumulative
#   This is a bit clunky but gets to the table 1 as requested for the sitrep


##### Admissions

t1a <-
  cte %>%
  group_by(epiweek_adm) %>%
  summarise(
    n = sum(admission)
  ) %>%
  pivot_wider(names_from = epiweek_adm, values_from = n) %>%
  mutate(name = "Admissions") %>%
  select(name, any_of(week3_vector)) %>%
  bind_cols(
    cte %>% summarise(Cumul = sum(admission))
  ) %>%
  mutate(across(everything(), as.character))

t1a_conf <-
  cte %>%
  filter(evd_status == "Confirmé") %>%
  group_by(epiweek_adm) %>%
  summarise(
    n = sum(admission)
  ) %>%
  pivot_wider(names_from = epiweek_adm, values_from = n) %>%
  mutate(name = "Confirmé") %>%
  select(name, any_of(week3_vector)) %>%
  bind_cols(Cumul = n_conf) %>%
  mutate(across(everything(), as.character))

##### Exits

## Total
t1b <-
  cte %>%
  group_by(epiweek_exit) %>%
  summarise(
    n = sum(rep_exit)
  ) %>%
  pivot_wider(names_from = epiweek_exit, values_from = n) %>%
  mutate(name = "Sorties") %>%
  select(name, any_of(week3_vector)) %>%
  bind_cols(
    cte %>% summarise(Cumul = sum(rep_exit))
  ) %>%
  mutate(across(everything(), as.character))


## Non-cases
t1c <-
  cte %>%
  filter(evd_status == "Non cas" & rep_exit == 1) %>%
  tabyl(rep_exit, epiweek_exit) %>%
  adorn_totals(where = "col", name = "Cumul") %>%
  mutate(name = "Sortie non cas") %>%
  select(name, any_of(week3_vector), Cumul) %>%
  mutate(across(everything(), as.character))

t1d <- 
  cte %>%
  filter(evd_status == "Non cas" & rep_exit == 1) %>%
  tabyl(type_of_exit3, epiweek_exit,
        show_missing_levels = FALSE) %>% 
  adorn_totals(where = "col", name = "Cumul") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns(position = "front") %>%
  select(name = type_of_exit3, any_of(week3_vector), Cumul)


## Confirmed
t1e <-
  cte %>%
  filter(evd_status == "Confirmé" & rep_exit == 1) %>%
  tabyl(rep_exit, epiweek_exit) %>%
  adorn_totals(where = "col", name = "Cumul") %>%
  mutate(name = "Sortie confirmé") %>%
  select(name, any_of(week3_vector), Cumul) %>%
  mutate(across(everything(), as.character))

t1f <- 
  cte %>%
  filter(evd_status == "Confirmé" & rep_exit == 1) %>%
  tabyl(type_of_exit3, epiweek_exit,
        show_missing_levels = FALSE) %>% 
  adorn_totals(where = "col", name = "Cumul") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns(position = "front") %>%
  select(name = type_of_exit3, any_of(week3_vector), Cumul)


## Probable
t1g <-
  cte %>%
  filter(evd_status == "Probable" & rep_exit == 1) %>%
  tabyl(rep_exit, epiweek_exit) %>%
  adorn_totals(where = "col", name = "Cumul") %>%
  mutate(name = "Sortie probable") %>%
  select(name, any_of(week3_vector), Cumul) %>%
  mutate(across(everything(), as.character))

t1h <- 
  cte %>%
  filter(evd_status == "Probable" & rep_exit == 1) %>%
  tabyl(type_of_exit3, epiweek_exit,
        show_missing_levels = FALSE) %>% 
  adorn_totals(where = "col", name = "Cumul") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns(position = "front") %>%
  select(name = type_of_exit3, any_of(week3_vector), Cumul)


## Suspect
t1i <-
  cte %>%
  filter(evd_status == "Suspect" & rep_exit == 1) %>%
  tabyl(rep_exit, epiweek_exit) %>%
  adorn_totals(where = "col", name = "Cumul") %>%
  mutate(name = "Sortie suspect") %>%
  select(name, any_of(week3_vector), Cumul) %>%
  mutate(across(everything(), as.character))

t1j <- 
  cte %>%
  filter(evd_status == "Suspect" & rep_exit == 1) %>%
  tabyl(type_of_exit3, epiweek_exit,
        show_missing_levels = FALSE) %>% 
  adorn_totals(where = "col", name = "Cumul") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns(position = "front") %>%
  select(name = type_of_exit3, any_of(week3_vector), Cumul)


## Combine
t1 <- 
  bind_rows(t1a, t1a_conf, t1b, t1c, t1d, t1e, t1f, t1g, t1h, t1i, t1j) %>%
  flextable() %>% border_inner_h(border = border_dotted) %>%
  set_header_labels(name = "") %>%
  hline(i = c(2,3), part = "body") %>%
  bold(i = 1, part = "header") %>%
  bold(i = ~ name %in% 
         c("Admissions", "Sorties", "Sortie non cas", "Sortie confirmé",
           "Sortie probable", "Sortie suspect")) %>%
  bg(i = ~ name %in% 
       c("Admissions", "Sorties", "Sortie non cas", "Sortie confirmé",
         "Sortie probable", "Sortie suspect"),
     part = "body", bg = "grey90")

rm(t1a, t1a_conf, t1b, t1c, t1d, t1e, t1f, t1g, t1h, t1i, t1j)




# Admissions --------------------------------------------------------------

##### Epi curve of admissions

ec_adm <-
  vis_epicurve(cte %>%
                 mutate(evd_status = factor(evd_status, levels = c(
                   "Non cas", "Suspect", "Probable", "Confirmé"))),
               # mutate(evd_status = factor(evd_status, levels = c(
               #   "Non cas", "Suspect", "Probable", "Confirmé"))),
             date_measure = "date_admission_eff",
             date_min = min(cte$week_adm),
             date_max = date_cut+1,
             # date_max = report_date+1,
             fill_by = "evd_status",
             pal = c("Non cas" = "#a4cd98", "Suspect" = "#d27760",
                     "Probable" = "#d57689", "Confirmé" = "#b0364e")) +
  labs(x = "Semaine d'admission", y = "N. patients") +
  theme(legend.position = "top", legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE))


# BOR ---------------------------------------------------------------------

##### Graph

## Filter to last 3 weeks

# max_bor <- plyr::round_any(max(bor_long$occupied[which(bor_long$date >= date_start_3w)],
#                                na.rm = TRUE), 10, f = ceiling)

# g_bor <-
#   bor_long %>%
#   filter(date >= date_start_3w) %>%
#   # mutate(bor = case_when(bor > 1.5 ~ NA,
#   #                        TRUE ~ bor)) %>%
#   ggplot() +
#   geom_col(aes(x = date, y = occupied), fill = "grey70") +
#   geom_line(aes(x = date, y = bor*max_bor), linewidth = 1) +
#   scale_y_continuous(sec.axis = sec_axis(~./max_bor,
#                                          name = "BOR", labels = scales::percent)) +
#   scale_x_date(labels = label_date_short(), date_breaks = "1 week") +
#   facet_wrap(~name) +
#   epicurve_theme +
#   theme(panel.grid.major.x = element_blank()) +
#   labs(x = "Date", y = "Lits occupées")

# bor %>%
#   filter(epiweek %in% week3_vector) %>%
#   group_by(epiweek) %>%
#   summarise(across(c(occupied_s, occupied_c, occupied_total, dispo_sp, dispo_c, dispo_total),
#                    mean, na.rm = TRUE)) %>%
#   mutate(across(c(occupied_s, occupied_c, occupied_total,
#                   dispo_sp, dispo_c, dispo_total), round)) %>%
#   rowwise() %>%
#   mutate(occupation_s = scales::percent(occupied_s/dispo_sp, 1),
#          occupation_c = scales::percent(occupied_c/dispo_c, 1),
#          occupation_total = scales::percent(occupied_total/dispo_total, 1))

g_bor_day <-
  bor_long %>%
  filter(name %in% c("Confirmé", "Suspect")) %>%
  filter(date >= report_date-60) %>%
  ggplot() +
  geom_line(aes(x = date, y = bor, color = name), linewidth = 1) +
  scale_x_date(labels = label_date_short(), date_breaks = "1 week") +
  scale_y_continuous(limits = c(0, NA), labels = scales::percent) +
  labs(x = "Date", y = "Taux d'occupation") +
  scale_color_manual(values = c("Confirmé" = "firebrick", "Suspect" = "cadetblue")) +
  epicurve_theme +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.x = element_blank())


  
##### Latest figures

bor_susp <- bor %>%
  filter(date == report_date) %>%
  pull(occupation_sp)

bor_conf <- bor %>%
  filter(date == report_date) %>%
  pull(occupation_c)




# BOR - by week
# - average patients/bed (overall, suspect, confirmé)
# - average length of stay (overall, suspect, confirmé)

# bor %>%
#   filter(epiweek %in% week3_vector) %>%
#   group_by(epiweek) %>%
#   summarise(across(c(occupied_s, occupied_c, occupied_total, dispo_sp, dispo_c, dispo_total),
#                    mean, na.rm = TRUE)) %>%
#   mutate(across(c(occupied_s, occupied_c, occupied_total,
#                   dispo_sp, dispo_c, dispo_total), round)) %>%
#   rowwise() %>%
#   mutate(occupation_s = scales::percent(occupied_s/dispo_sp, 1),
#          occupation_c = scales::percent(occupied_c/dispo_c, 1),
#          occupation_total = scales::percent(occupied_total/dispo_total, 1))


bor_occ <- bor_long1 %>%
  filter(epiweek %in% week3_vector) %>%
  group_by(epiweek, name) %>%
  summarise(across(c(occupied), mean, na.rm = TRUE)) %>%
  mutate(occupied = round(occupied, 1)) %>%
  pivot_wider(names_from = epiweek, values_from = occupied) %>%
  mutate(label = "Patients au lit", across(everything(), as.character))

bor_beds <- bor_long3 %>%
  filter(epiweek %in% week3_vector) %>%
  group_by(epiweek, name) %>%
  summarise(across(c(dispo), mean, na.rm = TRUE)) %>%
  mutate(dispo = round(dispo)) %>%
  pivot_wider(names_from = epiweek, values_from = dispo) %>%
  mutate(label = "Lits disponibles", across(everything(), as.character))

bor_taux <- bor_long2 %>%
  filter(epiweek %in% week3_vector) %>%
  mutate(across(c(occupied_total, dispo_total), na_if, 0)) %>%
  group_by(epiweek, name) %>%
  summarise(across(c(bor), mean, na.rm = TRUE)) %>%
  mutate(bor = scales::percent(bor, 1)) %>%
  pivot_wider(names_from = epiweek, values_from = bor) %>%
  mutate(label = "Taux d'occupation", across(everything(), as.character))

# bor_taux <- bor %>%
#   filter(epiweek %in% week3_vector) %>%
#   mutate(across(c(occupied_total, dispo_total), na_if, 0)) %>%
#   group_by(epiweek) %>%
#   summarise(bor_s = mean(occupied_s, na.rm = TRUE)/mean(dispo_sp, na.rm = TRUE),
#             bor_c = mean(occupied_c, na.rm = TRUE)/mean(dispo_c, na.rm = TRUE))

los <- 
  cte %>%
  filter(epiweek_exit %in% week3_vector) %>%
  mutate(name = case_when(evd_status == "Non cas" ~ "Suspect",
                          TRUE ~ evd_status)) %>%
  group_by(epiweek_exit, name) %>%
  summarise(los = as.character(round(mean(los, na.rm = TRUE), 1))) %>%
  pivot_wider(names_from = epiweek_exit, values_from = los) %>%
  mutate(label = "Durée de séjour")

los_total <-
  cte %>%
  filter(epiweek_exit %in% week3_vector) %>%
  # mutate(name = "Total") %>%
  group_by(epiweek_exit) %>%
  summarise(`Durée de séjour` = as.character(round(mean(los, na.rm = TRUE), 1)),
            `Delai DDS à l'admission` = as.character(round(mean(days_onset_adm, na.rm = TRUE), 1))) %>%
  pivot_longer(cols = -epiweek_exit, names_to = "label") %>%
  pivot_wider(names_from = epiweek_exit, values_from = value) %>%
  mutate(name = "Total") %>%
  select(name, any_of(week3_vector), label)


t_bor <-
  bind_rows(bor_occ, bor_beds, bor_taux, los, los_total) %>%
  select(name, label, everything()) %>%
  mutate(label = factor(label, levels = c(
    "Lits disponibles", "Patients au lit", "Taux d'occupation", "Durée de séjour", 
    "Delai DDS à l'admission"
  ))) %>%
  arrange(name, label) %>%
  flextable() %>%
  set_header_labels(name = "", label = "") %>%
  merge_v(j = ~name) %>%
  hline(i = ~before(label, "Lits disponibles")) %>%
  bold(i = 1, part = "header")

rm(bor_long1, bor_long2, bor_long3, 
   bor_occ, bor_taux, bor_beds, 
   los, los_total)





# Exits -------------------------------------------------------------------

##### CFR among conf

cfr_conf <- cte %>%
  filter(evd_status == "Confirmé", rep_exit == 1) %>%
  mutate(death = case_when(type_of_exit == "Décédé" ~ 1,
                           type_of_exit == "Guéri" ~ 0)) %>%
  epitabulate::case_fatality_rate_df(
    deaths = death,
    mergeCI = TRUE,
    digits = 1
  )


##### Mortality - among conf and suspected - over time?

deaths_week <- cte %>%
  filter(evd_status == "Confirmé",
         rep_exit == 1,
         type_of_exit %in% c("Décédé", "Guéri"),
         !is.na(week_exit)) %>%
  group_by(week_exit, type_of_exit) %>%
  count() %>%
  group_by(week_exit) %>%
  mutate(pct = n / sum(n)) %>%
  filter(type_of_exit == "Décédé")

max_exits_week <- cte %>%
  filter(evd_status == "Confirmé", rep_exit == 1) %>%
  group_by(week_exit) %>%
  count() %>% ungroup() %>%
  slice_max(n) %>% pull(n) %>%
  plyr::round_any(., 10, f = ceiling)

ec_exits_conf <- vis_epicurve(
  cte %>%
    filter(evd_status == "Confirmé") %>%
    mutate(type_of_exit3 = 
             fct_relevel(type_of_exit3,
                         "Retour à maison non-cas",  
                         "Inconnu", "Transféré au CTE", "Abandon", "Guéri", "Décédé")),
             fill_by = "type_of_exit3", 
             pal = c("Décédé" = "#d55e00", "Guéri" = "#72ad81", 
                     "Inconnu" = "grey80", "Abandon" = "#8585ad",
                     "Transféré au CTE" = "#87c5c5"),
             date_measure = "date_exit_eff",
             date_max = date_cut+1) +
  geom_line(data = deaths_week, aes(x = week_exit+3.5, y = pct*max_exits_week),
            color = "#391313", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./max_exits_week,
                                         name = "% décédé", labels = scales::percent)) +
  labs(x = "Semaine de sortie", y = "Sorties") +
  theme(legend.position = "top", legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE))


##### Deaths by case status

ec_deaths <- vis_epicurve(
  cte %>%
    filter(type_of_exit == "Décédé") %>%
    mutate(evd_status = factor(evd_status, levels = c("Non cas", "Suspect", "Probable", "Confirmé"))),
  fill_by = "evd_status",
  pal = c("Confirmé" = "#b0364e", "Probable" = "#d27760", "Suspect" = "#669999", "Non cas" = "#a4cd98"), 
  date_measure = "date_exit_eff",
  date_max = report_date+1
) +
  labs(x = "Semaine de sortie", y = "Décès") +
  theme(legend.position = "top", legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE))



##### Diagnosis for non-cases

t_dx <-
  cte %>%
  filter(evd_status == "Non cas") %>%
  mutate(diagnostic_secondary = case_when(
    diagnostic_secondary == diagnostic_principal ~ NA,
    TRUE ~ diagnostic_secondary
  )) %>%
  select(diagnostic_principal, diagnostic_secondary) %>%
  tbl_summary(
    sort = everything() ~ "frequency",
    label = list(diagnostic_principal ~ "Diagnostic principal",
                 diagnostic_secondary ~ "Diagnostic secondaire")
  ) %>%
  format_tab() %>%
  as_flex_table() %>% border_inner_h(border = border_dotted)

# 1 week

t_dx_1w <-
  cte %>%
  filter(evd_status %in% c("Suspect", "Non cas"),
         epiweek_exit == week1) %>%
  mutate(diagnostic_secondary = case_when(
    diagnostic_secondary == diagnostic_principal ~ NA,
    TRUE ~ diagnostic_secondary
  )) %>%
  select(diagnostic_principal, diagnostic_secondary, diagnostic_other_detail) %>%
  tbl_summary(
    sort = everything() ~ "frequency",
    label = list(diagnostic_principal ~ "Diagnostic principal",
                 diagnostic_secondary ~ "Diagnostic secondaire",
                 diagnostic_other_detail ~ "Autre diagnostic")
  ) %>%
  format_tab() %>%
  as_flex_table() %>% border_inner_h(border = border_dotted)

# linelist 1 week

list_dx_1w <-
  cte %>%
  filter(evd_status %in% c("Suspect", "Non cas"),
         epiweek_exit == week1) %>%
  mutate(diagnostic_secondary = case_when(
    diagnostic_secondary == diagnostic_principal ~ NA,
    TRUE ~ diagnostic_secondary
  )) %>%
  select(age, age_unit, sex, date_exit_eff,
         diagnostic_principal, diagnostic_secondary, diagnostic_other_detail) %>%
  flextable() %>% border_inner_h(border = border_dotted) %>%
  set_header_labels(age = "Age", age_unit = "", sex = "Sexe",
                    date_exit_eff = "Date de sortie",
                    diagnostic_principal = "Diagnostic 1",
                    diagnostic_secondary = "Diagnostic 2",
                    diagnostic_other_detail = "Autre/details")



# Demographics ------------------------------------------------------------

##### Age/sex

## Table

t_agesex <- 
  cte %>%
  mutate(sex_f = iconv(sex_f, to = "UTF-8", sub = "")) %>%
  select(sex_f, age_group) %>%
  tbl_summary(
    by = sex_f,
    percent = 'column',
    label = age_group ~ "Tranche d'âge"
  ) %>%
  format_tab_by2_header() %>%
  as_flex_table() %>% border_inner_h(border = border_dotted)


## By week

t_agesex_week <-
  cte %>%
  mutate(sex_f = iconv(sex_f, to = "UTF-8", sub = "")) %>%
  filter(evd_status == "Confirmé",
         epiweek_adm %in% week3_vector) %>%
  mutate(across(c(pregnant, breastfeeding), factor, levels = c("Oui", "Non", "Inc."))) %>%
  select(sex_f, age_group, pregnant, breastfeeding, epiweek_adm) %>%
  tbl_summary(
    by = epiweek_adm,
    label = list(sex_f ~ "Sexe",
                 age_group ~ "Tranche d'âge",
                 pregnant ~ "Enceinte",
                 breastfeeding ~ "Allaitante"),
    value = list(pregnant ~ "Oui", breastfeeding ~ "Oui"),
    statistic = c(pregnant, breastfeeding) ~ "{n}"
  ) %>%
  format_tab() %>%
  remove_row_type(variables = c(pregnant, breastfeeding),
                  type = "missing") %>%
  as_flex_table() %>%
  hline(i = ~before(label, "Tranche d'âge")) %>%
  hline(i = ~before(label, "Enceinte")) %>%
  hline(i = ~before(label, "Allaitante"))


## Age pyramid - by susp/conf, overall and last 3 weeks

ap1 <-
  cte %>%
  filter(evd_status == "Confirmé") %>%
  mutate(sex_f = factor(sex_f, levels = c("Féminin", "Masculin"))) %>%
  age_pyramid(age_group = "age_group",
              split_by = "sex_f") +
  scale_fill_manual(
    values=c(
      "Féminin" = alpha("#00bfbf", 0.6),
      "Masculin" = alpha("#005252", 0.6)),
    guide = guide_legend(reverse = FALSE)) +
  labs(y = "Cas (n)", x = "Tranche d'âge (années)",
       subtitle = "Cas confirmés, cumul") +
  epicurve_theme

# +
#   theme(legend.position = "top", legend.justification = "left")

ap3 <-
  cte %>%
  filter(evd_status == "Confirmé" & rep_3w_adm == 1) %>%
  mutate(sex_f = factor(sex_f, levels = c("Féminin", "Masculin"))) %>%
  age_pyramid(age_group = "age_group",
              split_by = "sex_f") +
  scale_fill_manual(
    values=c(
      "Féminin" = alpha("#00bfbf", 0.6),
      "Masculin" = alpha("#005252", 0.6)),
    guide = guide_legend(reverse = FALSE)) +
  labs(y = "Cas (n)", x = "Tranche d'âge (années)",
       subtitle = "Cas confirmés, 3 semaines") +
  epicurve_theme


ap2 <- 
  cte %>%
  mutate(sex_f = factor(sex_f, levels = c("Féminin", "Masculin"))) %>%
  age_pyramid(age_group = "age_group",
              split_by = "sex_f") +
  scale_fill_manual(
    values=c(
      "Féminin" = alpha("#00bfbf", 0.6),
      "Masculin" = alpha("#005252", 0.6)),
    guide = guide_legend(reverse = FALSE)) +
  labs(y = "Cas (n)", x = "Tranche d'âge (années)",
       subtitle = "Cumul admissions") +
  epicurve_theme


ap4 <- 
  cte %>%
  filter(rep_3w_adm == 1) %>%
  mutate(sex_f = factor(sex_f, levels = c("Féminin", "Masculin"))) %>%
  age_pyramid(age_group = "age_group",
              split_by = "sex_f") +
  scale_fill_manual(
    values=c(
      "Féminin" = alpha("#00bfbf", 0.6),
      "Masculin" = alpha("#005252", 0.6)),
    guide = guide_legend(reverse = FALSE)) +
  labs(y = "Cas (n)", x = "Tranche d'âge (années)",
       subtitle = "Cumul admissions, 3 semaines") +
  epicurve_theme

# Combine

ap <- 
  ap1 + ap2 + ap3 + ap4 +
  plot_layout(ncol = 2,
              guides = "collect", axis_titles = "collect") 
# +
#   theme(legend.position = "top", legend.justification = "left")



##### Age group over time

# All patients
age_week <- cte %>%
  filter(!is.na(age_group)) %>%
  group_by(week_adm, age_group, .drop = FALSE) %>%
  count() %>%
  group_by(week_adm) %>%
  mutate(pct = n/sum(n))

# Confirmed only
age_week_conf <- cte %>%
  filter(!is.na(age_group), evd_status == "Confirmé") %>%
  group_by(week_adm, age_group, .drop = FALSE) %>%
  tally() %>%
  group_by(week_adm) %>%
  mutate(pct = n/sum(n))

# Graph
g_age <- 
  ggplot() +
  geom_line(data = age_week %>% filter(age_group == "0-4"),
            aes(x = week_adm+3.5, y = pct, color = "Tous patients"), linewidth = 1) +
  geom_line(data = age_week_conf %>% filter(age_group == "0-4"),
            aes(x = week_adm+3.5, y = pct, color = "Confirmé"), linewidth = 1) +
  scale_color_manual(values = c("Confirmé" = "red", "Tous patients" = "black")) +
  scale_x_date(labels = label_date_short(), date_breaks = "2 weeks",
               date_minor_breaks = "1 week",
               limits = c(min(age_week$week_adm), max(age_week$week_adm)+6)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Semaine d'admission", y = "% des patients <5 ans") +
  epicurve_theme +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.x = element_blank()) +
  guides(x = guide_axis(minor.ticks = TRUE))



##### Pregnant women

n_preg <- sum(cte$pregnant == "Oui", na.rm = TRUE)
n_lact <- sum(cte$breastfeeding == "Oui", na.rm = TRUE)


##### Profession

# cte %>%
#   filter(epiweek_adm == week1) %>%
#   select(evd_status, job) %>%
#   tbl_summary(
#     by = evd_status,
#     sort = everything() ~ "frequency"
#   )



##### Geography - AS (table)

t_as <- 
  cte %>%
  filter(epiweek_adm == week1) %>%
  mutate(adm_print = case_when(adm2_name_res == "Mongbwalu" ~ adm3_name_res,
                               TRUE ~ paste0("ZS ", adm2_name_res, " - ", adm3_name_res))) %>%
  mutate(evd_status = fct_drop(evd_status)) %>%
  tabyl(adm_print, evd_status) %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns(position = "front") %>%
  mutate(adm_print = replace_na(adm_print, "Inconnu")) %>%
  flextable() %>%
  set_header_labels(adm_print = "Aire de Santé") %>%
  bold(i = 1, part = "header")


t_as_total <- 
  cte %>%
  # filter(epiweek_adm == week1) %>%
  mutate(adm_print = case_when(adm2_name_res == "Mongbwalu" ~ adm3_name_res,
                               TRUE ~ paste0("ZS ", adm2_name_res))) %>%
  tabyl(adm_print, evd_status) %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns(position = "front") %>%
  mutate(adm_print = replace_na(adm_print, "Inconnu")) %>%
  flextable() %>%
  set_header_labels(adm_print = "Aire de Santé") %>%
  bold(i = 1, part = "header")


# Délais épidémiologiques -------------------------------------------------

##### LOS

# Table


delays_los <- cte %>%
  filter(rep_exit == 1, evd_status %in% c("Confirmé", "Non cas")) %>%
  group_by(evd_status, type_of_exit3) %>%
  summarise(med = median(los, na.rm = TRUE),
            q1 = quantile(los, probs = 0.25, na.rm = TRUE),
            q3 = quantile(los, probs = 0.75, na.rm = TRUE))


# LOS over time

g_los_week <-
  cte %>%
  filter(rep_exit == 1) %>%
  mutate(evd_status = case_when(evd_status == "Confirmé" ~ evd_status,
                                evd_status %in% c("Non cas", "Probable", "Suspect") ~ "Suspect")) %>%
  group_by(week_adm, evd_status) %>%
  summarise(los = mean(los, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = week_adm+3.5, y = los, color = evd_status), linewidth = 1) +
  scale_color_manual(values = c("Confirmé" = "#b30000", "Suspect" = "cadetblue")) +
  scale_x_date(labels = label_date_short(), date_breaks = "1 week",
               limits = c(min(cte$week_adm)+7, max(cte$week_adm+6))) +
  labs(x = "Semaine d'admission", y = "Durée moyenne de séjour") +
  scale_y_continuous(limits = c(0, NA)) +
  epicurve_theme +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.x = element_blank())

  

# Graph

g_timing_conf2 <- 
  cte %>%
  filter(rep_exit == 1, evd_status == "Confirmé",
         type_of_exit %in% c("Décédé", "Guéri")) %>%
  select(patient_site_id, evd_status, type_of_exit, los) %>%
  pivot_longer(cols = -c(patient_site_id, evd_status, type_of_exit)) %>%
  ggplot(aes(x = type_of_exit, y = value, color = type_of_exit)) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2, height = 0.2), alpha = 0.5) +
  geom_boxplot(width = 0.5, alpha = 0.2) +
  scale_color_manual(values = c("Décédé" = "#bc5c5c", "Guéri" = "#006600"), guide = "none") +
  scale_y_continuous(limits = c(0, 25)) +
  facet_wrap(~evd_status) +
  epicurve_theme +
  labs(x = "Type d'issue", y = "Jours")

g_timing_noncas2 <-
  cte %>%
  filter(rep_exit == 1, evd_status == "Non cas",
         type_of_exit3 %in% c("Décédé", "Retour à maison non-cas", "Transféré (non cas)")) %>%
  select(patient_site_id, evd_status, type_of_exit3, los) %>%
  pivot_longer(cols = -c(patient_site_id, evd_status, type_of_exit3)) %>%
  ggplot(aes(x = type_of_exit3, y = value, color = type_of_exit3)) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2, height = 0.2), alpha = 0.5) +
  geom_boxplot(width = 0.5, alpha = 0.2) +
  scale_color_manual(values = c("Décédé" = "#bc5c5c", "Retour à maison non-cas" = "#006600",
                                "Transféré (non cas)" = "grey50"), guide = "none") +
  scale_y_continuous(limits = c(0, 25)) +
  facet_wrap(~evd_status) +
  epicurve_theme +
  labs(x = "Type d'issue", y = "Jours")

g_los <- g_timing_conf2 + g_timing_noncas2 +
  plot_layout(axis_titles = "collect")


##### Sx onset to admission

## Onset to admission and exit
g_timing_conf1 <- 
  cte %>%
  filter(rep_exit == 1, evd_status == "Confirmé",
         type_of_exit %in% c("Décédé", "Guéri")) %>%
  select(patient_site_id, evd_status, type_of_exit, days_onset_adm, days_onset_exit) %>%
  pivot_longer(cols = -c(patient_site_id, evd_status, type_of_exit)) %>%
  mutate(label = case_when(name == "days_onset_adm" ~ "DDS à l'admission au CTE",
                           name == 'days_onset_exit' ~ "DDS à l'issue final")) %>%
  ggplot(aes(x = type_of_exit, y = value, color = type_of_exit)) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2, height = 0.2), alpha = 0.5) +
  geom_boxplot(width = 0.5, alpha = 0.2) +
  scale_color_manual(values = c("Décédé" = "#bc5c5c", "Guéri" = "#006600"), guide = "none") +
  facet_grid(evd_status ~ label) +
  epicurve_theme +
  labs(x = "Type d'issue", y = "Jours")

g_timing_noncas1 <- 
  cte %>%
  filter(rep_exit == 1, evd_status == "Non cas",
         type_of_exit3 %in% c("Décédé", "Retour à maison non-cas", "Transféré (non cas)")) %>%
  select(patient_site_id, evd_status, type_of_exit3, days_onset_adm, days_onset_exit) %>%
  pivot_longer(cols = -c(patient_site_id, evd_status, type_of_exit3)) %>%
  mutate(label = case_when(name == "days_onset_adm" ~ "DDS à l'admission au CTE",
                           name == 'days_onset_exit' ~ "DDS à l'issue final")) %>%
  ggplot(aes(x = type_of_exit3, y = value, color = type_of_exit3)) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2, height = 0.2), alpha = 0.5) +
  geom_boxplot(width = 0.5, alpha = 0.2) +
  scale_color_manual(values = c("Décédé" = "#bc5c5c", "Retour à maison non-cas" = "#006600",
                                "Transféré (non cas)" = "grey50"), guide = "none") +
  scale_y_continuous(limits = c(0, 40)) +
  facet_grid(evd_status ~ label) +
  epicurve_theme +
  labs(x = "Type d'issue", y = "Jours")

g_timing <- g_timing_conf1 + g_timing_noncas1 +
  plot_layout(ncol = 1, axes = "collect")





