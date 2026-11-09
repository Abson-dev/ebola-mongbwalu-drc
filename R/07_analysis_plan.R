################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   7. Descriptive and bivariate analysis of the CTE line list
#      Implements the analysis plan of Dr Abdul Salam Mbengue (09/11/2026)
#
#   Requires: 01_setup.R, 02_data_linelist.R (and 04_analysis_cte.R for the
#             report, which reuses some sitrep tables and figures)
#   Output  : objects prefixed `pa_` (plan d'analyse) + `pa_tables`, a named
#             list of data frames exported to Excel by the render script
#
#   Mapping between the variables named in the plan and the MSF line list is
#   documented in docs/analysis-plan.md.
#
#   Author: Aboubacar Hema
#   Last updated: 9 November 2026
#
################################################################################


# Analysis dataset --------------------------------------------------------

symptom_vars <- c(
  "Fièvre"                       = "fever",
  "Nausées / vomissements"       = "nausea",
  "Diarrhée"                     = "diarrhoea",
  "Céphalées"                    = "headache",
  "Fatigue générale intense"     = "asthenia_weakness",
  "Perte d'appétit / anorexie"   = "loss_of_appetite",
  "Douleurs abdominales"         = "abdominal_pain",
  "Douleurs musculaires/osseuses" = "bone_muscle_pain",
  "Douleurs articulaires"        = "joint_pain",
  "Douleurs thoraciques"         = "chest_pain",
  "Toux"                         = "cough",
  "Difficultés respiratoires"    = "breathlessness",
  "Difficultés à avaler"         = "swallowing_problem",
  "Maux de gorge"                = "sorethroat",
  "Hoquet"                       = "hichups",
  "Conjonctivite"                = "conjunctivitis",
  "Ictère"                       = "jaundice",
  "Éruption cutanée"             = "skin_rash",
  "Confusion / désorientation"   = "confused_disoriented",
  "Coma"                         = "coma"
)

bleeding_vars <- c(
  "Saignement (tout type)"            = "bleeding",
  "Épistaxis"                         = "bleeding_epistaxis",
  "Saignement des gencives"           = "bleeding_gum",
  "Vomissements sanglants (hématémèse)" = "bleeding_vomit",
  "Vomito negro"                      = "bleeding_vomito_negro",
  "Méléna"                            = "bleeding_melenas",
  "Hémoptysie"                        = "bleeding_hemoptisis",
  "Saignements vaginaux / génitaux"   = "bleeding_vagina",
  "Hématurie"                         = "bleeding_urine",
  "Saignement au point d'injection"   = "bleeding_injection_site",
  "Hématomes / pétéchies / purpura"   = "hematomes_petechies_purpura",
  "Autres saignements"                = "bleeding_other"
)

pa <- dat |>
  filter(date_admission_eff <= report_date | is.na(date_admission_eff)) |>
  mutate(
    # Death: exit "Décédé" or dead on arrival (see docs/analysis-plan.md)
    deces = type_of_exit %in% "Décédé" | dead_upon_arrival %in% "Oui",
    date_deces = coalesce(date_death, if_else(dead_upon_arrival %in% "Oui", date_admission_eff, NA)),
    confirme = evd_status %in% "Confirmé",
    statut = fct_na_value_to_level(evd_status, "Inconnu"),
    # Health-care workers
    personnel_sante = str_detect(replace_na(job, ""), "^Personnel de sant"),
    profession = fct_na_value_to_level(factor(job), "Non renseigné"),
    # Age
    age_group_plan = bin_numbers(age_years, cfg$age_breaks_plan),
    age_cat2 = factor(if_else(age_years < 15, "< 15 ans", "15 ans et +"),
                      levels = c("< 15 ans", "15 ans et +")),
    # Residence
    aire_sante = case_when(
      adm2_name_res == cfg$health_zone ~ adm3_name_res,
      !is.na(adm2_name_res) ~ paste("ZS", adm2_name_res),
      TRUE ~ NA_character_
    ),
    residence_cat2 = factor(if_else(adm3_name_res == cfg$health_zone, "AS Mongbwalu", "Autres localisations"),
                            levels = c("AS Mongbwalu", "Autres localisations")),
    # Delay from symptom onset to notification
    delai_pec = as.numeric(date_notification - date_symptom_onset),
    delai_pec = if_else(delai_pec < 0 | delai_pec > 60, NA_real_, delai_pec),
    delai_pec_cat = cut(delai_pec, breaks = cfg$delay_breaks, labels = cfg$delay_labels),
    # 3 classes for bivariate analyses ("< 1 jour" is too small to be a reference)
    delai_pec_cat3 = cut(delai_pec, breaks = c(-Inf, 2, 5, Inf),
                         labels = c("0-2 jours", "3-5 jours", "> 5 jours")),
    # Number of lab samples
    n_prelevements = rowSums(!is.na(across(matches("^lab_result_\\d+$"))))
  )

conf <- filter(pa, confirme)


# 1. Key figures ------------------------------------------------------------

pa_key <- tibble::tribble(
  ~Indicateur, ~N,
  "Patients enregistrés dans la line list", nrow(pa),
  "Cas confirmés", sum(pa$confirme),
  "Cas probables", sum(pa$evd_status %in% "Probable"),
  "Cas suspects (en attente de classification)", sum(pa$evd_status %in% "Suspect"),
  "Non-cas", sum(pa$evd_status %in% "Non cas"),
  "Patients non confirmés (tous statuts autres que Confirmé)", sum(!pa$confirme),
  "Personnels de santé (tous statuts)", sum(pa$personnel_sante),
  "Personnels de santé confirmés", sum(pa$personnel_sante & pa$confirme),
  "Décès (tous statuts, y compris décès à l'arrivée)", sum(pa$deces),
  "Décès à l'arrivée", sum(pa$dead_upon_arrival %in% "Oui"),
  "Décès parmi les cas confirmés", sum(pa$deces & pa$confirme)
) |>
  mutate(N = as.integer(N))

pa_hcw <- pa |>
  filter(personnel_sante) |>
  mutate(categorie = str_remove(job, "^Personnel de santé: ?"),
         structure = str_remove(hf_name_job, " \\|.*$")) |>
  count(Catégorie = categorie, Structure = replace_na(structure, "Non renseignée"),
        Statut = statut, name = "N") |>
  arrange(desc(N))


# 2. Last week ------------------------------------------------------------

#  Admissions exclude deaths on arrival, as in the weekly sitrep
is_doa <- pa$dead_upon_arrival %in% "Oui"
pa_week <- tibble(
  semaine = week1,
  admissions = sum(pa$epiweek_adm %in% week1 & !is_doa),
  admissions_prec = sum(pa$epiweek_adm %in% week2 & !is_doa),
  confirmes = sum(pa$epiweek_adm %in% week1 & pa$confirme & !is_doa),
  deces = sum(pa$deces & epiweek_label(pa$date_deces) %in% week1),
  deces_arrivee = sum(pa$dead_upon_arrival %in% "Oui" & pa$epiweek_adm %in% week1)
)


# 3. Socio-demographics of confirmed cases --------------------------------

pa_age_stats <- conf |>
  summarise(
    `N (âge connu)` = sum(!is.na(age_years)),
    `Moyenne` = round(mean(age_years, na.rm = TRUE), 1),
    `Écart type` = round(sd(age_years, na.rm = TRUE), 1),
    `Médiane` = round(median(age_years, na.rm = TRUE), 1),
    `IIQ` = paste(round(quantile(age_years, c(0.25, 0.75), na.rm = TRUE), 1), collapse = "-"),
    `Minimum` = round(min(age_years, na.rm = TRUE), 2),
    `Maximum` = round(max(age_years, na.rm = TRUE), 1)
  )

pa_age_group <- freq_table(conf, "age_group_plan", "Tranche d'âge (ans)", sort = FALSE)
pa_sex       <- freq_table(mutate(conf, sex_f = sex_f), "sex_f", "Sexe")
pa_job       <- freq_table(conf, "job", "Profession")
pa_pregnant  <- freq_table(filter(conf, sex == "F"), "pregnant", "Enceinte (femmes)")
pa_breastf   <- freq_table(filter(conf, sex == "F"), "breastfeeding", "Allaitante (femmes)")

pa_sex_ratio <- sum(conf$sex == "M", na.rm = TRUE) / sum(conf$sex == "F", na.rm = TRUE)

pa_agesex <- conf |>
  filter(!is.na(sex_f)) |>
  select(age_group_plan, sex_f) |>
  tbl_summary(by = sex_f, percent = "column", missing_text = "Âge inconnu",
              label = age_group_plan ~ "Tranche d'âge") |>
  add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") |>
  format_tab_by2_header()

pa_pyramid <- conf |>
  mutate(sex_f = factor(sex_f, levels = c("Féminin", "Masculin"))) |>
  plot_age_pyramid(age_group = "age_group_plan", split_by = "sex_f") +
  labs(y = "Cas confirmés (n)", x = "Tranche d'âge (années)")


# 4. Residence ------------------------------------------------------------

pa_residence <- pa |>
  mutate(aire_sante = replace_na(aire_sante, "Inconnu"),
         statut = fct_drop(statut)) |>
  tabyl(aire_sante, statut) |>
  adorn_totals(where = c("row", "col")) |>
  adorn_percentages(denominator = "col") |>
  adorn_pct_formatting(digits = 0) |>
  adorn_ns(position = "front") |>
  rename(`Aire de santé de résidence` = aire_sante)

pa_residence_conf_wk <- pa |>
  filter(confirme, epiweek_adm %in% week1) |>
  count(aire_sante, sort = TRUE)


# 5. Epidemic curves ------------------------------------------------------

pa_ec_conf <- conf |>
  mutate(issue = factor(if_else(deces, "Décédé", "Vivant / issue inconnue"),
                        levels = c("Vivant / issue inconnue", "Décédé"))) |>
  vis_epicurve(date_measure = "date_notification", date_max = date_cut + 1,
               fill_by = "issue", pal = c("Décédé" = "#b0364e", "Vivant / issue inconnue" = "#7fa7b5")) +
  labs(x = "Semaine de notification", y = "Cas confirmés (n)") +
  theme(legend.position = "top", legend.justification = "left")

pa_deaths_day <- pa |>
  filter(deces, !is.na(date_deces), date_deces <= report_date) |>
  mutate(statut = fct_drop(statut)) |>
  count(date_deces, statut) |>
  ggplot(aes(x = date_deces, y = n, fill = statut)) +
  geom_col(width = 1) +
  scale_fill_manual(values = c("Confirmé" = "#b0364e", "Probable" = "#d57689",
                               "Suspect" = "#d27760", "Non cas" = "#a4cd98", "Inconnu" = "grey70")) +
  scale_x_date(labels = label_date_short(), date_breaks = "2 weeks") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Date du décès", y = "Décès (n)") +
  epicurve_theme +
  theme(legend.position = "top", legend.justification = "left", panel.grid.major.x = element_blank())

pa_deaths_week <- pa |>
  filter(deces, !is.na(date_deces), date_deces <= report_date) |>
  mutate(statut = fct_drop(statut)) |>
  vis_epicurve(date_measure = "date_deces", date_max = date_cut + 1, fill_by = "statut",
               pal = c("Confirmé" = "#b0364e", "Probable" = "#d57689", "Suspect" = "#d27760",
                       "Non cas" = "#a4cd98", "Inconnu" = "grey70")) +
  labs(x = "Semaine du décès", y = "Décès (n)") +
  theme(legend.position = "top", legend.justification = "left")


# 6. Clinical signs among confirmed cases ---------------------------------

sign_table <- function(df, vars) {
  purrr::imap_dfr(vars, function(v, lab) {
    x <- df[[v]]
    known <- sum(x %in% c("Oui", "Non"))
    yes <- sum(x %in% "Oui")
    tibble(`Signe clinique` = lab, `Renseigné (N)` = known, `Présent (n)` = yes,
           `%` = if (known > 0) round(100 * yes / known, 1) else NA_real_)
  }) |>
    arrange(desc(`%`))
}

pa_signs    <- sign_table(conf, symptom_vars)
pa_bleeding <- sign_table(conf, bleeding_vars)


# 7. Mortality ------------------------------------------------------------

pa_deaths_status <- pa |>
  filter(deces) |>
  mutate(type = if_else(dead_upon_arrival %in% "Oui", "Décès à l'arrivée", "Décès au CTE")) |>
  tabyl(statut, type, show_missing_levels = FALSE) |>
  adorn_totals(where = c("row", "col")) |>
  rename(`Statut MVE` = statut)

pa_deaths_as <- pa |>
  filter(deces) |>
  mutate(aire_sante = replace_na(aire_sante, "Inconnu")) |>
  count(`Aire de santé` = aire_sante, `Confirmé` = if_else(confirme, "Confirmé", "Non confirmé")) |>
  pivot_wider(names_from = `Confirmé`, values_from = n, values_fill = 0) |>
  mutate(Total = rowSums(across(where(is.numeric)))) |>
  arrange(desc(Total)) |>
  adorn_totals("row")

pa_cfr <- conf |>
  filter(type_of_exit %in% c("Décédé", "Guéri")) |>
  mutate(d = as.integer(type_of_exit == "Décédé")) |>
  pull(d) |>
  cfr_wilson()


# 8. Laboratory -----------------------------------------------------------

pa_lab <- pa |>
  mutate(lab_result_1 = fct_na_value_to_level(factor(lab_result_1), "Pas de résultat"),
         n_prelevements = factor(pmin(n_prelevements, 3), levels = 0:3,
                                 labels = c("0", "1", "2", "3 et +"))) |>
  mutate(statut = fct_drop(statut)) |>
  select(statut, lab_result_1, n_prelevements) |>
  tbl_summary(by = statut,
              label = list(lab_result_1 ~ "Résultat du 1er prélèvement",
                           n_prelevements ~ "Nombre de prélèvements")) |>
  add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") |>
  format_tab_by2_header()


# 9. Delay from symptom onset to notification -----------------------------

pa_delay <- pa |>
  filter(!is.na(statut)) |>
  mutate(groupe = if_else(confirme, "Cas confirmés", "Non confirmés")) |>
  select(groupe, delai_pec, delai_pec_cat) |>
  tbl_summary(by = groupe,
              type = delai_pec ~ "continuous",
              statistic = delai_pec ~ "{median} ({p25}-{p75})",
              label = list(delai_pec ~ "Délai (jours), médiane (IIQ)",
                           delai_pec_cat ~ "Délai de prise en charge"),
              missing_text = "Non calculable") |>
  add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") |>
  format_tab_by2_header()


# 10. Bivariate analyses --------------------------------------------------

exposures <- c("Âge" = "age_cat2",
               "Lieu de résidence" = "residence_cat2",
               "Délai de prise en charge" = "delai_pec_cat3")

# a) Laboratory confirmation among patients with a final classification
pa_biv_conf <- pa |>
  filter(evd_status %in% c("Confirmé", "Non cas")) |>
  bivariate_table("confirme", exposures)

# b) Death among confirmed cases with a known outcome
pa_biv_death <- conf |>
  filter(type_of_exit %in% c("Décédé", "Guéri")) |>
  mutate(deces_conf = type_of_exit == "Décédé") |>
  bivariate_table("deces_conf", exposures)

# c) Health area (detail) x confirmation and x death
pa_as_conf <- pa |>
  filter(evd_status %in% c("Confirmé", "Non cas"), !is.na(aire_sante)) |>
  mutate(aire_sante = fct_lump_min(aire_sante, min = 10, other_level = "Autres")) |>
  group_by(`Aire de santé` = aire_sante) |>
  summarise(Patients = n(), `Confirmés` = sum(confirme),
            `% confirmés` = round(100 * mean(confirme), 1), .groups = "drop") |>
  arrange(desc(Patients))

pa_as_death <- conf |>
  filter(type_of_exit %in% c("Décédé", "Guéri"), !is.na(aire_sante)) |>
  mutate(aire_sante = fct_lump_min(aire_sante, min = 5, other_level = "Autres")) |>
  group_by(`Aire de santé` = aire_sante) |>
  summarise(`Confirmés (issue connue)` = n(), `Décès` = sum(type_of_exit == "Décédé"),
            `Létalité (%)` = round(100 * mean(type_of_exit == "Décédé"), 1), .groups = "drop") |>
  arrange(desc(`Confirmés (issue connue)`))


# 11. Data quality: missing data ------------------------------------------

quality_vars <- c(
  "Sexe" = "sex", "Âge" = "age", "Unité d'âge" = "age_unit", "Profession" = "job",
  "Zone de santé de résidence" = "adm2_name_res", "Aire de santé de résidence" = "adm3_name_res",
  "Village / quartier" = "adm4_name_res", "Date de début des symptômes" = "date_symptom_onset",
  "Date d'admission" = "date_admission_eff", "Date de notification" = "date_notification",
  "Date de sortie" = "date_exit_eff", "Type de sortie" = "type_of_exit",
  "Résultat labo 1" = "lab_result_1", "Statut MVE" = "evd_status"
)

pa_missing <- missing_summary(pa, unname(quality_vars), names(quality_vars))
pa_missing_preg <- missing_summary(filter(pa, sex == "F", age_years >= 13, age_years <= 50),
                                   c("pregnant", "breastfeeding"),
                                   c("Grossesse (femmes 13-50 ans)", "Allaitement (femmes 13-50 ans)"))


# Tables for Excel export -------------------------------------------------

pa_tables <- list(
  "1_chiffres_cles"     = pa_key,
  "1b_personnel_sante"  = pa_hcw,
  "2_derniere_semaine"  = pa_week,
  "3_age_stats"         = pa_age_stats,
  "3_tranches_age"      = pa_age_group,
  "3_sexe"              = pa_sex,
  "3_profession"        = pa_job,
  "3_enceinte"          = pa_pregnant,
  "3_allaitante"        = pa_breastf,
  "4_residence"         = pa_residence,
  "6_signes"            = pa_signs,
  "6_saignements"       = pa_bleeding,
  "7_deces_statut"      = pa_deaths_status,
  "7_deces_AS"          = pa_deaths_as,
  "7_letalite"          = pa_cfr,
  "10_biv_confirmation" = pa_biv_conf,
  "10_biv_deces"        = pa_biv_death,
  "10_AS_confirmation"  = pa_as_conf,
  "10_AS_letalite"      = pa_as_death,
  "11_donnees_manq"     = bind_rows(pa_missing, pa_missing_preg)
)

rm(sign_table, exposures, quality_vars, is_doa)
