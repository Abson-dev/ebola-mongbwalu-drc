################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   4. Ebola treatment centre (CTE) indicators for the weekly sitrep
#
#   Requires: 01_setup.R, 02_data_linelist.R
#   Output  : summary figures (n_*), tables (t_*), figures (ec_*, g_*, ap*)
#
#   Author: Stephanie Ngai (original), refactored by Aboubacar Hema
#   Last updated: 9 November 2026
#
################################################################################


# Data --------------------------------------------------------------------

## Patients admitted up to the report date, excluding deaths on arrival
cte <- dat |>
  filter(date_admission_eff <= report_date) |>
  filter(dead_upon_arrival != "Oui" | is.na(dead_upon_arrival)) |>
  mutate(type_of_exit3 = factor(type_of_exit3, levels = c(
    "Guéri", "Décédé", "Retour à maison non-cas", "Transféré au CTE",
    "Transféré autre structure (non cas)", "Abandon", "Inconnu"
  )))

doa <- dat |>
  filter(date_admission_eff <= report_date, dead_upon_arrival == "Oui")


# Summary figures ---------------------------------------------------------

is_conf <- cte$evd_status %in% "Confirmé"

n_adm          <- sum(cte$admission)
n_adm_thisweek <- sum(cte$epiweek_adm %in% week1)
n_adm_lastweek <- sum(cte$epiweek_adm %in% week2)

n_exits          <- sum(cte$rep_exit)
n_exits_thisweek <- sum(cte$epiweek_exit %in% week1)
n_exits_lastweek <- sum(cte$epiweek_exit %in% week2)

n_conf <- sum(is_conf)
n_prob <- sum(cte$evd_status %in% "Probable")
n_conf_thisweek <- sum(is_conf & cte$rep_1w_adm == 1)

n_conf_dcd   <- sum(is_conf & cte$rep_exit == 1 & cte$type_of_exit %in% "Décédé")
n_conf_gueri <- sum(is_conf & cte$rep_exit == 1 & cte$type_of_exit %in% "Guéri")

n_conf_exits          <- sum(is_conf & cte$rep_exit == 1)
n_conf_exits_thisweek <- sum(is_conf & cte$epiweek_exit %in% week1)
n_conf_exits_lastweek <- sum(is_conf & cte$epiweek_exit %in% week2)

n_conf_dcd_thisweek   <- sum(is_conf & cte$epiweek_exit %in% week1 & cte$type_of_exit %in% "Décédé")
n_conf_gueri_thisweek <- sum(is_conf & cte$epiweek_exit %in% week1 & cte$type_of_exit %in% "Guéri")
n_conf_abandon_thisweek <- sum(is_conf & cte$epiweek_exit %in% week1 & cte$type_of_exit %in% "Abandon")

n_doa      <- nrow(doa)
n_doa_conf <- sum(doa$evd_status %in% "Confirmé")

# Exits of the current week by status (used in the report text)
exits_wk <- cte |> filter(epiweek_exit %in% week1)
n_noncas_exit_wk     <- sum(exits_wk$evd_status %in% "Non cas")
n_noncas_home_wk     <- sum(exits_wk$evd_status %in% "Non cas" & exits_wk$type_of_exit3 %in% "Retour à maison non-cas")
n_noncas_transfer_wk <- sum(exits_wk$evd_status %in% "Non cas" &
                              exits_wk$type_of_exit3 %in% "Transféré autre structure (non cas)")
n_noncas_dcd_wk      <- sum(exits_wk$evd_status %in% "Non cas" & exits_wk$type_of_exit3 %in% "Décédé")
n_susp_exit_wk       <- sum(exits_wk$evd_status %in% "Suspect")
n_susp_dcd_wk        <- sum(exits_wk$evd_status %in% "Suspect" & exits_wk$type_of_exit %in% "Décédé")
n_susp_abandon_wk    <- sum(exits_wk$evd_status %in% "Suspect" & exits_wk$type_of_exit %in% "Abandon")


# Table 1: admissions and exits, last 3 weeks and cumulative --------------

## Make sure every reporting week has a column (weeks without events = 0)
fill_weeks <- function(df, fill = "0") {
  for (w in setdiff(week3_vector, names(df))) df[[w]] <- fill
  mutate(df, across(any_of(week3_vector), ~ replace_na(as.character(.x), fill)))
}

## One "header" row: counts by week + cumulative
count_row <- function(df, week_var, value_var, label, cumul) {
  df |>
    group_by(.data[[week_var]]) |>
    summarise(n = sum(.data[[value_var]]), .groups = "drop") |>
    pivot_wider(names_from = all_of(week_var), values_from = n) |>
    mutate(name = label) |>
    fill_weeks() |>
    select(name, all_of(week3_vector)) |>
    mutate(Cumul = cumul) |>
    mutate(across(everything(), as.character))
}

## Exit rows for one case status: total + breakdown by exit type
exit_rows <- function(status, label) {
  d <- cte |> filter(evd_status == status, rep_exit == 1)
  if (nrow(d) == 0) return(NULL)
  total <- d |>
    tabyl(rep_exit, epiweek_exit) |>
    adorn_totals(where = "col", name = "Cumul") |>
    mutate(name = label) |>
    fill_weeks() |>
    select(name, all_of(week3_vector), Cumul) |>
    mutate(across(everything(), as.character))
  detail <- d |>
    tabyl(type_of_exit3, epiweek_exit, show_missing_levels = FALSE) |>
    adorn_totals(where = "col", name = "Cumul") |>
    adorn_percentages(denominator = "col") |>
    adorn_pct_formatting(digits = 0) |>
    adorn_ns(position = "front") |>
    select(name = type_of_exit3, any_of(week3_vector), Cumul) |>
    mutate(across(everything(), as.character))
  bind_rows(total, detail)
}

header_rows <- c("Admissions", "Sorties", "Sortie non cas", "Sortie confirmé",
                 "Sortie probable", "Sortie suspect")

t1_df <- bind_rows(
  count_row(cte, "epiweek_adm", "admission", "Admissions", n_adm),
  count_row(filter(cte, evd_status == "Confirmé"), "epiweek_adm", "admission", "Confirmé", n_conf),
  count_row(cte, "epiweek_exit", "rep_exit", "Sorties", n_exits),
  exit_rows("Non cas", "Sortie non cas"),
  exit_rows("Confirmé", "Sortie confirmé"),
  exit_rows("Probable", "Sortie probable"),
  exit_rows("Suspect", "Sortie suspect")
)

t1 <- t1_df |>
  flextable() |>
  border_inner_h(border = border_dotted) |>
  set_header_labels(name = "") |>
  hline(i = c(2, 3), part = "body") |>
  bold(i = 1, part = "header") |>
  bold(i = ~ name %in% header_rows) |>
  bg(i = ~ name %in% header_rows, part = "body", bg = "grey90")

rm(fill_weeks, count_row, exit_rows)


# Admissions --------------------------------------------------------------

pal_status <- c("Non cas" = "#a4cd98", "Suspect" = "#d27760",
                "Probable" = "#d57689", "Confirmé" = "#b0364e")

ec_adm <- cte |>
  mutate(evd_status = factor(evd_status, levels = c("Non cas", "Suspect", "Probable", "Confirmé"))) |>
  vis_epicurve(date_measure = "date_admission_eff",
               date_min = min(cte$week_adm, na.rm = TRUE),
               date_max = date_cut + 1,
               fill_by = "evd_status", pal = pal_status) +
  labs(x = "Semaine d'admission", y = "N. patients") +
  theme(legend.position = "top", legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE))


# Bed occupancy (BOR) -----------------------------------------------------

g_bor_day <- bor_long |>
  filter(name %in% c("Confirmé", "Suspect"), date >= report_date - 60) |>
  ggplot() +
  geom_line(aes(x = date, y = bor, colour = name), linewidth = 1) +
  scale_x_date(labels = label_date_short(), date_breaks = "1 week") +
  scale_y_continuous(limits = c(0, NA), labels = scales::percent) +
  scale_colour_manual(values = c("Confirmé" = "firebrick", "Suspect" = "cadetblue")) +
  labs(x = "Date", y = "Taux d'occupation") +
  epicurve_theme +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.x = element_blank())

## Latest figures
bor_susp <- bor |> filter(date == report_date) |> pull(occupation_sp)
bor_conf <- bor |> filter(date == report_date) |> pull(occupation_c)

## Weekly averages
weekly_mean <- function(df, value, label, fmt = function(x) round(x, 1)) {
  df |>
    filter(epiweek %in% week3_vector) |>
    group_by(epiweek, name) |>
    summarise(v = mean(.data[[value]], na.rm = TRUE), .groups = "drop") |>
    mutate(v = fmt(v)) |>
    pivot_wider(names_from = epiweek, values_from = v) |>
    mutate(label = label, across(everything(), as.character))
}

bor_occ   <- weekly_mean(bor_long1, "occupied", "Patients au lit")
bor_beds  <- weekly_mean(bor_long3, "dispo", "Lits disponibles", fmt = round)
bor_taux  <- weekly_mean(bor_long2 |> mutate(across(c(occupied_total, dispo_total), ~ na_if(.x, 0))),
                         "bor", "Taux d'occupation", fmt = function(x) scales::percent(x, 1))

los <- cte |>
  filter(epiweek_exit %in% week3_vector) |>
  mutate(name = if_else(evd_status == "Non cas", "Suspect", as.character(evd_status))) |>
  group_by(epiweek_exit, name) |>
  summarise(los = as.character(round(mean(los, na.rm = TRUE), 1)), .groups = "drop") |>
  pivot_wider(names_from = epiweek_exit, values_from = los) |>
  mutate(label = "Durée de séjour")

los_total <- cte |>
  filter(epiweek_exit %in% week3_vector) |>
  group_by(epiweek_exit) |>
  summarise(`Durée de séjour` = as.character(round(mean(los, na.rm = TRUE), 1)),
            `Delai DDS à l'admission` = as.character(round(mean(days_onset_adm, na.rm = TRUE), 1)),
            .groups = "drop") |>
  pivot_longer(cols = -epiweek_exit, names_to = "label") |>
  pivot_wider(names_from = epiweek_exit, values_from = value) |>
  mutate(name = "Total") |>
  select(name, any_of(week3_vector), label)

t_bor <- bind_rows(bor_occ, bor_beds, bor_taux, los, los_total) |>
  select(name, label, any_of(week3_vector)) |>
  mutate(label = factor(label, levels = c("Lits disponibles", "Patients au lit", "Taux d'occupation",
                                          "Durée de séjour", "Delai DDS à l'admission"))) |>
  arrange(name, label) |>
  flextable() |>
  set_header_labels(name = "", label = "") |>
  merge_v(j = ~name) |>
  hline(i = ~ before(label, "Lits disponibles")) |>
  bold(i = 1, part = "header")

rm(weekly_mean, bor_occ, bor_beds, bor_taux, los, los_total)


# Exits -------------------------------------------------------------------

##### CFR among confirmed cases with known outcome (Wilson 95% CI)

cfr_conf <- cte |>
  filter(evd_status == "Confirmé", rep_exit == 1) |>
  mutate(death = case_when(type_of_exit == "Décédé" ~ 1,
                           type_of_exit == "Guéri" ~ 0)) |>
  pull(death) |>
  cfr_wilson(digits = 1)


##### Weekly proportion of deaths among confirmed exits

deaths_week <- cte |>
  filter(evd_status == "Confirmé", rep_exit == 1,
         type_of_exit %in% c("Décédé", "Guéri"), !is.na(week_exit)) |>
  count(week_exit, type_of_exit) |>
  group_by(week_exit) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  filter(type_of_exit == "Décédé")

max_exits_week <- cte |>
  filter(evd_status == "Confirmé", rep_exit == 1) |>
  count(week_exit) |>
  pull(n) |>
  max(0) |>
  ceiling_to(10)

ec_exits_conf <- cte |>
  filter(evd_status == "Confirmé") |>
  mutate(type_of_exit3 = fct_relevel(type_of_exit3, "Retour à maison non-cas", "Inconnu",
                                     "Transféré au CTE", "Abandon", "Guéri", "Décédé")) |>
  vis_epicurve(fill_by = "type_of_exit3",
               pal = c("Décédé" = "#d55e00", "Guéri" = "#72ad81", "Inconnu" = "grey80",
                       "Abandon" = "#8585ad", "Transféré au CTE" = "#87c5c5",
                       "Retour à maison non-cas" = "#cccc99"),
               date_measure = "date_exit_eff",
               date_max = date_cut + 1) +
  geom_line(data = deaths_week, aes(x = week_exit + 3.5, y = pct * max_exits_week),
            colour = "#391313", linewidth = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . / max_exits_week, name = "% décédé",
                                         labels = scales::percent)) +
  labs(x = "Semaine de sortie", y = "Sorties") +
  theme(legend.position = "top", legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE))


##### Deaths by case status

ec_deaths <- cte |>
  filter(type_of_exit == "Décédé") |>
  mutate(evd_status = factor(evd_status, levels = c("Non cas", "Suspect", "Probable", "Confirmé"))) |>
  vis_epicurve(fill_by = "evd_status",
               pal = c("Confirmé" = "#b0364e", "Probable" = "#d27760",
                       "Suspect" = "#669999", "Non cas" = "#a4cd98"),
               date_measure = "date_exit_eff",
               date_max = report_date + 1) +
  labs(x = "Semaine de sortie", y = "Décès") +
  theme(legend.position = "top", legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE))


##### Diagnoses among non-cases

drop_dup_dx <- function(df) {
  same <- coalesce(df$diagnostic_secondary == df$diagnostic_principal, FALSE)
  df$diagnostic_secondary[same] <- NA_character_
  df
}

## Frequency table of diagnoses; variables without any value are skipped
dx_table <- function(df, vars) {
  vars <- vars[vapply(vars, function(v) any(!is.na(df[[v]])), logical(1))]
  if (length(vars) == 0 || nrow(df) == 0) return(NULL)
  df |>
    select(all_of(unname(vars))) |>
    mutate(across(everything(), as.character)) |>
    tbl_summary(type = everything() ~ "categorical",
                sort = everything() ~ "frequency",
                label = as.list(setNames(names(vars), vars))) |>
    format_tab() |>
    as_flex_table() |>
    border_inner_h(border = border_dotted)
}

dx_vars <- c("Diagnostic principal" = "diagnostic_principal",
             "Diagnostic secondaire" = "diagnostic_secondary",
             "Autre diagnostic" = "diagnostic_other_detail")

t_dx <- cte |>
  filter(evd_status == "Non cas") |>
  drop_dup_dx() |>
  dx_table(dx_vars[1:2])

dx_1w <- cte |>
  filter(evd_status %in% c("Suspect", "Non cas"), epiweek_exit %in% week1) |>
  drop_dup_dx()

t_dx_1w <- dx_table(dx_1w, dx_vars)

list_dx_1w <- dx_1w |>
  select(age, age_unit, sex, date_exit_eff,
         diagnostic_principal, diagnostic_secondary, diagnostic_other_detail) |>
  flextable() |>
  border_inner_h(border = border_dotted) |>
  set_header_labels(age = "Age", age_unit = "", sex = "Sexe",
                    date_exit_eff = "Date de sortie",
                    diagnostic_principal = "Diagnostic 1",
                    diagnostic_secondary = "Diagnostic 2",
                    diagnostic_other_detail = "Autre/details")

rm(drop_dup_dx, dx_table, dx_vars, dx_1w)


# Demographics ------------------------------------------------------------

##### Age / sex tables

t_agesex <- cte |>
  filter(!is.na(sex_f)) |>
  select(sex_f, age_group) |>
  tbl_summary(by = sex_f, percent = "column",
              label = age_group ~ "Tranche d'âge") |>
  format_tab_by2_header() |>
  as_flex_table() |>
  border_inner_h(border = border_dotted)

conf_3w <- cte |>
  filter(evd_status == "Confirmé", epiweek_adm %in% week3_vector)

t_agesex_week <- if (nrow(conf_3w) == 0) {
  empty_flextable("Aucun cas confirmé admis au cours des 3 dernières semaines")
} else {
  conf_3w |>
    mutate(across(c(pregnant, breastfeeding), ~ factor(.x, levels = c("Oui", "Non", "Inc.")))) |>
    select(sex_f, age_group, pregnant, breastfeeding, epiweek_adm) |>
    tbl_summary(by = epiweek_adm,
                label = list(sex_f ~ "Sexe", age_group ~ "Tranche d'âge",
                             pregnant ~ "Enceinte", breastfeeding ~ "Allaitante"),
                value = list(pregnant ~ "Oui", breastfeeding ~ "Oui"),
                statistic = c(pregnant, breastfeeding) ~ "{n}") |>
    format_tab() |>
    remove_row_type(variables = c(pregnant, breastfeeding), type = "missing") |>
    as_flex_table()
}


##### Age pyramids

pyramid <- function(df, subtitle) {
  df |>
    mutate(sex_f = factor(sex_f, levels = c("Féminin", "Masculin"))) |>
    plot_age_pyramid(age_group = "age_group", split_by = "sex_f") +
    labs(y = "Cas (n)", x = "Tranche d'âge (années)", subtitle = subtitle)
}

ap1 <- pyramid(filter(cte, evd_status == "Confirmé"), "Cas confirmés, cumul")
ap2 <- pyramid(cte, "Cumul admissions")
ap3 <- pyramid(filter(cte, evd_status == "Confirmé", rep_3w_adm == 1), "Cas confirmés, 3 semaines")
ap4 <- pyramid(filter(cte, rep_3w_adm == 1), "Cumul admissions, 3 semaines")

ap <- ap1 + ap2 + ap3 + ap4 +
  plot_layout(ncol = 2, guides = "collect", axis_titles = "collect")

rm(pyramid, conf_3w)


##### Proportion of children < 5 years over time

share_under5 <- function(df) {
  df |>
    filter(!is.na(age_group), !is.na(week_adm)) |>
    count(week_adm, age_group, .drop = FALSE) |>
    group_by(week_adm) |>
    mutate(pct = n / sum(n)) |>
    ungroup() |>
    filter(age_group == levels(age_group)[1])
}

age_week      <- share_under5(cte)
age_week_conf <- share_under5(filter(cte, evd_status == "Confirmé"))

g_age <- ggplot() +
  geom_line(data = age_week, aes(x = week_adm + 3.5, y = pct, colour = "Tous patients"), linewidth = 1) +
  geom_line(data = age_week_conf, aes(x = week_adm + 3.5, y = pct, colour = "Confirmé"), linewidth = 1) +
  scale_colour_manual(values = c("Confirmé" = "red", "Tous patients" = "black")) +
  scale_x_date(labels = label_date_short(), date_breaks = "2 weeks", date_minor_breaks = "1 week",
               limits = c(min(age_week$week_adm), max(age_week$week_adm) + 6)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Semaine d'admission", y = "% des patients <5 ans") +
  epicurve_theme +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.x = element_blank()) +
  guides(x = guide_axis(minor.ticks = TRUE))

rm(share_under5)


##### Pregnant and breastfeeding women

n_preg <- sum(cte$pregnant == "Oui", na.rm = TRUE)
n_lact <- sum(cte$breastfeeding == "Oui", na.rm = TRUE)


##### Origin of patients (health area)

as_table <- function(df, detail_outside = TRUE) {
  if (nrow(df) == 0) return(empty_flextable("Aucune admission sur la période"))
  df |>
    mutate(adm_print = case_when(
      is.na(adm2_name_res) ~ NA_character_,
      adm2_name_res == cfg$health_zone ~ adm3_name_res,
      detail_outside ~ paste0("ZS ", adm2_name_res, " - ", replace_na(adm3_name_res, "AS inconnue")),
      TRUE ~ paste0("ZS ", adm2_name_res)
    ),
    adm_print = replace_na(adm_print, "Inconnu"),
    evd_status = fct_drop(evd_status)) |>
    tabyl(adm_print, evd_status) |>
    adorn_totals(where = "col") |>
    adorn_percentages(denominator = "col") |>
    adorn_pct_formatting(digits = 0) |>
    adorn_ns(position = "front") |>
    flextable() |>
    set_header_labels(adm_print = "Aire de Santé") |>
    bold(i = 1, part = "header")
}

t_as       <- as_table(filter(cte, epiweek_adm %in% week1))
t_as_total <- as_table(cte, detail_outside = FALSE)

# Text for the report: origin of confirmed cases admitted this week
origin_conf_wk <- cte |>
  filter(evd_status == "Confirmé", epiweek_adm %in% week1) |>
  count(adm3_name_res, sort = TRUE) |>
  mutate(txt = paste0(replace_na(adm3_name_res, "Inconnu"), " (", n, ")")) |>
  pull(txt) |>
  paste(collapse = ", ")

rm(as_table)


# Epidemiological delays --------------------------------------------------

delays_los <- cte |>
  filter(rep_exit == 1, evd_status %in% c("Confirmé", "Non cas")) |>
  group_by(evd_status, type_of_exit3) |>
  summarise(med = median(los, na.rm = TRUE),
            q1 = quantile(los, 0.25, na.rm = TRUE),
            q3 = quantile(los, 0.75, na.rm = TRUE),
            .groups = "drop")

g_los_week <- cte |>
  filter(rep_exit == 1, !is.na(week_adm)) |>
  mutate(evd_status = if_else(evd_status == "Confirmé", "Confirmé", "Suspect")) |>
  group_by(week_adm, evd_status) |>
  summarise(los = mean(los, na.rm = TRUE), .groups = "drop") |>
  ggplot() +
  geom_line(aes(x = week_adm + 3.5, y = los, colour = evd_status), linewidth = 1) +
  scale_colour_manual(values = c("Confirmé" = "#b30000", "Suspect" = "cadetblue")) +
  scale_x_date(labels = label_date_short(), date_breaks = "2 weeks",
               limits = c(min(cte$week_adm, na.rm = TRUE) + 7, max(cte$week_adm, na.rm = TRUE) + 6)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Semaine d'admission", y = "Durée moyenne de séjour") +
  epicurve_theme +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.x = element_blank())

## Delays by outcome (jitter + boxplot)
noncas_exits <- c("Décédé", "Retour à maison non-cas", "Transféré autre structure (non cas)")
pal_outcome <- c("Décédé" = "#bc5c5c", "Guéri" = "#006600",
                 "Retour à maison non-cas" = "#006600",
                 "Transféré autre structure (non cas)" = "grey50")

delay_plot <- function(df, outcome, vars, ylim = NULL) {
  d <- df |>
    select(patient_site_id, evd_status, outcome = all_of(outcome), all_of(vars)) |>
    pivot_longer(cols = all_of(vars)) |>
    mutate(label = recode(name,
                          los = "Durée de séjour",
                          days_onset_adm = "DDS à l'admission au CTE",
                          days_onset_exit = "DDS à l'issue finale"))
  p <- ggplot(d, aes(x = outcome, y = value, colour = outcome)) +
    geom_jitter(position = position_jitter(seed = 1, width = 0.2, height = 0.2), alpha = 0.5) +
    geom_boxplot(width = 0.5, alpha = 0.2, outlier.shape = NA) +
    scale_colour_manual(values = pal_outcome, guide = "none") +
    facet_grid(evd_status ~ label) +
    epicurve_theme +
    labs(x = "Type d'issue", y = "Jours")
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}

conf_exits <- cte |> filter(rep_exit == 1, evd_status == "Confirmé", type_of_exit %in% c("Décédé", "Guéri"))
nc_exits   <- cte |> filter(rep_exit == 1, evd_status == "Non cas", type_of_exit3 %in% noncas_exits)

g_los <- delay_plot(conf_exits, "type_of_exit", "los", c(0, 25)) +
  delay_plot(nc_exits, "type_of_exit3", "los", c(0, 25)) +
  plot_layout(axis_titles = "collect")

g_timing <- delay_plot(conf_exits, "type_of_exit", c("days_onset_adm", "days_onset_exit")) +
  delay_plot(nc_exits, "type_of_exit3", c("days_onset_adm", "days_onset_exit"), c(0, 40)) +
  plot_layout(ncol = 1, axes = "collect")

rm(delay_plot, conf_exits, nc_exits, noncas_exits, exits_wk, is_conf)
