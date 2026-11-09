################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   8. DHIS2 line list: epidemic curves, transmission chains, Rt estimation
#
#   Standalone, exploratory script (not part of the routine sitrep): run it
#   interactively section by section for periodic checks of the DHIS2 export.
#
#   Extra packages: gghighlight, EpiEstim, epicontacts (GitHub, timeline
#   branch): see scripts/install_packages.R
#
#   Author: Stephanie Ngai (original), refactored by Aboubacar Hema
#   Last updated: 9 November 2026
#
################################################################################


# Setup -------------------------------------------------------------------

source(here::here("R", "01_setup.R"), encoding = "UTF-8")
source(here::here("R", "02_data_dhis.R"), encoding = "UTF-8")

# NB: if the DHIS2 export contains ALL case classifications, filter confirmed
# cases in 02_data_dhis.R before running this script.

pal_alert <- c("#a0c5a9", "#993333")


# Epidemic curves ---------------------------------------------------------

## By date of lab result / report (used in the sitrep)
ec_rep <- vis_epicurve(dhis_conf,
                       date_measure = "report_date",
                       date_min = min(dhis_conf$week_report, na.rm = TRUE),
                       date_max = date_cut + 1,
                       fill_by = "mve_n_nature_de_l_alerte", pal = pal_alert) +
  labs(x = "Semaine de prélèvement", y = "N. cas confirmé", caption = "Source: DHIS2") +
  theme(legend.position = "top", legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE))

ggsave(path_output("figures", paste0(format(Sys.Date(), "%Y%m%d"), "_epicurve.png")),
       plot = ec_rep, width = 12, height = 5, dpi = 300)

## By (estimated) symptom onset date
ec_sx <- vis_epicurve(dhis_conf, date_measure = "pdate", date_max = date_cut + 1,
                      fill_by = "mve_n_nature_de_l_alerte", pal = pal_alert) +
  labs(x = "Semaine de début des symptômes (estimation)", y = "N. cas confirmé",
       title = "Cas confirmés par nature de l'alerte, ZS Mongbwalu", caption = "Source: DHIS2") +
  theme(legend.position = "top", legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE))

## By report date and health area
ec_rep_as <- vis_epicurve(dhis_conf,
                          date_measure = "date_de_rapportage",
                          date_min = min(dhis_conf$week_report, na.rm = TRUE),
                          date_max = date_cut + 1,
                          fill_by = "mve_n_nature_de_l_alerte", pal = pal_alert) +
  facet_wrap(~orgunit) +
  scale_x_date(labels = label_date_short(), date_breaks = "3 weeks", date_minor_breaks = "1 week",
               limits = c(min(dhis_conf$week_report, na.rm = TRUE), date_cut + 1)) +
  labs(x = "Semaine de rapport", y = "N. cas confirmé",
       title = "Cas confirmés par Aire de Santé et nature de l'alerte, ZS Mongbwalu",
       caption = "Source: DHIS2") +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.y = element_line(linewidth = 0.2),
        axis.minor.ticks.length.x = rel(0.8),
        axis.minor.ticks.x.bottom = element_line(colour = "black")) +
  guides(fill = guide_legend(reverse = TRUE), x = guide_axis(minor.ticks = TRUE))

if (requireNamespace("gghighlight", quietly = TRUE)) {
  ec_rep_as_highlight <- ec_rep_as +
    gghighlight::gghighlight(unhighlighted_params = list(fill = "grey90"))
}


# Status at notification --------------------------------------------------

dhis_conf |>
  mutate(epiweek_report = epiweek_label(date_de_rapportage)) |>
  tabyl(mve_n_nature_de_l_alerte, epiweek_report) |>
  adorn_totals(where = c("col", "row")) |>
  select(mve_n_nature_de_l_alerte, any_of(week3_vector), Total)


# Summary by health area --------------------------------------------------

dhis_conf |>
  filter(date_de_rapportage <= date_cut) |>
  group_by(orgunit) |>
  summarise(cases_total = n(),
            cases_3w = sum(date_de_rapportage >= date_start_3w, na.rm = TRUE),
            cases_4w = sum(date_de_rapportage >= date_cut - 27, na.rm = TRUE))


# Export for chainchecker / Go.Data ---------------------------------------

dhis_ll <- dhis_conf |>
  rename(date_sx = mve_dds_date_de_debut_des_symptomes,
         infector_id = x127c_mve_s4_ma_1_si_cas_ebola_numero_didentification,
         id = mve_n_numero_d_identification_de_l_alerte) |>
  mutate(pdate = case_when(
    # Deaths: onset assumed 10 days before report when onset is missing/implausible
    mve_n_nature_de_l_alerte == "Décès" & !is.na(date_sx) & date_de_rapportage - date_sx < 4 ~ date_de_rapportage - 10,
    mve_n_nature_de_l_alerte == "Décès" & !is.na(date_sx) & date_de_rapportage - date_sx > 20 ~ date_de_rapportage - 10,
    mve_n_nature_de_l_alerte == "Décès" & is.na(date_sx) ~ date_de_rapportage - 10,
    !is.na(date_sx) ~ date_sx,
    TRUE ~ date_de_rapportage  # very biased fallback
  ))

dhis_exp <- dhis_ll |>
  select(id, date_de_rapportage, mve_n_sexe, mve_age_ans, mve_age_mois,
         organisation_unit_name, mve_n_nature_de_l_alerte, mve_lien_epidemiologique,
         x108_mve_s4_ma_1_lien_de_parente, infector_id,
         x199_mve_s6_statut_final_du_patient, pdate, report_date, date_sx, orgunit)

write_xlsx_table(dhis_exp, path_output("chainchecker", "ll_dhis2.xlsx"))


# Transmission chains -----------------------------------------------------

table(dhis_conf$mve_lien_epidemiologique, useNA = "always")

dhis_conf |>
  filter(semaine_result > 36) |>
  mutate(mve_lien_epidemiologique = replace_na(mve_lien_epidemiologique, "Ne sait pas")) |>
  select(orgunit, mve_lien_epidemiologique) |>
  tbl_summary(by = mve_lien_epidemiologique, percent = "row") |>
  format_tab_by2_header()

if (requireNamespace("epicontacts", quietly = TRUE)) {
  ec_ll <- dhis_ll |>
    select(id, everything()) |>
    filter(semaine_result > 36)

  contacts <- ec_ll |>
    filter(!is.na(infector_id)) |>
    select(from = infector_id, to = id) |>
    mutate(transmission_type = "Inconnu")

  ec <- epicontacts::make_epicontacts(linelist = ec_ll, contacts = contacts, directed = TRUE)

  plot(ec,
       x_axis = "pdate", node_color = "orgunit",
       node_shape = "mve_lien_epidemiologique",
       shapes = c("Oui" = "plus", "Non" = "circle", "Ne sait pas" = "circle-o"),
       node_size = 20, label = FALSE, parent_pos = "top", position_dodge = TRUE,
       root_order = "orgunit", date_labels = "%d-%b-%Y", thin = FALSE,
       arrow_size = 0.5, height = 1200)
}


# Rough Rt estimation (EpiEstim) ------------------------------------------
#  Parametric serial interval from the literature (cfg$serial_interval).
#  Daily incidence by report date: interpret with caution (reporting delays).

cases_incidence <- dhis_conf |>
  filter(!is.na(date_de_rapportage), date_de_rapportage <= date_cut) |>
  count(dates = date_de_rapportage, name = "I") |>
  complete(dates = seq.Date(min(dates), date_cut, by = "day"), fill = list(I = 0))

rt_from_epiestim <- function(res) {
  res$R |>
    as_tibble() |>
    mutate(date = res$dates[t_end])
}

si_config <- function(t_start, t_end) {
  EpiEstim::make_config(mean_si = cfg$serial_interval$mean,
                        std_si = cfg$serial_interval$sd,
                        t_start = t_start, t_end = t_end)
}

## 1-week sliding window from the start of the outbreak period
t_start_1w <- seq(as.integer(cfg$rt_start_date - min(cases_incidence$dates)) + 1,
                  nrow(cases_incidence) - 6)
rt_1w <- EpiEstim::estimate_R(cases_incidence, method = "parametric_si",
                              config = si_config(t_start_1w, t_start_1w + 6)) |>
  rt_from_epiestim()

## 2-week sliding window
t_start_2w <- seq(2, nrow(cases_incidence) - 13)
rt_2w <- EpiEstim::estimate_R(cases_incidence, method = "parametric_si",
                              config = si_config(t_start_2w, t_start_2w + 13)) |>
  rt_from_epiestim()

g_rt <- ggplot(rt_2w, aes(x = date, y = `Median(R)`)) +
  geom_ribbon(aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), alpha = 0.2, fill = "grey60") +
  geom_line(colour = "darkred") +
  geom_hline(yintercept = 1, linetype = "longdash") +
  scale_x_date(labels = label_date_short(), date_breaks = "1 week",
               limits = c(min(dhis_conf$week_report, na.rm = TRUE), date_cut + 1)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Date de rapportage", y = "R médian (fenêtre de 14 jours)") +
  epicurve_theme +
  theme(panel.grid.major.x = element_blank())

combined <- (ec_rep + labs(title = "Cas confirmés de la MVE par date de rapportage et taux de reproduction",
                           subtitle = "Zone de Santé de Mongbwalu")) +
  g_rt +
  plot_layout(ncol = 1, axis_titles = "collect_x")

combined
