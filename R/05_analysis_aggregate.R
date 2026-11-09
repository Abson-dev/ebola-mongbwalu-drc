################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   5. Surveillance (alerts) indicators for the weekly sitrep
#
#   Requires: 01_setup.R, 02_data_daily.R
#   Output  : pct_alerts_v, pct_alerts_ref, t_alerts, g_alerts, g_alerts_as2,
#             g_alerts_as_rate
#
#   Author: Stephanie Ngai (original), refactored by Aboubacar Hema
#   Last updated: 9 November 2026
#
################################################################################


# Headline figures --------------------------------------------------------

alerts_week1 <- alerts |>
  filter(epiweek_alert == week1) |>
  summarise(across(c(alertes_vivants, vivants_validees, cas_suspect_cte), ~ sum(.x, na.rm = TRUE)))

# Share of live alerts validated, and of validated alerts referred to the CTE
pct_alerts_v   <- alerts_week1$vivants_validees / alerts_week1$alertes_vivants
pct_alerts_ref <- alerts_week1$cas_suspect_cte / alerts_week1$vivants_validees


# Table: alerts over the last 3 weeks + cumulative ------------------------

alert_vars <- c(
  `Alertes vivants`                    = "alertes_vivants",
  `Alertes validées chez vivants`      = "vivants_validees",
  `Acheminée au CTE`                   = "cas_suspect_cte",
  `Refus ou incapacité d'aller au CTE` = "cas_suspects_non_transferes",
  `Alertes décédés`                    = "alertes_deces"
)

alerts_long <- alerts |>
  select(epiweek_alert, all_of(alert_vars)) |>
  pivot_longer(-epiweek_alert, names_to = "Indicateur") |>
  mutate(Indicateur = factor(Indicateur, levels = names(alert_vars)))

alerts_tab <- alerts_long |>
  filter(epiweek_alert %in% week3_vector) |>
  group_by(Indicateur, epiweek_alert) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = epiweek_alert, values_from = value) |>
  left_join(alerts_long |>
              group_by(Indicateur) |>
              summarise(Cumul = sum(value, na.rm = TRUE), .groups = "drop"),
            by = "Indicateur") |>
  select(Indicateur, any_of(week3_vector), Cumul) |>
  mutate(across(-Indicateur, ~ format(.x, big.mark = " ")),
         # Referral indicators are only meaningful weekly
         Cumul = if_else(Indicateur %in% c("Acheminée au CTE", "Refus ou incapacité d'aller au CTE"),
                         "", Cumul))

t_alerts <- alerts_tab |>
  flextable() |>
  set_header_labels(Indicateur = "") |>
  border_inner_h(border = border_dotted) |>
  bold(i = c(1, 2, 5), j = 1, part = "body") |>
  bg(i = c(1, 2, 5), part = "body", bg = "grey90") |>
  bold(part = "header")

rm(alert_vars, alerts_long, alerts_tab)


# Figures -----------------------------------------------------------------

pal_alerts <- c("Vivant" = "cadetblue", "Décédé" = "#C04F15", "Total" = "black")

g_alerts <- alerts_week |>
  mutate(name = factor(name, levels = c("Total", "Vivant", "Décédé"))) |>
  ggplot() +
  geom_line(aes(x = epiweek_alert, y = value, group = name, colour = name), linewidth = 1) +
  scale_colour_manual(values = pal_alerts) +
  labs(x = "Semaine", y = "N. alertes") +
  epicurve_theme +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top", legend.justification = "left",
        axis.text.x = element_text(angle = 45, hjust = 1))

g_alerts_as2 <- alerts_week_as |>
  filter(name %in% c("Vivant", "Décédé")) |>
  mutate(name = factor(name, levels = c("Vivant", "Décédé"))) |>
  ggplot() +
  geom_col(aes(x = week, y = value, fill = name)) +
  scale_fill_manual(values = pal_alerts) +
  facet_wrap(~as) +
  scale_x_date(labels = label_date_short(), date_breaks = "2 weeks") +
  labs(x = "Semaine", y = "N. alertes") +
  epicurve_theme +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top", legend.justification = "left")

g_alerts_as_rate <- alerts_week_as_rate |>
  ggplot() +
  geom_line(aes(x = week, y = rate), linewidth = 0.5) +
  facet_wrap(~as) +
  scale_x_date(labels = label_date_short(), date_breaks = "2 weeks") +
  labs(x = "Semaine", y = "Alertes / 10 000 pop") +
  epicurve_theme +
  theme(panel.grid.major.x = element_blank())
