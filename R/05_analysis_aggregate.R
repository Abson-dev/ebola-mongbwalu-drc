################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   5. Aggregate data summary
#
#   Author: Stephanie Ngai
#   Last updated: 21 September 2026
#
################################################



# Alerts ------------------------------------------------------------------

##### Overall figures for text

# Percent of vivants validées

alerts_week1 <- alerts %>%
  filter(epiweek_alert == week1) %>%
  summarise(across(c(alertes_vivants, vivants_validees, cas_suspect_cte), sum, na.rm = TRUE))

pct_alerts_v <- alerts_week1$vivants_validees/alerts_week1$alertes_vivants

pct_alerts_ref <- alerts_week1$cas_suspect_cte/alerts_week1$vivants_validees


# Percent oriented to CTE



##### Table

# Base table
t_alerts <- 
  alerts %>%
  select(epiweek_alert, 
         `Alertes vivants` = alertes_vivants,
         `Alertes validées chez vivants`= vivants_validees, 
         `Acheminée au CTE` = cas_suspect_cte, 
         `Refus ou incapacité d'aller au CTE` = cas_suspects_non_transferes,
         `Alertes décédés` = alertes_deces) %>%
  tbl_summary(
    by = epiweek_alert,
    type = everything() ~ "continuous", 
    statistic = list(all_continuous() ~ "{sum}"),
    missing = "no",
    digits = everything() ~ 0
  ) %>%
  add_overall(last = TRUE) %>%
  modify_header(all_stat_cols() ~ "**{level}**",
                stat_0 = "**Cumul**") %>%
  remove_footnote_header() %>%
  modify_table_body(
    ~ .x %>%
      mutate(stat_0 = case_when(variable %in% c("Acheminée au CTE",
                                                "Refus ou incapacité d'aller au CTE") ~ "",
                                TRUE ~ stat_0))
  )


# Get only most recent 3 weeks
alerts_cols <- names(t_alerts$table_body) %>% as.data.frame() %>%
  clean_names() %>%
  filter(grepl("stat_", x)) %>%
  filter(x != "stat_0") %>%
  slice_head(n = nrow(.)-3) %>%
  pull(x)

# Select columns and format
t_alerts <- t_alerts %>%
  modify_column_hide(alerts_cols) %>%
  as_flex_table() %>% border_inner_h(border = border_dotted) %>%
  bold(i = c(1, 2, 5), j = 1, part = "body") %>%
  bg(i = c(1, 2, 5), part = "body", bg = "grey90")


##### Curve of alerts

# "#156082"

g_alerts <- 
  alerts_week %>%
  mutate(name = factor(name, levels = c("Total", "Vivant", "Décédé"))) %>%
  ggplot() +
  geom_line(aes(x = epiweek_alert, y = value, group = name, color = name),
            linewidth = 1) +
  scale_color_manual(values = c("Vivant" = "cadetblue", "Décédé" = "#C04F15", "Total" = "black")) + 
  labs(x = "Semaine", y = "N. alertes") +
  epicurve_theme +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top", legend.justification = "left",
        axis.text.x = element_text(angle = 45, hjust = 1))

# g_alerts_as <-
#   alerts_week_as %>%
#   mutate(name = factor(name, levels = c("Total", "Vivant", "Décédé"))) %>%
#   ggplot() +
#   geom_line(aes(x = week, y = value, group = name, color = name),
#             linewidth = 1) +
#   scale_color_manual(values = c("Vivant" = "cadetblue", "Décédé" = "#C04F15", "Total" = "black")) + 
#   facet_wrap(~as) +
#   scale_x_date(labels = label_date_short(), date_breaks = "2 weeks") +
#   labs(x = "Semaine", y = "N. alertes") +
#   epicurve_theme +
#   theme(panel.grid.major.x = element_blank(),
#         legend.position = "top", legend.justification = "left")

# max_alerts_week <- plyr::round_any(max(alerts_week_as$value), 10, f = ceiling)

g_alerts_as2 <-
  alerts_week_as %>%
  filter(name %in% c("Vivant", "Décédé")) %>%
  mutate(name = factor(name, levels = c("Vivant", "Décédé"))) %>%
  ggplot() +
  geom_col(aes(x = week, y = value, group = name, fill = name)) +
  scale_fill_manual(values = c("Vivant" = "cadetblue", "Décédé" = "#C04F15")) + 
  # geom_line(data = alerts_week_as_rate, aes(x = week, y = rate)) +
  # scale_y_continuous(limits = c(0, max_alerts_week),
  #                    sec.axis = sec_axis(~.,
  #                                        name = "Alertes / 10 000 pop")) +
  facet_wrap(~as) +
  scale_x_date(labels = label_date_short(), date_breaks = "2 weeks") +
  labs(x = "Semaine", y = "N. alertes") +
  epicurve_theme +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top", legend.justification = "left")

g_alerts_as_rate <- alerts_week_as_rate %>%
  ggplot() +
  geom_line(aes(x = week, y = rate), linewidth = 0.5) +
  facet_wrap(~as) +
  scale_x_date(labels = label_date_short(), date_breaks = "2 weeks") +
  labs(x = "Semaine", y = "Alertes / 10 000 pop") +
  epicurve_theme +
  theme(panel.grid.major.x = element_blank())




