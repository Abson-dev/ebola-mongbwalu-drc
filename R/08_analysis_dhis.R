################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   8a. DHIS2 linelist - epi curves, Rt estimation, transmission chains, etc
#
#   Author: Stephanie Ngai
#   Last updated: 27 October 2026
#
################################################

# This script has a messy bunch of ad hoc visualizations, tables, and analyses
# I was not using it for routine reporting, but rather for semi-regular checks on the
# linelist exported from DHIS2


##### Source files

source(here::here("R", "01_setup.R"))
source(here::here("R", "02_data_dhis.R"))


##### Need to check 02_data_dhis.R
#     If exported linelist has ALL case status, need to add in line to filter only confirmed


# Packages ----------------------------------------------------------------

# Epi curves

pacman::p_load(
  gghighlight()
)


# Transmission chain

remotes::install_github('reconhub/epicontacts@timeline')

library(epicontacts)


# Transmission dynamics

pacman::p_load(
  EpiNow2,      # Rt estimation
  EpiEstim,     # Rt estimation
  projections,  # Incidence projections
  incidence2,   # Handling incidence data
  epitrix,      # Useful epi functions
  distcrete     # Discrete delay distributions
)




# Epi curves --------------------------------------------------------------


# By date of lab result or report
# To include in sitrep
# Need to make sure DHIS2 linelist export is up to date!

ec_rep <- 
  vis_epicurve(dhis_conf,
               date_measure = "report_date",
               # date_measure = "date_de_rapportage",
               date_min = min(dhis_conf$week_report),
               date_max = date_cut+1,
               fill_by = "mve_n_nature_de_l_alerte", pal = c("#a0c5a9", "#993333")) +
  labs(x = "Semaine de prélèvement", y = "N. cas confirmé",
       # title = "Cas confirmés par nature de l'alerte, ZS Mongbwalu",
       caption = "Source: DHIS2") +
  theme(legend.position = "top", legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE))


ggsave(
  paste0(format(Sys.Date(), "%Y%m%d"), "_epicurve.png"),
  plot = ec_rep,
  path = here::here("output", "figures"),
  width = 12, height = 5
)


# By symptom onset date

ec_sx <- 
  vis_epicurve(dhis_conf,
             date_max = date_cut+1,
             fill_by = "mve_n_nature_de_l_alerte", pal = c("#a0c5a9", "#993333")) +
  labs(x = "Semaine de début des symptômes (estimation)", y = "N. cas confirmé",
       title = "Cas confirmés par nature de l'alerte, ZS Mongbwalu",
       caption = "Source: DHIS2") +
  theme(legend.position = "top", legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE))


# By date of report and AS

ec_rep_as <- 
  vis_epicurve(dhis_conf,
             date_measure = "date_de_rapportage",
             date_min = min(dhis_conf$week_report),
             date_max = date_cut+1,
             fill_by = "mve_n_nature_de_l_alerte", pal = c("#a0c5a9", "#993333")) +
  facet_wrap(~orgunit) +
  scale_x_date(labels = label_date_short(), date_breaks = "3 weeks", date_minor_breaks = "1 week", 
               limits = c(min(dhis_conf$week_report), date_cut+1)) +
  labs(x = "Semaine de rapport", y = "N. cas confirmé",
       title = "Cas confirmés par Aire de Santé et nature de l'alerte, ZS Mongbwalu",
       caption = "Source: DHIS2") +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.y = element_line(linewidth = 0.2),
        axis.minor.ticks.length.x = rel(0.8), axis.minor.ticks.x.bottom = element_line(colour = 'black')) +
  guides(fill = guide_legend(reverse = TRUE),
         x = guide_axis(minor.ticks = TRUE))


# With highlight

ec_rep_as_highlight <- ec_rep_as +
  gghighlight::gghighlight(
    unhighlighted_params = list(fill = "grey90")
  )



# 
# ggsave(
#   paste0(format(Sys.Date(), "%Y%m%d"), "_epicurve_AS.png"),
#   plot = ec_rep_as,
#   path = here::here("output", "figures"),
#   width = 12, height = 8
# )
# 
# ggsave(
#   paste0(format(Sys.Date(), "%Y%m%d"), "_epicurve_AS_highlight.png"),
#   plot = ec_rep_as_highlight,
#   path = here::here("output", "figures"),
#   width = 12, height = 8
# )


## With ZS facet

# CreateAllFacet <- function(df, col){
#   df$facet <- df[[col]]
#   temp <- df
#   temp$facet <- "all"
#   merged <-rbind(temp, df)
#   
#   # ensure the facet value is a factor
#   merged[[col]] <- as.factor(merged[[col]])
#   
#   return(merged)
# }
# 
# dhis_facet <- CreateAllFacet(dhis_conf, col = "orgunit") %>%
#   mutate(facet = fct_relevel(facet, "all", after = Inf))
# 
# 
# ec_rep_as_v2 <- 
#   vis_epicurve(dhis_facet,
#                date_measure = "date_de_rapportage",
#                date_min = min(dhis_conf$week_report),
#                date_max = date_cut+1,
#                fill_by = "mve_n_nature_de_l_alerte", pal = c("#a0c5a9", "#993333")) +
#   facet_wrap(~facet) +
#   scale_x_date(labels = label_date_short(), date_breaks = "3 weeks", date_minor_breaks = "1 week", 
#                limits = c(min(dhis_conf$week_report), date_cut+1)) +
#   labs(x = "Semaine de rapport", y = "N. cas confirmé",
#        title = "Cas confirmés par Aire de Santé et nature de l'alerte, ZS Mongbwalu",
#        caption = "Source: DHIS2") +
#   theme(legend.position = "top", legend.justification = "left",
#         panel.grid.major.y = element_line(linewidth = 0.2),
#         axis.minor.ticks.length.x = rel(0.8), axis.minor.ticks.x.bottom = element_line(colour = 'black')) +
#   guides(fill = guide_legend(reverse = TRUE),
#          x = guide_axis(minor.ticks = TRUE))





# Status at notification --------------------------------------------------

dhis_conf %>%
  mutate(epiweek_report = date2week(date_de_rapportage, factor = TRUE),
         epiweek_report = str_replace(epiweek_report, "W", "S")) %>%
  tabyl(mve_n_nature_de_l_alerte, epiweek_report) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  select(mve_n_nature_de_l_alerte, any_of(week3_vector), Total)




# Summary table by AS -----------------------------------------------------

# date_3w <- as.Date("2026-09-27")-20

dhis_conf %>%
  filter(date_de_rapportage <= date_cut) %>%
  mutate(epiweek_report = date2week(date_de_rapportage, factor = TRUE),
         rep_3w = case_when(date_de_rapportage >= date_start_3w ~ 1,
                            TRUE ~ 0)) %>%
  group_by(orgunit) %>%
  summarise(cases_total = n(),
            cases_3w = sum(rep_3w),
            cases_4w = sum(date_de_rapportage >= date_cut-27, na.rm = TRUE))

# date_cut-27




# Output data for chainchecker/Go.Data ------------------------------------

##### Calculate estimated symptom onset

dhis_exp <- dhis_conf %>%
  rename(date_sx = mve_dds_date_de_debut_des_symptomes,
         infector_id = x127c_mve_s4_ma_1_si_cas_ebola_numero_didentification,
         id = mve_n_numero_d_identification_de_l_alerte) %>%
  mutate(pdate = case_when(
    # Deaths
    mve_n_nature_de_l_alerte == "Décès" & !is.na(date_sx) & date_de_rapportage-date_sx < 4 ~ date_de_rapportage-10,
    mve_n_nature_de_l_alerte == "Décès" & !is.na(date_sx) & date_de_rapportage-date_sx >20 ~ date_de_rapportage-10,
    mve_n_nature_de_l_alerte == "Décès" & is.na(date_sx) ~ date_de_rapportage-10,
    # Non missing sx onset
    !is.na(date_sx) ~ date_sx,
    # Otherwise report date (very biased!)
    TRUE ~ date_de_rapportage
  )) %>%
  select(id, date_de_rapportage, mve_n_sexe, mve_age_ans, mve_age_mois,
         organisation_unit_name, mve_n_nature_de_l_alerte, mve_lien_epidemiologique,
         x108_mve_s4_ma_1_lien_de_parente, infector_id,
         x199_mve_s6_statut_final_du_patient, pdate, report_date, date_sx, orgunit)


##### Export to chainchecker folder
write.xlsx(
  dhis_exp,
  file = here::here("chainchecker-v1.4.5", "ll_dhis2.xlsx")
)




# Transmission chain ------------------------------------------------------

table(dhis_conf$mve_lien_epidemiologique, useNA = "always")

##### Epi links

dhis_conf %>%
  filter(semaine_result > 36) %>%
  select(orgunit, mve_lien_epidemiologique) %>%
  mutate(mve_lien_epidemiologique = replace_na(mve_lien_epidemiologique, "Ne sait pas")) %>%
  tbl_summary(
    by = mve_lien_epidemiologique,
    percent = "row"
  ) %>%
  format_tab_by2_header()

dhis_conf %>%
  tabyl(orgunit, mve_lien_epidemiologique)




##### Check types of links - very messy

table(dhis_conf$x108_mve_s4_ma_1_lien_de_parente)


##### Create epicontacts object

# Linelist

ec_ll <- dhis_conf %>%
  rename(date_sx = mve_dds_date_de_debut_des_symptomes,
         infector_id = x127c_mve_s4_ma_1_si_cas_ebola_numero_didentification,
         id = mve_n_numero_d_identification_de_l_alerte) %>%
  mutate(pdate = case_when(
    # Deaths
    mve_n_nature_de_l_alerte == "Décès" & !is.na(date_sx) & date_de_rapportage-date_sx < 4 ~ date_de_rapportage-10,
    mve_n_nature_de_l_alerte == "Décès" & !is.na(date_sx) & date_de_rapportage-date_sx >20 ~ date_de_rapportage-10,
    mve_n_nature_de_l_alerte == "Décès" & is.na(date_sx) ~ date_de_rapportage-10,
    # Non missing sx onset
    !is.na(date_sx) ~ date_sx,
    # Otherwise report date (very biased!)
    TRUE ~ date_de_rapportage
  )) %>%
  select(id, everything())

# Filter to only recent cases
ec_ll <- ec_ll %>%
  filter(semaine_result > 36)


# Contacts

contacts <- ec_ll %>%
  filter(!is.na(infector_id)) %>%
  select(from = infector_id, to = id) %>%
  mutate(transmission_type = "Inconnu")


# Objects
ec <- make_epicontacts(linelist = ec_ll,
                       contacts = contacts,
                       # id = "id", from = "infector_id", to = "id",
                       directed = TRUE)


##### Interactive chain

plot(
  ec,
  x_axis = "pdate",
  # x_axis = "date_sx",
  # x_axis = "date_de_rapportage",
  node_color = "orgunit",
  # col_pal = node_pal,
  node_shape = "mve_lien_epidemiologique",
  shapes = c(
    "Oui" = "plus",
    "Non" = "circle",
    "Ne sait pas" = "circle-o"
  ),
  node_size = 20,
  label = FALSE,
  # node_label = "id",
  # edge_linetype = "transmission_type",
  # network_shape = "rectangle",
  parent_pos = "top",
  position_dodge = TRUE,
  # reverse_root_order = TRUE,
  root_order = "orgunit",
  date_labels = "%d-%b-%Y",
  # method = "ggplot",
  thin = FALSE,
  # legend_width = 0.1,
  arrow_size = 0.5
  ,
  height = 1200
)










# Very rough Rt estimations -----------------------------------------------

## Prep data ---------------------------------------------------------------

## Cases by day

cases <- incidence2::incidence(dhis_conf, date_index = "date_de_rapportage") %>% # get case counts by day
  tidyr::complete(date_index = seq.Date(                              # ensure all dates are represented
    from = min(date_index, na.rm = T),
    # to = max(date_index, na.rm=T),
    to = date_cut,
    by = "day"),
    fill = list(count = 0)) %>%                                       # convert NA counts to 0
  rename(I = count,                                                   # rename to names expected by estimateR
         dates = date_index)


cases <- cases %>% 
  filter(!is.na(dates))


#create a dataframe for the function estimate_R()
cases_incidence <- data.frame(dates = seq.Date(from = min(cases$dates),
                                               to = max(cases$dates), 
                                               by = 1))

cases_incidence <- left_join(cases_incidence, cases) %>% 
  select(dates, I) %>% 
  mutate(I = ifelse(is.na(I), 0, I))



## Calculate ---------------------------------------------------------------


## Serial interval estimation
#   Based on Beni

# config_lit <- make_config(
#   mean_si = 11.61,
#   std_si = 5.2
# )
# 
# epiestim_res_lit <- estimate_R(
#   incid = cases_incidence,
#   method = "parametric_si",
#   config = config_lit
# )
# 
# plot(epiestim_res_lit)
# 
# 
# 
# rt0_dates <- epiestim_res_lit$dates[-(1:(length(epiestim_res_lit$dates) - nrow(epiestim_res_lit$R)))]
# rt0 <- epiestim_res_lit$R %>%
#   bind_cols(date = rt0_dates)
# 
# ggplot(rt0, aes(x = date, y = `Median(R)`)) +
#   geom_line(color = "black") +
#   geom_ribbon(aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), alpha = 0.2, fill = "grey60") +
#   scale_x_date(labels = label_date_short(), date_breaks =  "1 week",
#                limits = c(min(dhis_conf$week_report), date_cut+1)) +
#   scale_y_continuous(limits = c(0, NA)) +
#   labs(x = "Date de rapportage", y = "Median R") +
#   epicurve_theme +
#   theme(panel.grid.major.x = element_blank())






## Partial window

start_dates <- seq.Date(
  as.Date("2026-06-01"),
  max(cases_incidence$dates) - 7,
  by = 1
) %>%
  ## subtract the starting date to convert to numeric
  `-`(min(cases$dates)) %>%
  ## convert to integer
  as.integer()

## add six days for a one week sliding window
end_dates <- start_dates + 6

## make config
config_partial <- make_config(
  mean_si = 11.61,
  std_si = 5.2,
  t_start = start_dates,
  t_end = end_dates
)


## run epiestim
epiestim_res_partial <- estimate_R(
  incid = cases_incidence,
  method = "parametric_si",
  config = config_partial
)

## plot outputs
plot(epiestim_res_partial, legend = FALSE)




rt_dates <- epiestim_res_partial$dates[-(1:(length(epiestim_res_partial$dates) - nrow(epiestim_res_partial$R)))]
rt <- epiestim_res_partial$R %>%
  bind_cols(date = rt_dates)

ggplot(rt, aes(x = date, y = `Median(R)`)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), alpha = 0.2, fill = "grey60") +
  scale_x_date(labels = label_date_short(), date_breaks =  "1 week",
               limits = c(min(dhis_conf$week_report), date_cut+1)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Date de rapportage", y = "Median R") +
  epicurve_theme +
  theme(panel.grid.major.x = element_blank())






#### 3 week

# t_start` is a sequence of start days for the sliding window, starting on day 2
# ending 3 weeks before the last day in the dataset:
t_start <- seq(2, nrow(cases_incidence) - 13) 

# `t_end` is a sequence of end dates for the window 3 weeks after each start
# date defined above:
t_end <- t_start + 13

R_window <- estimate_R(incid = cases_incidence, 
                       method = "parametric_si",
                       config = make_config(mean_si = 11.61,
                                            std_si = 5.2,
                                            t_start = t_start,
                                            t_end = t_end))

plot(R_window, legend = FALSE, "R")


rt3_dates <- R_window$dates[-(1:(length(R_window$dates) - nrow(R_window$R)))]
rt3 <- R_window$R %>%
  bind_cols(date = rt3_dates)


g_rt3 <- 
  ggplot(rt3, aes(x = date, y = `Median(R)`)) +
  geom_line(color = "darkred") +
  geom_hline(yintercept = 1, linetype = "longdash") +
  geom_ribbon(aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), alpha = 0.2, fill = "grey60") +
  scale_x_date(labels = label_date_short(), date_breaks =  "1 week",
               limits = c(min(dhis_conf$week_report), date_cut+1)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Date de rapportage", y = "Median R") +
  epicurve_theme +
  theme(panel.grid.major.x = element_blank())


combined <- 
  (ec_rep + labs(title = "Cas confirmés de la MVE par date de rapportage et taux de reproduction (median 2 semaines)",
                 subtitle = "Zone de Santé de Mongbwalu")) + g_rt3 +
  plot_layout(ncol = 1, axis_titles = "collect_x")

combined


