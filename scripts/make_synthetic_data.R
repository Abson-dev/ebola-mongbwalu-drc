################################################################################
#
#   Generate a fully SYNTHETIC data set with the structure of the real inputs
#
#   No real patient data is used: every value is drawn at random. The files
#   reproduce the sheet names and column names expected by the pipeline, so
#   that the code can be tested, demonstrated and run in CI safely.
#
#   Usage:
#     Rscript scripts/make_synthetic_data.R [output_dir] [n_patients]
#     -> output_dir defaults to data-synthetic/ (git-ignored)
#
#   Then run the pipeline on it:
#     Sys.setenv(EBOLA_DATA_DIR = "data-synthetic")
#
################################################################################

make_synthetic_data <- function(out_dir = "data-synthetic", n = 600, seed = 20261109,
                                start = as.Date("2026-05-15"), end = as.Date("2026-11-08")) {
  set.seed(seed)
  for (d in c("linelist", "daily", "population")) {
    dir.create(file.path(out_dir, d), recursive = TRUE, showWarnings = FALSE)
  }

  pick <- function(x, size = n, prob = NULL) sample(x, size, replace = TRUE, prob = prob)
  yn <- function(p_yes, size = n) pick(c("Oui", "Non", "Inc.", NA), size, prob = c(p_yes, 0.9 - p_yes, 0.02, 0.08))

  # ---- Admissions ------------------------------------------------------------
  days <- as.numeric(end - start)
  # Epidemic shape: rise then decline
  t <- round(stats::rbeta(n, 1.8, 1.5) * days)
  date_adm <- sort(start + t)
  delay <- pmin(stats::rnbinom(n, mu = 4, size = 2), 30)
  onset <- date_adm - delay
  onset[sample(n, round(n * 0.06))] <- NA

  status <- pick(c("Confirmé", "Non cas", "Suspect", "Probable"), prob = c(0.30, 0.62, 0.07, 0.01))
  # Recent suspects are still waiting for results
  status[date_adm < end - 14 & status == "Suspect"] <- "Non cas"

  age_unit <- pick(c("Ans", "Mois", "Jour"), prob = c(0.91, 0.085, 0.005))
  age <- ifelse(age_unit == "Ans", pmax(1, round(stats::rgamma(n, 2.2, 0.07))),
                ifelse(age_unit == "Mois", sample(1:11, n, TRUE), sample(1:28, n, TRUE)))
  sex <- pick(c("M", "F"), prob = c(0.56, 0.44))

  doa <- ifelse(stats::runif(n) < 0.02, "Oui", NA)

  # ---- Outcomes --------------------------------------------------------------
  los <- pmax(0, round(stats::rgamma(n, 1.6, 0.45)))
  exit_type <- character(n)
  conf <- status == "Confirmé"
  exit_type[conf] <- pick(c("Décédé", "Guéri", "Abandon", "Inconnu"), sum(conf), prob = c(0.58, 0.35, 0.05, 0.02))
  exit_type[status == "Non cas"] <- pick(c("Retour a la maison (non cas)", "Transféré autre structure (non cas)",
                                           "Décédé", "Abandon"), sum(status == "Non cas"),
                                         prob = c(0.66, 0.28, 0.04, 0.02))
  exit_type[status == "Suspect"] <- pick(c("Décédé", "Abandon", NA), sum(status == "Suspect"), prob = c(0.3, 0.3, 0.4))
  exit_type[status == "Probable"] <- "Décédé"
  exit_type[exit_type == "Inconnu"] <- NA
  exit_type[!is.na(doa)] <- "Décédé"
  los[!is.na(doa)] <- 0
  date_exit <- date_adm + los
  still_in <- date_exit > end | is.na(exit_type)
  exit_type[still_in] <- NA
  date_exit[still_in] <- NA

  # ---- Geography -------------------------------------------------------------
  as_mgl <- c("Mongbwalu", "Ceca 20", "Saïo", "NDCM", "Pluto", "Lodjo", "Pilipili", "Dala", "Abelkozo", "Yedi")
  zs <- pick(c("Mongbwalu", "Bambu", "Damas", "Kilo"), prob = c(0.93, 0.035, 0.02, 0.015))
  as_res <- ifelse(zs == "Mongbwalu",
                   pick(as_mgl, prob = c(.26, .20, .15, .14, .12, .04, .03, .03, .02, .01)),
                   paste(zs, "Centre"))

  # ---- Laboratory ------------------------------------------------------------
  res1 <- ifelse(conf, "Positif", "Négatif")
  res1[status == "Suspect" & date_adm > end - 2] <- NA
  res1[status == "Probable"] <- NA
  res2 <- ifelse(status == "Non cas", "Négatif", ifelse(conf & stats::runif(n) < 0.1, "Positif", NA))
  ct1 <- ifelse(res1 %in% "Positif", round(stats::runif(n, 16, 36), 1), NA)

  # ---- Symptoms --------------------------------------------------------------
  symptom_p <- c(fever = .85, nausea = .55, diarrhoea = .35, asthenia_weakness = .9, loss_of_appetite = .7,
                 abdominal_pain = .4, chest_pain = .08, bone_muscle_pain = .3, joint_pain = .15, headache = .75,
                 cough = .2, breathlessness = .1, swallowing_problem = .08, sorethroat = .12, jaundice = .01,
                 conjunctivitis = .02, skin_rash = .01, hichups = .08, coma = .05, confused_disoriented = .03,
                 bleeding = .13, bleeding_gum = .02, bleeding_injection_site = .01, bleeding_epistaxis = .02,
                 bleeding_melenas = .05, bleeding_vomit = .03, bleeding_vomito_negro = .01, bleeding_hemoptisis = .01,
                 bleeding_vagina = .02, hematomes_petechies_purpura = .01, bleeding_urine = .02, bleeding_other = .01)
  symptoms <- lapply(symptom_p, yn)

  fake_id <- sprintf("SYN%04d", seq_len(n))

  ll <- data.frame(
    patient_site_id = fake_id,
    sex = sex, age = age, age_unit = age_unit,
    pregnant = ifelse(sex == "F" & age_unit == "Ans" & age >= 13 & age <= 50,
                      pick(c("Oui", "Non", "Inc."), prob = c(.1, .5, .4)), NA),
    breastfeeding = ifelse(sex == "F" & age_unit == "Ans" & age >= 13 & age <= 50,
                           pick(c("Oui", "Non", "Inc."), prob = c(.05, .55, .4)), NA),
    job = ifelse(stats::runif(n) < 0.2,
                 pick(c("Mineur - Orpailleur/euse", "Ménagère", "Ecolier/ière-Elève",
                        "Personnel de santé: Infirmier/ ière", "Commerçant/e-Marchand/e")), NA),
    HF_name_job = NA,
    adm2_name__res = zs, adm3_name__res = as_res, adm4_name__res = "Quartier",
    date_symptom_onset = onset,
    dead_upon_arrival = doa,
    date_admission_rep = date_adm, date_admission_eff = date_adm,
    date_notification = date_adm,
    date_exit_rep = date_exit, date_exit_eff = date_exit,
    type_of_exit = exit_type,
    exit_no_result = NA,
    EVD_status = status,
    diagnostic_principal = ifelse(status == "Non cas", pick(c("Paludisme", "Fièvre typhoide", "Autre pathologie"),
                                                             prob = c(.7, .2, .1)),
                                  ifelse(conf, "Maladie à Virus Ebola", NA)),
    diagnostic_secondary = NA, diagnostic_other_detail = NA,
    symptoms,
    lab_result_1 = res1, date_lab_sample_1 = ifelse(is.na(res1), NA, date_adm), lab_NP_1 = ct1,
    lab_result_2 = res2, date_lab_sample_2 = ifelse(is.na(res2), NA, date_adm + 2), lab_NP_2 = NA,
    lab_result_3 = NA, date_lab_sample_3 = NA, lab_NP_3 = NA,
    lab_result_4 = NA, date_lab_sample_4 = NA, lab_NP_4 = NA,
    lab_result_5 = NA, date_lab_sample_5 = NA, lab_NP_5 = NA,
    contact_EVD_case = yn(0.1), vaccination_rvsv_yn = ifelse(stats::runif(n) < 0.1, "Non", NA),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  for (v in grep("^date_lab_sample", names(ll), value = TRUE)) ll[[v]] <- as.Date(ll[[v]], origin = "1970-01-01")

  # Health-care worker facility
  hcw <- grepl("^Personnel de sant", ll$job)
  ll$HF_name_job[hcw] <- "HGR Mongbwalu | NDCM | Mongbwalu"

  # ---- Bed occupancy ---------------------------------------------------------
  dates <- seq(start, end, by = "day")
  in_bed <- function(st) {
    vapply(dates, function(d) {
      sum(ll$EVD_status %in% st & ll$date_admission_eff <= d & (is.na(ll$date_exit_eff) | ll$date_exit_eff > d))
    }, numeric(1))
  }
  occ_s <- in_bed(c("Suspect", "Non cas"))
  occ_c <- in_bed("Confirmé")
  beds_s <- 24
  beds_c <- 30
  occupancy <- data.frame(date = dates, occupied_S = occ_s, occupied_P = 0, occupied_C = occ_c,
                          occupied_total = occ_s + occ_c, blank1 = NA, dispo_SP = beds_s,
                          occupation_SP = occ_s / beds_s, dispo_C = beds_c, occupation_C = occ_c / beds_c,
                          dispo_total = beds_s + beds_c, occupation_total = (occ_s + occ_c) / (beds_s + beds_c))
  # Real template: columns 6 and 9 have no header (column 9 holds confirmed beds)
  occ_header <- names(occupancy)
  occ_header[c(6, 9)] <- ""

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "linelist")
  openxlsx::writeData(wb, "linelist", ll)
  openxlsx::addWorksheet(wb, "occupancy")
  openxlsx::writeData(wb, "occupancy", t(occ_header), colNames = FALSE)
  openxlsx::writeData(wb, "occupancy", occupancy, startRow = 2, colNames = FALSE)
  ll_path <- file.path(out_dir, "linelist", "synthetic_linelist.xlsx")
  openxlsx::saveWorkbook(wb, ll_path, overwrite = TRUE)

  # ---- Daily alerts and population ------------------------------------------
  as_all <- c(as_mgl, "Hors zone")
  alerts <- expand.grid(date = dates, as = as_all, stringsAsFactors = FALSE)
  m <- nrow(alerts)
  alerts$reports_vivants <- stats::rpois(m, 0.4)
  alerts$alertes_vivants <- stats::rpois(m, 1.2)
  alerts$report_deces <- stats::rpois(m, 0.05)
  alerts$alertes_deces <- stats::rpois(m, 0.15)
  alerts$vivants_validees <- stats::rbinom(m, alerts$alertes_vivants, 0.7)
  alerts$cas_suspect_cte <- stats::rbinom(m, alerts$vivants_validees, 0.8)
  alerts$cas_suspects_non_transferes <- alerts$vivants_validees - alerts$cas_suspect_cte
  openxlsx::write.xlsx(list(Alerts_Det_Data = alerts), file.path(out_dir, "daily", "Daily data entry.xlsx"))

  pop <- data.frame(zone_de_sante = "Mongbwalu", aire_de_sante = as_mgl,
                    population_de_l_aire_de_sante = round(stats::runif(length(as_mgl), 8000, 45000)))
  openxlsx::write.xlsx(pop, file.path(out_dir, "population", "Population_AS2026_Ituri_dhis2.xlsx"))

  message("Synthetic data written to: ", normalizePath(out_dir))
  invisible(out_dir)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- if (length(args) >= 1) args[1] else "data-synthetic"
  n <- if (length(args) >= 2) as.integer(args[2]) else 600
  make_synthetic_data(out, n)
}
