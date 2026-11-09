################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   0. Project configuration
#
#   Single place for paths and analysis parameters. Every other script reads
#   its settings from the `cfg` list defined here.
#
#   Data location can be overridden without editing code, e.g. to run the
#   pipeline on synthetic data:
#       Sys.setenv(EBOLA_DATA_DIR = here::here("data-synthetic"))
#
################################################################################

cfg <- list(

  # ---- Paths ----------------------------------------------------------------
  data_dir   = Sys.getenv("EBOLA_DATA_DIR", unset = here::here("data")),
  output_dir = Sys.getenv("EBOLA_OUTPUT_DIR", unset = here::here("output")),
  config_dir = here::here("config"),

  # File patterns (the most recently modified matching file is used)
  linelist_pattern   = "\\.xlsx$",
  dhis2_pattern      = "\\.xlsx$",
  daily_file         = "Daily data entry.xlsx",
  population_file    = "Population_AS2026_Ituri_dhis2.xlsx",

  # ---- Outbreak context -----------------------------------------------------
  health_zone        = "Mongbwalu",
  # Patients admitted before this date are considered exited (historic records)
  historic_exit_date = as.Date("2026-07-01"),
  # Start of the outbreak period used for Rt estimation
  rt_start_date      = as.Date("2026-06-01"),

  # Serial interval (days), Bundibugyo / Beni literature values
  serial_interval    = list(mean = 11.61, sd = 5.2),

  # ---- Age groups -------------------------------------------------------------
  # CTE sitrep (unchanged from the routine weekly report)
  age_breaks_sitrep  = c(0, 5, 15, 45),
  # Analysis plan (Dr A. S. Mbengue). The groups in the plan overlap
  # (0-9 / 5-14 / 15-44 / 40-49 / 50+); these breaks give the closest
  # non-overlapping equivalent: 0-4, 5-14, 15-44, 45-49, 50+.
  age_breaks_plan    = c(0, 5, 15, 45, 50),

  # ---- Delay from symptom onset to notification (days) -----------------------
  delay_breaks       = c(-Inf, 0, 2, 5, Inf),
  delay_labels       = c("< 1 jour", "1-2 jours", "3-5 jours", "> 5 jours"),

  # ---- QA -------------------------------------------------------------------
  qa_lookback_days   = 7,
  ct_range           = c(15, 40)
)
