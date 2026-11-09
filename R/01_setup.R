################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   1. Setup: configuration, packages, helpers, reporting period
#
#   Author: Stephanie Ngai (original), refactored by Aboubacar Hema
#   Last updated: 9 November 2026
#
################################################################################


# Configuration and helper functions --------------------------------------

source(here::here("R", "00_config.R"), encoding = "UTF-8")

for (f in list.files(here::here("R", "funcs"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}
rm(f)


# Packages ----------------------------------------------------------------
#  Packages are NOT installed automatically at run time (reproducibility).
#  Install them once with:  source(here::here("scripts", "install_packages.R"))

required_packages <- c(
  "tidyverse", "lubridate", "scales", "janitor", "readxl", "openxlsx",
  "gtsummary", "flextable", "officer", "knitr", "aweek", "patchwork", "here"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                              logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "),
       "\nRun: source(here::here('scripts', 'install_packages.R'))", call. = FALSE)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(janitor)
  library(openxlsx)
  library(gtsummary)
  library(flextable)
  library(aweek)
  library(patchwork)
})


# Reporting cutoff date ---------------------------------------------------
#  `report_date` can be set before sourcing this file (e.g. by the render
#  script). Otherwise: today if today is Sunday, else the last Sunday, i.e.
#  the end of the latest complete epidemiological week.

if (!exists("report_date") || is.null(report_date)) {
  report_date <- if (lubridate::wday(Sys.Date()) == 1) {
    Sys.Date()
  } else {
    lubridate::floor_date(Sys.Date(), "week", week_start = 1) - 1
  }
}
report_date <- as.Date(report_date)


# Figure and table defaults -----------------------------------------------

ggplot2::theme_set(ggplot2::theme_classic(base_size = 12))
epicurve_theme <- theme_epicurve()

# Epidemiological weeks start on Monday
aweek::set_week_start("Monday")

invisible(gtsummary::theme_gtsummary_compact(font_size = 10))

flextable::set_flextable_defaults(
  font.family = "Aptos", hansi.family = "Aptos", font.size = 10,
  padding.bottom = 2, padding.top = 2,
  theme_fun = "theme_booktabs",
  table.layout = "autofit", table_align = "left",
  fmt_date = "%d %b"
)

border_dotted <- officer::fp_border(style = "dotted", color = "grey50")


# Epi week cut-offs -------------------------------------------------------

## End of the last complete week (Sunday)
date_cut <- if (lubridate::wday(report_date) == 1) {
  report_date
} else {
  lubridate::floor_date(report_date, "week", week_start = 1) - 1
}

## Start of the current week and of the last 21 days
date_start_1w <- lubridate::floor_date(report_date, "week", week_start = 1)
date_start_3w <- lubridate::floor_date(report_date - 20, "week", week_start = 1)

## Labels of the last 3 weeks ("2026-S44")
week1 <- epiweek_label(report_date)
week2 <- epiweek_label(report_date - 7)
week3 <- epiweek_label(report_date - 14)
week3_vector <- c(week3, week2, week1)

message("Report date: ", format(report_date), " (", week1, ")  |  data: ", cfg$data_dir)
