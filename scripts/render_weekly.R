################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   Render weekly epi report
#
#   Author: Stephanie Ngai
#   Last updated: 21 September 2026
#
################################################

##### This script is used to render the weekly epi update (qmd)
#     Input R scripts will be called in
#     Output is saved as a word document in the same folder where this script is saved
#     The input linelist needs to be exported in .xlsx format and NOT password-protected
#       and saved in the data folder of the R project
#     The report/data cutoff date can be changed manually below


# Setup -------------------------------------------------------------------

# Remove all objects
rm(list = ls())

# Ensures the package "pacman" is installed
if (!require("pacman")) {
  install.packages("pacman") }

pacman::p_load(
  quarto
)


# Reporting cutoff date ---------------------------------------------------

#  By default: latest complete week
#  Uncomment second line and change manually as needed

report_date <- lubridate::floor_date(Sys.Date(), "week", 1) - 1
# report_date <- Sys.Date()
# report_date <- as.Date("2026-09-25")

report_week <- aweek::date2week(report_date, numeric = TRUE)


# Render report -----------------------------------------------------------

#  apparently cannot use filepath in quarto render

quarto::quarto_render(
  input = here::here("R", "weekly report.qmd"),
  output_format = "docx",
  output_file = paste0("Sitrep Hebdomadaire Epidémio_S", report_week,
                       "_", format(report_date, "%Y%m%d"), ".docx"))



