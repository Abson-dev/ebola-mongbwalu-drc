################################################################################
#   Render the line list analysis report (analysis plan of Dr A. S. Mbengue)
#   to Word, and export all its tables to Excel.
#
#   Open the project (ebola-mongbwalu-drc.Rproj), then source this file.
################################################################################

report_date <- NULL                 # NULL = last complete week (Sunday)
# report_date <- as.Date("2026-11-09")

source(here::here("scripts", "render.R"))
render_report("analysis_plan", if (is.null(report_date)) default_report_date() else report_date)
