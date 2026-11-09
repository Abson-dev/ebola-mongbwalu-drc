################################################################################
#   Render the weekly epidemiological sitrep (Word)
#
#   Open the project (ebola-mongbwalu-drc.Rproj), then source this file.
#   Change `report_date` below to render a past week.
################################################################################

report_date <- NULL                 # NULL = last complete week (Sunday)
# report_date <- as.Date("2026-11-08")

source(here::here("scripts", "render.R"))
render_report("weekly", if (is.null(report_date)) default_report_date() else report_date)
