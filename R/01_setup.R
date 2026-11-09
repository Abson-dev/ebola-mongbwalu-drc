################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   1. Setup
#
#   Author: Stephanie Ngai
#   Last updated: 17 September 2026
#
################################################



# Reporting cutoff date ---------------------------------------------------

#  By default: latest complete week
#  Uncomment second line and change manually as needed
#  Usually taken from render document

# report_date <- if (!exists("report_date")) {
#   lubridate::floor_date(Sys.Date(), "week", 1) - 1
# } else if (is.null(report_date)) {
#     lubridate::floor_date(Sys.Date(), "week", 1) - 1
#   } else {
#   report_date
# }

report_date <- if (!exists("report_date") & lubridate::wday(Sys.Date()) == 1) {
  Sys.Date()
} else if(!exists("report_date")) {
  lubridate::floor_date(Sys.Date(), "week", 1) - 1
} else if (is.null(report_date)) {
  lubridate::floor_date(Sys.Date(), "week", 1) - 1
} else {
  report_date
}




# report_date <- as.Date("2026-09-13")


# Load packages -----------------------------------------------------------

# Ensures the package "pacman" is installed
if (!require("pacman")) {
  install.packages("pacman") }

pacman::p_load(
  tidyverse,
  lubridate,
  scales,
  janitor,
  readxl,
  openxlsx,
  gt,
  gtsummary,
  gtExtras,
  flextable,
  knitr,
  gghighlight,
  visNetwork,
  ggrepel,
  apyramid,
  aweek,
  vistime,
  zoo,
  patchwork,
  rio,
  here,
  epitabulate
)

if (!require(epicontacts)) {
  remotes::install_github("reconhub/epicontacts@timeline")
}



# Functions ---------------------------------------------------------------

walk(list.files(here::here("R/funcs"), full.names = TRUE, pattern = ".R$"), source)



# Figure and table defaults -----------------------------------------------

## ggplot
ggplot2::theme_set(theme_classic(base_size = 12))

epicurve_theme <- theme(
  legend.title = element_blank(),
  panel.grid.major = element_line(colour = "grey", linetype = "dashed"),
  panel.grid.minor = element_blank(),
  plot.caption = element_text(hjust = 0, face = "italic"),
  strip.text.x = element_text(color = "black"))

## set the day that defines the beginning of your epiweek.
aweek::set_week_start("Monday")

## gtsummary theme
theme_gtsummary_compact(font_size = 10)

##### Set default formatting for flextables
set_flextable_defaults(font.family = "Aptos", hansi.family = "Aptos", font.size = 10,
                       padding.bottom = 2, padding.top = 2,
                       theme_fun="theme_booktabs",
                       table.layout = "autofit", table_align = "left",
                       fmt_date = "%d %b")

border_dotted <- officer::fp_border(style = "dotted", color = "grey50")




# Epi week cutoffs --------------------------------------------------------

## Last complete week (based on sys date)
# date_cut <- lubridate::floor_date(Sys.Date(), "weeks", 1)-1

date_cut <- if(lubridate::wday(report_date) == 1) {
  report_date
} else {
  lubridate::floor_date(report_date, "weeks", 1)-1
}


## Start date of 21 days (based on report date)
date_start_1w <- lubridate::floor_date(report_date, "weeks", 1)
date_start_3w <- lubridate::floor_date(report_date-20, "weeks", 1)

## Vector with last 3 weeks
week1 <- as.character(date2week(report_date, factor = T))
week2 <- as.character(date2week(report_date-7, factor = T))
week3 <- as.character(date2week(report_date-14, factor = T))

week1 <- str_replace(week1, "W", "S")
week2 <- str_replace(week2, "W", "S")
week3 <- str_replace(week3, "W", "S")

week3_vector <- c(week3, week2, week1)

## Last 3 weeks
# this_week <- date2week(report_date, factor = T)
# last_week <- date2week(report_date-7, factor = T)
# week3 <- date2week(report_date-14, factor = T)
# 
# last_week <- report_date - 7
# this_week <- date2week(report_date, numeric = T)
# week3_vector <- c(this_week-2, this_week-1, this_week)
# week3_vector_txt <- paste0("2026_", week3_vector)

