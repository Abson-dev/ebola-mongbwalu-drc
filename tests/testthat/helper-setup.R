# Load configuration and helper functions (not the full pipeline)
source(here::here("R", "00_config.R"), encoding = "UTF-8")
for (f in list.files(here::here("R", "funcs"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}
rm(f)
aweek::set_week_start("Monday")
