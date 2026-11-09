################################################################################
#   Install the R packages used by the pipeline (run once per machine)
#
#     source("scripts/install_packages.R")
################################################################################

options(repos = c(CRAN = "https://cloud.r-project.org"))

# Routine pipeline (sitrep, analysis plan, QA, Monday table)
core <- c("tidyverse", "lubridate", "scales", "janitor", "readxl", "openxlsx",
          "gtsummary", "cardx", "flextable", "officer", "knitr", "rmarkdown",
          "aweek", "patchwork", "here")

# Exploratory DHIS2 analyses (R/08_analysis_dhis.R) and development tools
extra <- c("EpiEstim", "gghighlight", "remotes", "testthat", "withr", "lintr")

missing <- setdiff(c(core, extra), rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)

# Transmission chains with timeline layout (GitHub branch)
if (!requireNamespace("epicontacts", quietly = TRUE)) {
  remotes::install_github("reconhub/epicontacts@timeline", upgrade = "never")
}

message("All packages installed. Quarto CLI is also required: https://quarto.org")
