# End-to-end smoke test on SYNTHETIC data (never on real data).
# Runs the pipeline in a separate R process so that the test session is not
# modified by the scripts' global objects.

run_r <- function(code, env) {
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(code, script, useBytes = TRUE)
  withr::with_envvar(env, system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
                                  stdout = TRUE, stderr = TRUE))
}

test_that("the pipeline runs end-to-end on synthetic data", {
  skip_on_cran()
  root <- here::here()
  data_dir <- withr::local_tempdir("synthetic")
  out_dir <- withr::local_tempdir("output")
  env <- c(EBOLA_DATA_DIR = data_dir, EBOLA_OUTPUT_DIR = out_dir)

  code <- sprintf(paste(
    "setwd('%1$s')",
    "source('scripts/make_synthetic_data.R'); make_synthetic_data('%2$s', n = 400)",
    "report_date <- as.Date('2026-11-08')",
    "for (s in c('01_setup.R', '02_data_linelist.R', '02_data_daily.R', '04_analysis_cte.R',",
    "            '05_analysis_aggregate.R', '07_analysis_plan.R'))",
    "  source(file.path('R', s), encoding = 'UTF-8')",
    "stopifnot(n_adm > 0, n_conf > 0, nrow(pa) == 400, length(pa_tables) > 10)",
    "stopifnot(inherits(t1, 'flextable'), inherits(ec_adm, 'ggplot'), inherits(t_alerts, 'flextable'))",
    "stopifnot(sum(as.integer(t1_df$Cumul[t1_df$name == 'Admissions'])) == n_adm)",
    "cat('PIPELINE_OK\\n')",
    sep = "\n"), root, gsub("\\\\", "/", data_dir))

  out <- run_r(code, env)
  expect_true(any(grepl("PIPELINE_OK", out)), info = paste(utils::tail(out, 30), collapse = "\n"))
})

test_that("the QA script exports a checklist on synthetic data", {
  skip_on_cran()
  root <- here::here()
  data_dir <- withr::local_tempdir("synthetic")
  out_dir <- withr::local_tempdir("output")
  env <- c(EBOLA_DATA_DIR = data_dir, EBOLA_OUTPUT_DIR = out_dir)

  code <- sprintf(paste(
    "setwd('%1$s')",
    "source('scripts/make_synthetic_data.R'); make_synthetic_data('%2$s', n = 300)",
    "cutoff <- as.Date('2026-11-08')",
    "source('R/03_qa_linelist.R', encoding = 'UTF-8')",
    "cat('QA_OK\\n')",
    sep = "\n"), root, gsub("\\\\", "/", data_dir))

  out <- run_r(code, env)
  expect_true(any(grepl("QA_OK", out)), info = paste(utils::tail(out, 30), collapse = "\n"))
  expect_true(file.exists(file.path(out_dir, "data quality", "20261108_ll_qa.xlsx")))
})
