# Run all tests from the project root:
#   Rscript tests/testthat.R
library(testthat)
Sys.setenv(NOT_CRAN = "true")  # run the end-to-end tests
test_dir(here::here("tests", "testthat"), reporter = c("progress", "fail"), stop_on_failure = TRUE)
