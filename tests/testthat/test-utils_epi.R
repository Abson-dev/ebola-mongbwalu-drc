test_that("bin_numbers creates closed-open labelled groups", {
  x <- c(0, 4.9, 5, 14, 15, 44, 45, 90, NA)
  out <- bin_numbers(x, breaks = c(0, 5, 15, 45))
  expect_s3_class(out, "factor")
  expect_identical(levels(out), c("0-4", "5-14", "15-44", "45+"))
  expect_identical(as.character(out),
                   c("0-4", "0-4", "5-14", "5-14", "15-44", "15-44", "45+", "45+", NA))
})

test_that("bin_numbers rejects unsorted breaks", {
  expect_error(bin_numbers(1:3, c(5, 0)))
})

test_that("epiweek_label uses Monday weeks and the French 'S' prefix", {
  # 2026-11-02 (Monday) to 2026-11-08 (Sunday) is ISO week 45
  expect_identical(epiweek_label(as.Date(c("2026-11-02", "2026-11-08"))), c("2026-S45", "2026-S45"))
  expect_identical(epiweek_label(as.Date("2026-11-09")), "2026-S46")
  expect_true(is.na(epiweek_label(as.Date(NA))))
})

test_that("cfr_wilson matches the Wilson score interval", {
  res <- cfr_wilson(c(rep(1, 5), rep(0, 5)))
  expect_equal(res$deaths, 5)
  expect_equal(res$population, 10)
  expect_equal(res$cfr, 50)
  # Reference values: binom::binom.wilson(5, 10) -> 0.2366, 0.7634
  expect_equal(res$lower, 23.7)
  expect_equal(res$upper, 76.3)
})

test_that("cfr_wilson ignores missing outcomes and handles empty input", {
  expect_equal(cfr_wilson(c(1, 0, NA))$population, 2)
  empty <- cfr_wilson(numeric(0))
  expect_equal(empty$population, 0)
  expect_true(is.na(empty$cfr))
})

test_that("cfr_wilson bounds stay within [0, 100]", {
  expect_equal(cfr_wilson(rep(1, 3))$upper, 100)
  expect_equal(cfr_wilson(rep(0, 3))$lower, 0)
})

test_that("ceiling_to rounds up and never returns 0", {
  expect_equal(ceiling_to(c(1, 10, 11, 0)), c(10, 10, 20, 10))
})

test_that("pct_change_text describes direction and handles zero", {
  expect_identical(pct_change_text(30, 29), "une augmentation de 3%")
  expect_identical(pct_change_text(20, 40), "une réduction de 50%")
  expect_identical(pct_change_text(5, 5), "un nombre stable")
  expect_match(pct_change_text(5, 0), "non calculable")
})

test_that("safe_percent never divides by zero", {
  expect_identical(safe_percent(1, 4), "25%")
  expect_identical(safe_percent(1, 0), "--")
})

test_that("format_date_fr uses French month names", {
  expect_identical(format_date_fr(as.Date("2026-11-08")), "08 novembre 2026")
  expect_identical(format_date_fr(as.Date("2026-08-01"), "%d %B"), "01 août")
})

test_that("format_ci formats bounds with fixed decimals", {
  expect_identical(format_ci(58, 68.04), "58.0-68.0")
})
