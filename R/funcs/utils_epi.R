################################################################################
#   Epidemiological helpers
################################################################################

#' Bin a numeric vector into labelled, closed-open groups
#'
#' `bin_numbers(c(2, 7, 30, 60), breaks = c(0, 5, 15, 45))` returns a factor
#' with levels `"0-4"`, `"5-14"`, `"15-44"`, `"45+"`.
#'
#' @param x Numeric vector (e.g. age in years).
#' @param breaks Increasing lower bounds of each group; the last group is
#'   open-ended.
#' @return A factor; values below the first break or `NA` become `NA`.
bin_numbers <- function(x, breaks) {
  stopifnot(is.numeric(breaks), !is.unsorted(breaks))
  n <- length(breaks)
  labels <- c(paste0(breaks[-n], "-", breaks[-1] - 1), paste0(breaks[n], "+"))
  cut(x, breaks = c(breaks, Inf), labels = labels, right = FALSE)
}

#' Epidemiological week label in French notation ("2026-S44")
#'
#' Weeks start on Monday (ISO/aweek convention set in `01_setup.R`).
#'
#' @param date A `Date` vector.
#' @return A character vector such as `"2026-S44"`.
epiweek_label <- function(date) {
  out <- as.character(aweek::date2week(date, week_start = "Monday", floor_day = TRUE))
  out <- sub("W", "S", out, fixed = TRUE)
  out[is.na(date)] <- NA_character_
  out
}

#' Case fatality ratio with Wilson score confidence interval
#'
#' Replaces `epitabulate::case_fatality_rate_df()` with identical output
#' columns (`deaths`, `population`, `cfr`, `ci`), avoiding a GitHub-only
#' dependency.
#'
#' @param deaths Logical or 0/1 vector (1 = died). `NA` is excluded.
#' @param conf_level Confidence level.
#' @param digits Rounding of the percentage.
#' @return A one-row tibble; `cfr` is expressed in percent.
cfr_wilson <- function(deaths, conf_level = 0.95, digits = 1) {
  d <- deaths[!is.na(deaths)]
  x <- sum(d == 1)
  n <- length(d)
  if (n == 0) {
    return(tibble::tibble(deaths = 0L, population = 0L, cfr = NA_real_,
                          lower = NA_real_, upper = NA_real_, ci = NA_character_))
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  lower <- 100 * max(0, centre - half)
  upper <- 100 * min(1, centre + half)
  tibble::tibble(
    deaths = x, population = n,
    cfr = round(100 * p, digits),
    lower = round(lower, digits), upper = round(upper, digits),
    ci = paste0("(", format_ci(lower, upper, digits), ")")
  )
}

#' Round up to the next multiple of `accuracy` (replaces `plyr::round_any`)
#'
#' @param x Numeric.
#' @param accuracy Multiple to round to.
ceiling_to <- function(x, accuracy = 10) {
  out <- ceiling(x / accuracy) * accuracy
  ifelse(out == 0, accuracy, out)
}

#' Relative change between two counts, as a French sentence fragment
#'
#' `pct_change_text(30, 29)` gives `"une augmentation de 3%"`.
#'
#' @param current,previous Counts for the current and previous period.
#' @return Character string ready to be inserted in report text.
pct_change_text <- function(current, previous) {
  if (is.na(previous) || previous == 0) {
    return("variation non calculable (aucune donnée la semaine précédente)")
  }
  change <- (current - previous) / previous
  if (change == 0) return("un nombre stable")
  sprintf("une %s de %s",
          if (change > 0) "augmentation" else "réduction",
          scales::percent(abs(change), accuracy = 1))
}

#' Format dates in French, independently of the system locale
#'
#' `format_date_fr(as.Date("2026-11-08"), "%d %B %Y")` gives
#' `"08 novembre 2026"`.
#'
#' @param x A `Date`.
#' @param fmt A `strftime` format; `%B` is replaced by the French month name.
format_date_fr <- function(x, fmt = "%d %B %Y") {
  months_fr <- c("janvier", "février", "mars", "avril", "mai", "juin", "juillet",
                 "août", "septembre", "octobre", "novembre", "décembre")
  out <- vapply(seq_along(x), function(i) {
    f <- gsub("%B", months_fr[as.integer(format(x[i], "%m"))], fmt, fixed = TRUE)
    format(x[i], f)
  }, character(1))
  out[is.na(x)] <- NA_character_
  out
}

#' Confidence interval as text ("58.0-68.0")
#'
#' @param lower,upper Bounds.
#' @param digits Decimal places.
format_ci <- function(lower, upper, digits = 1) {
  sprintf(paste0("%.", digits, "f-%.", digits, "f"), lower, upper)
}

#' Percentage that never divides by zero
#'
#' @param num,den Numerator and denominator.
#' @param accuracy Passed to `scales::percent()`.
safe_percent <- function(num, den, accuracy = 1) {
  if (is.na(den) || den == 0) return("--")
  scales::percent(num / den, accuracy = accuracy)
}
