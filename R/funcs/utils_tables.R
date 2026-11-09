################################################################################
#   Table helpers (gtsummary / flextable)
################################################################################

#' Standard formatting for gtsummary tables in the sitrep
#'
#' @param tbl A `gtsummary` table.
#' @return The formatted `gtsummary` table.
format_tab <- function(tbl) {
  tbl |>
    gtsummary::bold_labels() |>
    gtsummary::modify_header(label = "")
}

#' Standard formatting for stratified gtsummary tables
#'
#' Puts the stratum name and its size on two header lines.
#'
#' @param tbl A `gtsummary` table created with `by =`.
#' @return The formatted `gtsummary` table.
format_tab_by2_header <- function(tbl) {
  tbl |>
    gtsummary::modify_header(gtsummary::all_stat_cols() ~ "**{level}**  \nN = {n}") |>
    format_tab()
}

#' Count and percentage table of one categorical variable
#'
#' @param df Data frame.
#' @param var Column name.
#' @param label Header for the category column.
#' @param sort Sort categories by decreasing frequency?
#' @return A tibble with columns `label`, `n` and `pct`.
freq_table <- function(df, var, label = var, sort = TRUE) {
  out <- df |>
    dplyr::mutate(.x = forcats::fct_na_value_to_level(factor(.data[[var]]), "Inconnu")) |>
    dplyr::count(.x, name = "n") |>
    dplyr::mutate(pct = scales::percent(n / sum(n), accuracy = 0.1))
  if (sort) out <- dplyr::arrange(out, dplyr::desc(n))
  out |>
    janitor::adorn_totals("row") |>
    dplyr::mutate(pct = dplyr::if_else(.x == "Total", "100%", pct)) |>
    dplyr::rename(!!label := .x, `N` = n, `%` = pct)
}

#' Bivariate analysis of a binary outcome against categorical exposures
#'
#' For each exposure level: number of patients, number and percentage with
#' the outcome, crude odds ratio (logistic regression, Wald 95% CI) against
#' the first level, and a global p-value (Chi-squared test, or Fisher's exact
#' test when an expected count is below 5).
#'
#' @param df Data frame.
#' @param outcome Name of a logical / 0-1 outcome column.
#' @param exposures Named character vector: names are display labels, values
#'   are column names (factors; the first level is the reference).
#' @return A tibble, one row per exposure level, with columns `Variable`,
#'   `Modalité`, `Effectif`, `Issue (n)`, `%`, `OR (IC 95%)` and `p`.
bivariate_table <- function(df, outcome, exposures) {
  purrr::imap_dfr(exposures, function(var, label) {
    d <- df |>
      dplyr::filter(!is.na(.data[[outcome]]), !is.na(.data[[var]])) |>
      dplyr::mutate(.y = as.integer(as.logical(.data[[outcome]])),
                    .x = droplevels(factor(.data[[var]])))
    if (nlevels(d$.x) < 2 || length(unique(d$.y)) < 2) return(NULL)

    tab <- table(d$.x, d$.y)
    expected <- suppressWarnings(stats::chisq.test(tab)$expected)
    p <- if (any(expected < 5)) {
      stats::fisher.test(tab, simulate.p.value = nrow(tab) > 2, B = 1e4)$p.value
    } else {
      stats::chisq.test(tab)$p.value
    }

    # Odds ratios are not estimable for levels with 0% or 100% outcome
    # (complete separation): reported as "NC" (non calculable)
    cnt <- d |> dplyr::group_by(.x) |> dplyr::summarise(k = sum(.y), m = dplyr::n(), .groups = "drop")
    degenerate <- cnt$k == 0 | cnt$k == cnt$m
    if (degenerate[1]) {
      or_txt <- c("Réf.", rep("NC", nlevels(d$.x) - 1))
    } else {
      fit <- suppressWarnings(stats::glm(.y ~ .x, data = d, family = stats::binomial()))
      est <- stats::coef(fit)[-1]
      ci <- stats::confint.default(fit)[-1, , drop = FALSE]
      or_txt <- c("Réf.", sprintf("%.2f (%.2f-%.2f)", exp(est), exp(ci[, 1]), exp(ci[, 2])))
      or_txt[degenerate] <- "NC"
      or_txt[1] <- "Réf."
    }

    d |>
      dplyr::group_by(.x) |>
      dplyr::summarise(N = dplyr::n(), n = sum(.y), .groups = "drop") |>
      dplyr::mutate(
        Variable = c(label, rep("", dplyr::n() - 1)),
        `%` = scales::percent(n / N, accuracy = 0.1),
        `OR (IC 95%)` = or_txt,
        `p` = c(format.pval(p, digits = 2, eps = 0.001), rep("", dplyr::n() - 1))
      ) |>
      dplyr::select(Variable, Modalité = .x, Effectif = N, `Issue (n)` = n, `%`, `OR (IC 95%)`, p) |>
      dplyr::mutate(Modalité = as.character(Modalité))
  })
}

#' Placeholder flextable for an empty table
#'
#' @param msg Message displayed instead of the table.
#' @return A one-cell `flextable`.
empty_flextable <- function(msg = "Aucune donnée pour la période") {
  flextable::flextable(data.frame(x = msg)) |>
    flextable::delete_part(part = "header") |>
    flextable::italic()
}

#' Flextable with the house style
#'
#' @param df Data frame.
#' @return A `flextable`.
house_flextable <- function(df) {
  df |>
    flextable::flextable() |>
    flextable::border_inner_h(border = officer::fp_border(style = "dotted", color = "grey50")) |>
    flextable::bold(part = "header")
}
