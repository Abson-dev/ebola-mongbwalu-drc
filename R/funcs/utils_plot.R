################################################################################
#   Plot helpers
################################################################################

#' Common theme for epidemic curves and time series
#'
#' @return A `ggplot2::theme` object.
theme_epicurve <- function() {
  ggplot2::theme(
    legend.title     = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "grey", linetype = "dashed"),
    panel.grid.minor = ggplot2::element_blank(),
    plot.caption     = ggplot2::element_text(hjust = 0, face = "italic"),
    strip.text.x     = ggplot2::element_text(colour = "black")
  )
}

#' Weekly epidemic curve
#'
#' Counts rows per (Monday-starting) week of `date_measure`. Bars span the
#' whole week, so that a weekly series drawn at `week + 3.5` sits in the middle
#' of each bar.
#'
#' @param df Data frame, one row per patient/case.
#' @param date_measure Name of the date column to aggregate.
#' @param date_min,date_max Optional limits; rows on/after `date_max` are
#'   dropped (use the Monday following the reporting week).
#' @param fill_by Optional name of a categorical column used for stacking.
#' @param pal Optional colour palette for `fill_by` (named or unnamed).
#' @return A `ggplot` object.
vis_epicurve <- function(df, date_measure = "pdate", date_min = NULL,
                         date_max = NULL, fill_by = NULL, pal = NULL) {
  stopifnot(date_measure %in% names(df))
  d <- df |>
    dplyr::mutate(.date = as.Date(.data[[date_measure]])) |>
    dplyr::filter(!is.na(.date))
  if (!is.null(date_max)) d <- dplyr::filter(d, .date < as.Date(date_max))
  if (!is.null(date_min)) d <- dplyr::filter(d, .date >= as.Date(date_min))

  d <- dplyr::mutate(d, .week = lubridate::floor_date(.date, "week", week_start = 1))
  if (is.null(fill_by)) {
    d <- dplyr::count(d, .week)
  } else {
    d <- dplyr::count(d, .week, .fill = .data[[fill_by]])
  }

  x_min <- if (is.null(date_min)) min(d$.week) else lubridate::floor_date(as.Date(date_min), "week", week_start = 1)
  x_max <- if (is.null(date_max)) max(d$.week) + 7 else as.Date(date_max)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .week + 3.5, y = n)) +
    ggplot2::scale_x_date(labels = scales::label_date_short(),
                          date_breaks = "2 weeks", date_minor_breaks = "1 week",
                          limits = c(x_min, x_max),
                          expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    theme_epicurve() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

  if (is.null(fill_by)) {
    p <- p + ggplot2::geom_col(width = 7, fill = "#b0364e", colour = "white", linewidth = 0.2)
  } else {
    p <- p + ggplot2::geom_col(ggplot2::aes(fill = .fill), width = 7,
                               colour = "white", linewidth = 0.2)
    if (!is.null(pal)) p <- p + ggplot2::scale_fill_manual(values = pal, drop = TRUE)
  }
  p
}

#' Age-sex pyramid
#'
#' Lightweight replacement for `apyramid::age_pyramid()`: the first level of
#' `split_by` is drawn on the left.
#'
#' @param df Data frame.
#' @param age_group Name of the (factor) age group column.
#' @param split_by Name of the two-level grouping column (usually sex).
#' @param pal Named colour vector for the levels of `split_by`.
#' @return A `ggplot` object.
plot_age_pyramid <- function(df, age_group = "age_group", split_by = "sex_f",
                             pal = c("Féminin" = "#00bfbf99", "Masculin" = "#00525299")) {
  d <- df |>
    dplyr::filter(!is.na(.data[[age_group]]), !is.na(.data[[split_by]])) |>
    dplyr::count(.age = .data[[age_group]], .split = .data[[split_by]], .drop = FALSE)
  lv <- levels(factor(d$.split))
  d <- dplyr::mutate(d, n_signed = dplyr::if_else(.split == lv[1], -n, n))
  lim <- max(c(d$n, 1))

  ggplot2::ggplot(d, ggplot2::aes(x = .age, y = n_signed, fill = .split)) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = abs, limits = c(-lim, lim)) +
    ggplot2::scale_fill_manual(values = pal) +
    theme_epicurve()
}
