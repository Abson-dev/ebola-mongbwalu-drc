################################################################################
#   Data quality helpers
################################################################################

#' Patient IDs exempted from a given QA rule
#'
#' Some records trigger a rule legitimately (verified with the clinical team).
#' Exemptions are kept in `config/qa_exceptions.csv` (columns `rule`,
#' `patient_site_id`, `reason`) instead of being hard-coded.
#'
#' @param rule Rule identifier, e.g. `"err10"`.
#' @param path Path of the exceptions file.
#' @return Character vector of patient IDs.
qa_exempt <- function(rule, path = file.path(cfg$config_dir, "qa_exceptions.csv")) {
  if (!file.exists(path)) return(character(0))
  ex <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                        comment = "#", show_col_types = FALSE)
  unique(ex$patient_site_id[ex$rule == rule])
}

#' Collapse all error columns of a row into a single string
#'
#' @param df Data frame containing error columns.
#' @param prefix Regular expression identifying error columns.
#' @return `df` restricted to rows with at least one error, with a new
#'   `errors` column ("err A; err B").
collapse_errors <- function(df, prefix = "^err") {
  err_cols <- grep(prefix, names(df), value = TRUE)
  df |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(err_cols), ~ !is.na(.x) & .x != "")) |>
    tidyr::unite("errors", dplyr::all_of(err_cols), sep = "; ", na.rm = TRUE, remove = FALSE) |>
    dplyr::mutate(errors = gsub("(; )+", "; ", errors),
                  errors = gsub("^; |; $", "", errors))
}

#' Percentage of missing values per variable
#'
#' @param df Data frame.
#' @param vars Columns to assess.
#' @param labels Optional display labels (same length as `vars`).
#' @return A tibble with `Variable`, `Manquant (n)`, `Manquant (%)`.
missing_summary <- function(df, vars, labels = vars) {
  tibble::tibble(
    Variable = labels,
    `Renseigné (n)` = vapply(vars, function(v) sum(!is.na(df[[v]])), integer(1), USE.NAMES = FALSE),
    `Manquant (n)` = vapply(vars, function(v) sum(is.na(df[[v]])), integer(1), USE.NAMES = FALSE)
  ) |>
    dplyr::mutate(`Manquant (%)` = scales::percent(`Manquant (n)` / nrow(df), accuracy = 0.1)) |>
    dplyr::arrange(dplyr::desc(`Manquant (n)`))
}
