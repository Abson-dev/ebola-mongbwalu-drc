################################################################################
#   Input / output helpers
################################################################################

#' Build a path inside the data directory
#'
#' @param ... Path components passed to `file.path()`.
#' @return A character path rooted at `cfg$data_dir`.
path_data <- function(...) file.path(cfg$data_dir, ...)

#' Build a path inside the output directory, creating parent folders
#'
#' @param ... Path components passed to `file.path()`.
#' @return A character path rooted at `cfg$output_dir`.
path_output <- function(...) {
  p <- file.path(cfg$output_dir, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

#' Most recently modified file in a folder
#'
#' The field team overwrites/adds a new export every day; the pipeline always
#' uses the latest one.
#'
#' @param dir Folder to search.
#' @param pattern Regular expression the file name must match.
#' @return The full path of the newest matching file.
latest_file <- function(dir, pattern = "\\.xlsx$") {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  # Ignore Excel lock files ("~$file.xlsx")
  files <- files[!startsWith(basename(files), "~$")]
  if (length(files) == 0) {
    stop("No file matching '", pattern, "' found in: ", dir,
         "\nSee data/README.md for the expected folder layout.", call. = FALSE)
  }
  files[which.max(file.info(files)$mtime)]
}

#' Read one sheet of an Excel workbook with clean names
#'
#' Wraps `readxl::read_excel()` with a very large `guess_max`, so that sparse
#' columns (e.g. CT values of a 5th lab sample) are not silently mistyped as
#' logical and set to `NA`.
#'
#' @param path Workbook path.
#' @param sheet Sheet name or index.
#' @param ... Passed on to `readxl::read_excel()`.
#' @return A tibble with snake_case column names.
read_sheet <- function(path, sheet = 1, ...) {
  readxl::read_excel(path, sheet = sheet, guess_max = 1e6,
                     .name_repair = "unique_quiet", ...) |>
    janitor::clean_names()
}

#' Write a data frame to Excel as a formatted table
#'
#' @param x Data frame.
#' @param path Output path.
write_xlsx_table <- function(x, path) {
  openxlsx::write.xlsx(x, path, asTable = TRUE, overwrite = TRUE)
  message("Written: ", path)
  invisible(path)
}

#' Apply manual data corrections from a CSV file
#'
#' Corrections agreed with the clinical team are kept in
#' `config/manual_corrections.csv` (columns `patient_site_id`, `variable`,
#' `value`, `reason`) rather than hard-coded in analysis scripts.
#'
#' @param df Line list.
#' @param path Path of the corrections file.
#' @return `df` with corrected values (as character, then re-typed by caller).
apply_corrections <- function(df, path = file.path(cfg$config_dir, "manual_corrections.csv")) {
  if (!file.exists(path)) return(df)
  corr <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                          comment = "#", show_col_types = FALSE)
  for (i in seq_len(nrow(corr))) {
    v <- corr$variable[i]
    if (!v %in% names(df)) {
      warning("Correction ignored, unknown variable: ", v, call. = FALSE)
      next
    }
    hit <- which(df$patient_site_id == corr$patient_site_id[i])
    if (length(hit) == 0) next
    df[[v]] <- as.character(df[[v]])
    df[[v]][hit] <- corr$value[i]
  }
  df
}
