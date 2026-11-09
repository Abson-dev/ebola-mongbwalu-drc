################################################################################
#
#   MSF OCG - Mongbwalu - Ebola Bundibugyo
#
#   Render a Quarto report to Word
#
#   Usage (from the project root):
#     Rscript scripts/render.R weekly                  # last complete week
#     Rscript scripts/render.R analysis_plan 2026-11-08
#
#   or from R / RStudio:
#     source("scripts/render.R"); render_report("weekly")
#
#   The Word template is reports/templates/msf_template.docx if present
#   (organisation template, kept out of version control), otherwise the
#   neutral reports/templates/reference.docx.
#
#   Output: <output_dir>/reports/<name>_S<week>_<YYYYMMDD>.docx
#
################################################################################

#' Locate the Quarto command-line tool
find_quarto <- function() {
  candidates <- c(
    Sys.getenv("QUARTO_PATH"),
    Sys.which("quarto"),
    "C:/Program Files/Quarto/bin/quarto.exe",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
    "/usr/local/bin/quarto"
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit) == 0) stop("Quarto not found. Install it from https://quarto.org or set QUARTO_PATH.", call. = FALSE)
  normalizePath(hit[1], winslash = "/")
}

#' Default report date: end (Sunday) of the last complete epidemiological week
default_report_date <- function(today = Sys.Date()) {
  if (as.POSIXlt(today)$wday == 0) today else lubridate::floor_date(today, "week", week_start = 1) - 1
}

#' Render one report
#'
#' @param report `"weekly"` or `"analysis_plan"`.
#' @param report_date Data cut-off date (`Date` or `"YYYY-MM-DD"`).
#' @return Path of the rendered Word document (invisibly).
render_report <- function(report = c("weekly", "analysis_plan"), report_date = default_report_date()) {
  report <- match.arg(report)
  report_date <- as.Date(report_date)
  root <- here::here()

  input <- file.path(root, "reports", paste0(report, "_report.qmd"))
  template_dir <- file.path(root, "reports", "templates")
  msf_template <- file.path(template_dir, "msf_template.docx")

  week <- format(report_date, "%V")
  stem <- switch(report,
                 weekly = "Sitrep_Hebdomadaire_Epidemio",
                 analysis_plan = "Analyse_LineList_Mongbwalu")
  out_file <- sprintf("%s_S%s_%s.docx", stem, week, format(report_date, "%Y%m%d"))
  out_dir <- Sys.getenv("EBOLA_OUTPUT_DIR", unset = file.path(root, "output"))
  out_dir <- file.path(out_dir, "reports")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  args <- c("render", shQuote(input), "--to", "docx",
            "--output", shQuote(out_file),
            "-P", paste0("report_date:", format(report_date)))
  if (file.exists(msf_template)) {
    args <- c(args, "-M", "reference-doc:templates/msf_template.docx")
  }

  message("Rendering ", basename(input), " (report date ", report_date, ")...")
  # Quarto resolves --output against the working directory
  old_wd <- setwd(dirname(input))
  on.exit(setwd(old_wd), add = TRUE)
  status <- system2(find_quarto(), args)
  setwd(old_wd)
  if (status != 0) stop("Quarto render failed (exit status ", status, ").", call. = FALSE)

  # Quarto writes next to the .qmd; move the document to output/reports
  produced <- file.path(dirname(input), out_file)
  target <- file.path(out_dir, out_file)
  if (!file.copy(produced, target, overwrite = TRUE)) stop("Could not copy ", produced, call. = FALSE)
  unlink(produced)

  # Analysis plan: also export all tables to Excel
  #  (run in a fresh R process so that the caller's session is not modified)
  if (report == "analysis_plan") {
    xlsx <- file.path(out_dir, sub("\\.docx$", "_tableaux.xlsx", out_file))
    code <- sprintf(paste(
      "report_date <- as.Date('%s')",
      "for (s in c('01_setup.R', '02_data_linelist.R', '07_analysis_plan.R'))",
      "  source(file.path('%s', 'R', s), encoding = 'UTF-8')",
      "openxlsx::write.xlsx(pa_tables, '%s', asTable = TRUE, overwrite = TRUE)",
      sep = "\n"), report_date, root, xlsx)
    script <- tempfile(fileext = ".R")
    writeLines(code, script, useBytes = TRUE)
    status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(script))
    if (status != 0) stop("Excel export of the analysis tables failed.", call. = FALSE)
    message("Tables: ", xlsx)
  }

  message("Report: ", target)
  invisible(target)
}

# Command-line entry point
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  report <- if (length(args) >= 1) args[1] else "weekly"
  date <- if (length(args) >= 2) args[2] else default_report_date()
  render_report(report, date)
}
