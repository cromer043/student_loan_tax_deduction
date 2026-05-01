#!/usr/bin/env Rscript

raw_script_path <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1])
script_path <- normalizePath(gsub("~\\+~", " ", raw_script_path), winslash = "/", mustWork = FALSE)
code_dir <- dirname(script_path)

sipp_years <- 2018:2024
scf_years <- c(2016, 2019, 2022)

run_rscript <- function(script_name, extra_args = character(0)) {
  script_arg <- normalizePath(file.path(code_dir, script_name), winslash = "/", mustWork = TRUE)
  system2("Rscript", args = c(shQuote(script_arg), extra_args))
}

run_rscript("download_sipp_years.R")
run_rscript("download_scf_years.R")

for (year in sipp_years) {
  run_rscript("sipp_student_debt_by_race_marital_status.R", paste0("--year=", year))
}

for (year in scf_years) {
  run_rscript("scf_student_loan_interest_deduction_analysis.R", paste0("--year=", year))
}

run_rscript("build_student_debt_time_series_summaries.R")
run_rscript("student_loan_interest_deduction_charts.R")
