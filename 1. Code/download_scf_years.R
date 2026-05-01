#!/usr/bin/env Rscript

raw_script_path <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1])
script_path <- normalizePath(gsub("~\\+~", " ", raw_script_path), winslash = "/", mustWork = FALSE)
code_dir <- dirname(script_path)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(code_dir, "student_loan_interest_workflow_utils.R"))
paths <- get_workflow_paths(project_dir)

if (!requireNamespace("scf", quietly = TRUE)) {
  install.packages("scf", repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages(library(scf))

years <- c(2016, 2019, 2022)

for (year in years) {
  year_dir <- file.path(paths$scf_raw_dir, year)
  ensure_dir(year_dir)
  old_wd <- getwd()
  setwd(year_dir)
  on.exit(setwd(old_wd), add = TRUE)

  scf::scf_download(year)
}

cat("SCF download cache refreshed in:", paths$scf_raw_dir, "\n")
