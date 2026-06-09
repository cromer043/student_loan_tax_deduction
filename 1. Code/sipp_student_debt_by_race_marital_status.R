#!/usr/bin/env Rscript

# SIPP student debt analysis by race and household marital status
# Optimized for very large files:
#   - loads headers first
#   - identifies variables before loading data
#   - reloads only required columns
#   - uses data.table::fread() for speed
#   - optionally loads replicate weights

# =========================
# MANUAL VARIABLE OVERRIDES
# =========================
raw_script_path <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1])
script_path <- normalizePath(gsub("~\\+~", " ", raw_script_path), winslash = "/", mustWork = FALSE)
code_dir <- dirname(script_path)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(code_dir, "student_loan_interest_workflow_utils.R"))
paths <- get_workflow_paths(project_dir)
analysis_year <- parse_year_arg(2024)
data_dir <- file.path(paths$sipp_raw_dir, analysis_year)
legacy_data_dir <- file.path(project_dir, "0. Data", "SIPP")
output_csv_dir_sipp <- paths$outmoded_sipp_csv_dir
output_csv_dir_sipp_year <- file.path(output_csv_dir_sipp, "by-year", analysis_year)
output_csv_dir_sipp_outmoded <- paths$outmoded_sipp_csv_dir

ensure_dir(output_csv_dir_sipp)
ensure_dir(output_csv_dir_sipp_year)
ensure_dir(output_csv_dir_sipp_outmoded)

data_file <- if (file.exists(file.path(data_dir, paste0("pu", analysis_year, ".csv")))) {
  file.path(data_dir, paste0("pu", analysis_year, ".csv"))
} else {
  file.path(legacy_data_dir, paste0("pu", analysis_year, ".csv"))
}
codebook_file <- if (file.exists(file.path(data_dir, paste0(analysis_year, "_SIPP_Data_Dictionary.pdf")))) {
  file.path(data_dir, paste0(analysis_year, "_SIPP_Data_Dictionary.pdf"))
} else {
  file.path(legacy_data_dir, paste0(analysis_year, "_SIPP_Data_Dictionary.pdf"))
}
replicate_weight_file <- if (file.exists(file.path(data_dir, paste0("rw", analysis_year, ".csv")))) {
  file.path(data_dir, paste0("rw", analysis_year, ".csv"))
} else {
  file.path(legacy_data_dir, paste0("rw", analysis_year, ".csv"))
}
replicate_weight_doc_file <- if (file.exists(file.path(data_dir, paste0(analysis_year, "_rw", analysis_year, "_dictionary.txt.pdf")))) {
  file.path(data_dir, paste0(analysis_year, "_rw", analysis_year, "_dictionary.txt.pdf"))
} else {
  file.path(legacy_data_dir, paste0(analysis_year, "_rw", analysis_year, "_dictionary.txt.pdf"))
}

student_debt_var <- NA_character_
race_var <- NA_character_
marital_status_var <- NA_character_
filing_status_var <- NA_character_
household_id_var <- NA_character_
person_id_var <- NA_character_
income_var <- NA_character_
weight_var <- NA_character_
origin_var <- NA_character_

replicate_weight_vars <- NA_character_
replicate_weight_id_vars <- NA_character_
replicate_variance_scale <- NA_real_
replicate_variance_formula_notes <- NA_character_

wealth_anchor_vars_override <- NA_character_
household_size_var <- NA_character_
month_var <- NA_character_
panel_var <- NA_character_
wave_var <- NA_character_
household_status_var <- NA_character_
net_worth_var <- NA_character_
mortgage_debt_var <- NA_character_

use_replicate_weights <- TRUE

output_rate_275 <- file.path(output_csv_dir_sipp_year, paste0("sipp_", analysis_year, "_student_loan_interest_deduction_2_75.csv"))
output_rate_680 <- file.path(output_csv_dir_sipp_year, paste0("sipp_", analysis_year, "_student_loan_interest_deduction_6_80.csv"))
output_combined <- file.path(output_csv_dir_sipp_year, paste0("sipp_", analysis_year, "_student_loan_interest_deduction_combined.csv"))
output_married_cap_full_race <- file.path(output_csv_dir_sipp_year, paste0("sipp_", analysis_year, "_student_loan_interest_deduction_married_cap_comparison_by_race.csv"))
output_married_cap_black_nonblack <- file.path(output_csv_dir_sipp_year, paste0("sipp_", analysis_year, "_student_loan_interest_deduction_married_cap_comparison_black_nonblack.csv"))
current_output_combined <- file.path(output_csv_dir_sipp, "sipp_student_loan_interest_deduction_combined.csv")
current_output_married_cap_full_race <- file.path(output_csv_dir_sipp, "sipp_student_loan_interest_deduction_married_cap_comparison_by_race.csv")
current_output_married_cap_black_nonblack <- file.path(output_csv_dir_sipp, "sipp_student_loan_interest_deduction_married_cap_comparison_black_nonblack.csv")
outmoded_output_rate_275 <- file.path(output_csv_dir_sipp_outmoded, "sipp_student_loan_interest_deduction_2_75.csv")
outmoded_output_rate_680 <- file.path(output_csv_dir_sipp_outmoded, "sipp_student_loan_interest_deduction_6_80.csv")
outmoded_output_combined <- file.path(output_csv_dir_sipp_outmoded, "sipp_student_loan_interest_deduction_combined.csv")

interest_rate_scenarios <- tibble::tribble(
  ~rate_scenario_id, ~rate_scenario,                            ~annual_interest_rate,
  "R275",            "Estimated interest at 2.75% annual rate", 0.0275,
  "R680",            "Estimated interest at 6.8% annual rate",  0.068
)

student_loan_interest_deduction_max <- 2500
student_loan_interest_deduction_max_married_proposed <- 5000
single_hoh_qss_full_magi_max <- 85000
single_hoh_qss_phaseout_end <- 100000
mfj_full_magi_max <- 170000
mfj_phaseout_end <- 200000

single_bracket_10_top <- 11925
single_bracket_12_top <- 48475
single_bracket_22_top <- 103350
single_bracket_24_top <- 197300
single_bracket_32_top <- 250525
single_bracket_35_top <- 626350

hoh_bracket_10_top <- 17000
hoh_bracket_12_top <- 64850
hoh_bracket_22_top <- 103350
hoh_bracket_24_top <- 197300
hoh_bracket_32_top <- 250500
hoh_bracket_35_top <- 626350

mfj_bracket_10_top <- 23850
mfj_bracket_12_top <- 96950
mfj_bracket_22_top <- 206700
mfj_bracket_24_top <- 394600
mfj_bracket_32_top <- 501050
mfj_bracket_35_top <- 751600

zero_payment_income_cutoff <- function(household_size) {
  dplyr::case_when(
    is.na(household_size) ~ NA_real_,
    household_size <= 1 ~ 23500,
    TRUE ~ 23500 + (household_size - 1) * 8200
  )
}

ibr_payment_rate_from_age <- function(age) {
  dplyr::case_when(
    is.na(age) ~ NA_real_,
    age < 34 ~ 0.10,
    TRUE ~ 0.15
  )
}

ibr_repayment_years_from_age <- function(age) {
  dplyr::case_when(
    is.na(age) ~ NA_real_,
    age < 34 ~ 20,
    TRUE ~ 25
  )
}

annual_ibr_payment_from_income <- function(income, household_size, age) {
  discretionary_income <- pmax(income - zero_payment_income_cutoff(household_size), 0)
  discretionary_income * ibr_payment_rate_from_age(age)
}

sipp_married_code_default <- 1
reference_person_code_default <- 101

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(readr)
})

required_pkgs <- c("data.table", "dplyr", "stringr", "tidyr", "purrr", "tibble", "readr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "))
}

script_start_time <- Sys.time()

rule <- function(x = "") {
  cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
}

progress_msg <- function(msg, start_time = NULL) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", msg, sep = "")
  if (!is.null(start_time)) {
    cat(" | elapsed: ", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "s", sep = "")
  }
  cat("\n")
}

first_existing <- function(candidates, names_vec) {
  hit <- candidates[candidates %in% names_vec]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

choose_var <- function(manual, auto) {
  if (!is.na(manual) && nzchar(manual)) manual else auto
}

safe_num <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(x))
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

weighted_median_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  x <- x[ok]
  w <- w[ok]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  x[which(cumsum(w) / sum(w) >= 0.5)[1]]
}

weighted_percent_safe <- function(flag, w) {
  ok <- !is.na(flag) & !is.na(w) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  100 * stats::weighted.mean(as.numeric(flag[ok]), w[ok])
}

unweighted_percent_safe <- function(flag) {
  ok <- !is.na(flag)
  if (!any(ok)) return(NA_real_)
  100 * mean(as.numeric(flag[ok]))
}

race_label <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "Missing race",
    x == 1 ~ "White alone",
    x == 2 ~ "Black alone",
    x == 3 ~ "American Indian / Alaska Native alone",
    x == 4 ~ "Asian alone",
    x == 5 ~ "Native Hawaiian / Pacific Islander alone",
    x == 6 ~ "Some other race alone",
    x %in% c(7, 8, 9, 10) ~ "Multiracial",
    TRUE ~ paste0("Race code ", x)
  )
}

race_label_scf_aligned <- function(race, ethnicity) {
  dplyr::case_when(
    ethnicity == "Hispanic" ~ "Hispanic",
    ethnicity != "Non-Hispanic" ~ NA_character_,
    race == "White alone" ~ "White alone",
    race == "Black alone" ~ "Black alone",
    TRUE ~ "Other"
  )
}

black_nonblack_label <- function(race_detail) {
  dplyr::case_when(
    is.na(race_detail) ~ NA_character_,
    race_detail == "Black alone" ~ "Black",
    TRUE ~ "Non-Black"
  )
}

hispanic_label <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x == 1 ~ "Hispanic",
    x == 2 ~ "Non-Hispanic",
    TRUE ~ NA_character_
  )
}

filing_status_label <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x == 1 ~ "Single",
    x == 2 ~ "Married filing jointly",
    x == 3 ~ "Married filing separately",
    x == 4 ~ "Head of household",
    TRUE ~ NA_character_
  )
}

filing_status_proxy_from_household_group <- function(household_group) {
  dplyr::case_when(
    household_group == "Married household" ~ "Married filing jointly",
    household_group == "Unmarried household" ~ "Single",
    TRUE ~ NA_character_
  )
}

allowable_deduction_from_magi <- function(max_potential_deduction, magi, filing_status) {
  dplyr::case_when(
    is.na(max_potential_deduction) | is.na(magi) | is.na(filing_status) ~ NA_real_,
    filing_status == "Married filing separately" ~ 0,
    filing_status %in% c("Single", "Head of household") & magi <= single_hoh_qss_full_magi_max ~ max_potential_deduction,
    filing_status %in% c("Single", "Head of household") & magi >= single_hoh_qss_phaseout_end ~ 0,
    filing_status == "Married filing jointly" & magi <= mfj_full_magi_max ~ max_potential_deduction,
    filing_status == "Married filing jointly" & magi >= mfj_phaseout_end ~ 0,
    filing_status %in% c("Single", "Head of household") ~ max_potential_deduction * (1 - (magi - single_hoh_qss_full_magi_max) / (single_hoh_qss_phaseout_end - single_hoh_qss_full_magi_max)),
    filing_status == "Married filing jointly" ~ max_potential_deduction * (1 - (magi - mfj_full_magi_max) / (mfj_phaseout_end - mfj_full_magi_max)),
    TRUE ~ NA_real_
  ) %>%
    pmax(0)
}

marginal_tax_rate_from_income <- function(taxable_income_proxy, filing_status) {
  dplyr::case_when(
    is.na(taxable_income_proxy) | is.na(filing_status) ~ NA_real_,
    filing_status == "Married filing separately" ~ NA_real_,
    filing_status == "Single" & taxable_income_proxy <= single_bracket_10_top ~ 0.10,
    filing_status == "Single" & taxable_income_proxy <= single_bracket_12_top ~ 0.12,
    filing_status == "Single" & taxable_income_proxy <= single_bracket_22_top ~ 0.22,
    filing_status == "Single" & taxable_income_proxy <= single_bracket_24_top ~ 0.24,
    filing_status == "Single" & taxable_income_proxy <= single_bracket_32_top ~ 0.32,
    filing_status == "Single" & taxable_income_proxy <= single_bracket_35_top ~ 0.35,
    filing_status == "Single" ~ 0.37,
    filing_status == "Head of household" & taxable_income_proxy <= hoh_bracket_10_top ~ 0.10,
    filing_status == "Head of household" & taxable_income_proxy <= hoh_bracket_12_top ~ 0.12,
    filing_status == "Head of household" & taxable_income_proxy <= hoh_bracket_22_top ~ 0.22,
    filing_status == "Head of household" & taxable_income_proxy <= hoh_bracket_24_top ~ 0.24,
    filing_status == "Head of household" & taxable_income_proxy <= hoh_bracket_32_top ~ 0.32,
    filing_status == "Head of household" & taxable_income_proxy <= hoh_bracket_35_top ~ 0.35,
    filing_status == "Head of household" ~ 0.37,
    filing_status == "Married filing jointly" & taxable_income_proxy <= mfj_bracket_10_top ~ 0.10,
    filing_status == "Married filing jointly" & taxable_income_proxy <= mfj_bracket_12_top ~ 0.12,
    filing_status == "Married filing jointly" & taxable_income_proxy <= mfj_bracket_22_top ~ 0.22,
    filing_status == "Married filing jointly" & taxable_income_proxy <= mfj_bracket_24_top ~ 0.24,
    filing_status == "Married filing jointly" & taxable_income_proxy <= mfj_bracket_32_top ~ 0.32,
    filing_status == "Married filing jointly" & taxable_income_proxy <= mfj_bracket_35_top ~ 0.35,
    filing_status == "Married filing jointly" ~ 0.37,
    TRUE ~ NA_real_
  )
}

extract_text_lines <- function(path) {
  if (!file.exists(path)) return(character(0))
  tryCatch(
    system2("strings", args = c("-n", "8", path), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
}

missing_override_error <- function(var_label) {
  stop(
    "Could not confidently identify ", var_label, ". ",
    "Set ", var_label, " manually in the '# =========================\n# MANUAL VARIABLE OVERRIDES\n# =========================' section."
  )
}

get_delim <- function(path) {
  line <- readLines(path, n = 1L, warn = FALSE)
  if (length(line) == 0) stop("Could not read header from: ", path)
  if (grepl("|", line, fixed = TRUE)) "|" else ","
}

load_header_names <- function(path, delim = NULL) {
  if (is.null(delim)) delim <- get_delim(path)
  data.table::fread(
    input = path,
    sep = delim,
    nrows = 0,
    showProgress = FALSE,
    data.table = FALSE
  ) %>%
    names()
}

load_selected_columns <- function(path, select_cols, delim = NULL) {
  if (is.null(delim)) delim <- get_delim(path)
  if (length(select_cols) == 0) stop("No columns requested from ", path)
  data.table::fread(
    input = path,
    sep = delim,
    select = select_cols,
    showProgress = TRUE,
    data.table = TRUE
  )
}

replicate_se_from_estimates <- function(full_estimate, replicate_estimates, scale) {
  reps <- replicate_estimates[is.finite(replicate_estimates)]
  if (!is.finite(full_estimate) || length(reps) == 0) return(NA_real_)
  sqrt(scale * sum((reps - full_estimate)^2))
}

build_replicate_estimates <- function(df, rep_cols, estimate_fun) {
  if (nrow(df) == 0 || length(rep_cols) == 0) return(rep(NA_real_, length(rep_cols)))
  purrr::map_dbl(rep_cols, function(rc) estimate_fun(df, rc))
}

estimate_weighted_mean <- function(df, weight_col) {
  weighted_mean_safe(df$student_debt, df[[weight_col]])
}

estimate_weighted_median <- function(df, weight_col) {
  weighted_median_safe(df$student_debt, df[[weight_col]])
}

estimate_weighted_mean_positive <- function(df, weight_col) {
  sub <- df %>% filter(positive_student_debt)
  if (nrow(sub) == 0) return(NA_real_)
  weighted_mean_safe(sub$student_debt, sub[[weight_col]])
}

estimate_weighted_median_positive <- function(df, weight_col) {
  sub <- df %>% filter(positive_student_debt)
  if (nrow(sub) == 0) return(NA_real_)
  weighted_median_safe(sub$student_debt, sub[[weight_col]])
}

estimate_weighted_pct <- function(df, flag_col, weight_col) {
  weighted_percent_safe(df[[flag_col]], df[[weight_col]])
}

estimate_weighted_pct_positive <- function(df, flag_col, weight_col) {
  sub <- df %>% filter(positive_student_debt)
  if (nrow(sub) == 0) return(NA_real_)
  weighted_percent_safe(sub[[flag_col]], sub[[weight_col]])
}

estimate_weighted_mean_col_positive <- function(df, value_col, weight_col) {
  sub <- df %>% filter(positive_student_debt)
  if (nrow(sub) == 0) return(NA_real_)
  weighted_mean_safe(sub[[value_col]], sub[[weight_col]])
}

estimate_weighted_population <- function(df, weight_col) {
  sum(df[[weight_col]][!is.na(df[[weight_col]]) & is.finite(df[[weight_col]]) & df[[weight_col]] > 0], na.rm = TRUE)
}

deduction_cap_from_household_group <- function(household_group, married_cap, nonmarried_cap = student_loan_interest_deduction_max) {
  dplyr::case_when(
    household_group == "Married household" ~ married_cap,
    household_group == "Unmarried household" ~ nonmarried_cap,
    TRUE ~ NA_real_
  )
}

rule("Setup")
progress_msg("Script started")

main_delim <- get_delim(data_file)
replicate_doc_lines <- extract_text_lines(replicate_weight_doc_file)
replicate_doc_text <- paste(replicate_doc_lines, collapse = "\n")

progress_msg("Loading headers...")
header_start <- Sys.time()
main_names <- load_header_names(data_file, delim = main_delim)
progress_msg("Main file headers loaded", header_start)

rep_names <- character(0)
rep_names_original <- character(0)
replicate_delim <- NULL
if (use_replicate_weights) {
  if (!file.exists(replicate_weight_file)) {
    stop("Replicate weight file not found: ", replicate_weight_file)
  }
  replicate_delim <- get_delim(replicate_weight_file)
  rep_header_start <- Sys.time()
  rep_names_original <- load_header_names(replicate_weight_file, delim = replicate_delim)
  rep_names <- toupper(rep_names_original)
  progress_msg("Replicate-weight headers loaded", rep_header_start)
}

rule("Variable identification")
progress_msg("Variables identified...")

auto_student_debt <- first_existing(c("THDEBT_ED", "TDEBT_ED", "AHDEBT_ED", "ADEBT_ED", "ARDEBT_ED"), main_names)
auto_race <- first_existing(c("TRACE", "ERACE", "ARACE"), main_names)
auto_marital_status <- first_existing(c("EMS", "TMARPATH", "AMARPATH"), main_names)
auto_filing_status <- first_existing(c("EFSTATUS"), main_names)
auto_household_id <- first_existing(c("SSUID", "SHHADID", "ERESIDENCEID"), main_names)
auto_person_id <- first_existing(c("PNUM"), main_names)
auto_income <- first_existing(c("THTOTINC", "AHTOTINC", "TPTOTINC", "APTOTINC"), main_names)
auto_weight <- first_existing(c("WPFINWGT", "RFAMREFWT2", "RFAMNUMWT2"), main_names)
auto_origin <- first_existing(c("EORIGIN", "AORIGIN"), main_names)
auto_household_size <- first_existing(c("RHNUMPER", "AHNUMPER", "RFPERSONS", "AFPERSONS"), main_names)
auto_age <- first_existing(c("AAGE", "TAGE", "EAGE"), main_names)
auto_month <- first_existing(c("MONTHCODE"), main_names)
auto_panel <- first_existing(c("SPANEL"), main_names)
auto_wave <- first_existing(c("SWAVE"), main_names)
auto_household_status <- first_existing(c("THHLDSTATUS"), main_names)
auto_net_worth <- first_existing(c("THNETWORTH", "TNETWORTH", "AHNETWORTH"), main_names)
auto_mortgage_debt <- first_existing(c("THDEBT_HOME", "TDEBT_HOME", "AHDEBT_HOME", "AMHDEBT", "EMHDEBT"), main_names)

default_wealth_anchor_vars <- c(
  "THDEBT_ED", "TDEBT_ED",
  "THNETWORTH", "TNETWORTH",
  "THDEBT_CC", "TDEBT_CC",
  "THDEBT_OT", "TDEBT_OT",
  "THDEBT_MD", "TDEBT_MD",
  "THDEBT_AST", "TDEBT_AST",
  "THVAL_AST", "TVAL_AST"
)

student_debt_var <- choose_var(student_debt_var, auto_student_debt)
race_var <- choose_var(race_var, auto_race)
marital_status_var <- choose_var(marital_status_var, auto_marital_status)
filing_status_var <- choose_var(filing_status_var, auto_filing_status)
household_id_var <- choose_var(household_id_var, auto_household_id)
person_id_var <- choose_var(person_id_var, auto_person_id)
income_var <- choose_var(income_var, auto_income)
weight_var <- choose_var(weight_var, auto_weight)
origin_var <- choose_var(origin_var, auto_origin)
household_size_var <- choose_var(household_size_var, auto_household_size)
age_var <- choose_var(NA_character_, auto_age)
month_var <- choose_var(month_var, auto_month)
panel_var <- choose_var(panel_var, auto_panel)
wave_var <- choose_var(wave_var, auto_wave)
household_status_var <- choose_var(household_status_var, auto_household_status)
net_worth_var <- choose_var(net_worth_var, auto_net_worth)
mortgage_debt_var <- choose_var(mortgage_debt_var, auto_mortgage_debt)

if (is.na(student_debt_var)) missing_override_error("student_debt_var")
if (is.na(race_var)) missing_override_error("race_var")
if (is.na(marital_status_var)) missing_override_error("marital_status_var")
if (is.na(filing_status_var)) missing_override_error("filing_status_var")
if (is.na(household_id_var)) missing_override_error("household_id_var")
if (is.na(person_id_var)) missing_override_error("person_id_var")
if (is.na(income_var)) missing_override_error("income_var")
if (is.na(weight_var)) missing_override_error("weight_var")
if (is.na(origin_var)) missing_override_error("origin_var")

wealth_anchor_vars <- if (!is.na(wealth_anchor_vars_override) && nzchar(wealth_anchor_vars_override)) {
  str_split(wealth_anchor_vars_override, "\\s*,\\s*")[[1]]
} else {
  default_wealth_anchor_vars[default_wealth_anchor_vars %in% main_names]
}
if (length(wealth_anchor_vars) == 0) {
  stop("Could not identify wealth/debt anchor variables. Set wealth_anchor_vars_override manually.")
}

replicate_id_vars <- character(0)
replicate_weight_cols <- character(0)
full_sample_replicate_weight_var <- NA_character_
replicate_scale <- NA_real_
replicate_formula_notes <- "Replicate weights not used."

if (use_replicate_weights) {
  replicate_id_vars <- if (!is.na(replicate_weight_id_vars) && nzchar(replicate_weight_id_vars)) {
    str_split(replicate_weight_id_vars, "\\s*,\\s*")[[1]]
  } else {
    c(household_id_var, person_id_var, month_var)
  }
  replicate_id_vars <- unique(replicate_id_vars[!is.na(replicate_id_vars)])

  if (!is.na(panel_var) && panel_var %in% rep_names) replicate_id_vars <- unique(c(replicate_id_vars, panel_var))
  if (!is.na(wave_var) && wave_var %in% rep_names) replicate_id_vars <- unique(c(replicate_id_vars, wave_var))

  auto_rep_cols <- rep_names[str_detect(rep_names, "^REPWGT[0-9]+$")]
  replicate_weight_cols <- if (!is.na(replicate_weight_vars) && nzchar(replicate_weight_vars)) {
    str_split(toupper(replicate_weight_vars), "\\s*,\\s*")[[1]]
  } else {
    auto_rep_cols[auto_rep_cols != "REPWGT0"]
  }
  full_sample_replicate_weight_var <- if ("REPWGT0" %in% rep_names) "REPWGT0" else NA_character_

  if (length(replicate_weight_cols) == 0) {
    stop("Could not identify replicate weight variables. Set replicate_weight_vars manually.")
  }
  if (is.na(full_sample_replicate_weight_var)) {
    stop("Could not identify REPWGT0 in the replicate-weight file.")
  }

  replicate_scale <- if (!is.na(replicate_variance_scale)) replicate_variance_scale else 1 / (length(replicate_weight_cols) * 0.5^2)
  replicate_formula_notes <- if (!is.na(replicate_variance_formula_notes) && nzchar(replicate_variance_formula_notes)) {
    replicate_variance_formula_notes
  } else {
    paste0(
      "Fay modified BRR using ", length(replicate_weight_cols),
      " replicates with Fay coefficient 0.5; variance = 1 / (",
      length(replicate_weight_cols), " * 0.5^2) * sum((theta_i - theta_0)^2)."
    )
  }
}

identified_vars <- tibble(
  concept = c(
    "student debt variable",
    "race variable",
    "marital status variable",
    "filing status variable",
    "household id variable",
    "person id variable",
    "income variable",
    "weight variable",
    "origin variable",
    "household size variable",
    "month variable",
    "age variable",
    "net worth variable",
    "mortgage debt variable"
  ),
  value = c(
    student_debt_var,
    race_var,
    marital_status_var,
    filing_status_var,
    household_id_var,
    person_id_var,
    income_var,
    weight_var,
    origin_var,
    household_size_var,
    month_var,
    age_var,
    net_worth_var,
    mortgage_debt_var
  )
)
print(identified_vars, n = nrow(identified_vars))
cat("Wealth/debt anchor vars:\n")
cat(paste0("  - ", wealth_anchor_vars), sep = "\n")
cat("\n")

if (use_replicate_weights) {
  cat("Replicate-weight method: Fay modified BRR\n")
  cat("Replicate-weight count:", length(replicate_weight_cols), "\n")
  cat("Variance formula:", replicate_formula_notes, "\n")
}

rule("Loading required columns only")
main_required_cols <- unique(na.omit(c(
  student_debt_var,
  race_var,
  marital_status_var,
  filing_status_var,
  household_id_var,
  person_id_var,
  income_var,
  weight_var,
  origin_var,
  household_size_var,
  age_var,
  net_worth_var,
  mortgage_debt_var,
  month_var,
  panel_var,
  wave_var,
  household_status_var,
  wealth_anchor_vars
)))

main_load_start <- Sys.time()
progress_msg("Loading required columns only...")
sipp_dt <- load_selected_columns(data_file, main_required_cols, delim = main_delim)
progress_msg("Required columns loaded", main_load_start)

raw_rows <- nrow(sipp_dt)

if (use_replicate_weights) {
  rule("Loading replicate-weight columns only")
  rep_required_cols_upper <- unique(na.omit(c(replicate_id_vars, full_sample_replicate_weight_var, replicate_weight_cols)))
  rep_name_map <- stats::setNames(rep_names_original, rep_names)
  rep_required_cols <- unname(rep_name_map[rep_required_cols_upper])
  rep_required_cols <- rep_required_cols[!is.na(rep_required_cols)]
  if (length(rep_required_cols) == 0) {
    stop("Could not map required replicate-weight columns from normalized names to the actual rw2024.csv header names.")
  }
  rep_load_start <- Sys.time()
  progress_msg("Loading replicate-weight columns only...")
  repw_dt <- load_selected_columns(replicate_weight_file, rep_required_cols, delim = replicate_delim)
  data.table::setnames(repw_dt, old = names(repw_dt), new = toupper(names(repw_dt)))
  progress_msg("Replicate-weight columns loaded", rep_load_start)
}

rule("Merging replicate weights")
if (use_replicate_weights) {
  merge_keys <- replicate_id_vars[replicate_id_vars %in% names(sipp_dt) & replicate_id_vars %in% names(repw_dt)]
  if (length(merge_keys) < 3) {
    stop("Not enough merge keys available between the main file and replicate file. Check replicate_weight_id_vars.")
  }
  merge_start <- Sys.time()
  progress_msg("Merging replicate weights...")
  setkeyv(sipp_dt, merge_keys)
  setkeyv(repw_dt, merge_keys)
  merged_dt <- repw_dt[sipp_dt]
  progress_msg("Replicate weights merged", merge_start)
  merge_success_rate <- mean(!is.na(merged_dt[[full_sample_replicate_weight_var]]))
  cat("Merge keys used:", paste(merge_keys, collapse = ", "), "\n")
  cat("Merge success rate:", round(100 * merge_success_rate, 2), "%\n")
} else {
  merged_dt <- copy(sipp_dt)
  merge_success_rate <- NA_real_
  merge_keys <- character(0)
}

rule("Anchoring on wealth / debt observations")
anchor_start <- Sys.time()
progress_msg("Filtering to wealth/debt observations...")

wealth_observed_flag <- Reduce(
  f = `|`,
  x = lapply(wealth_anchor_vars, function(v) !is.na(merged_dt[[v]])),
  init = rep(FALSE, nrow(merged_dt))
)

anchored_dt <- merged_dt[wealth_observed_flag]
progress_msg("Wealth/debt filter complete", anchor_start)
cat("Rows after anchoring on wealth/debt observations:", format(nrow(anchored_dt), big.mark = ","), "\n")

# We first restrict to observations where wealth/debt variables are observed,
# then deduplicate within that restricted sample. This avoids treating the
# latest arbitrary person-month as the wealth observation.

rule("Deduplicating within wealth sample")
dedupe_start <- Sys.time()
progress_msg("Deduplicating wealth/debt sample...")

anchored_dt[, .household_id := get(household_id_var)]
anchored_dt[, .person_id := get(person_id_var)]
anchored_dt[, .month_order := if (!is.na(month_var) && month_var %in% names(anchored_dt)) safe_num(get(month_var)) else NA_real_]
anchored_dt[, .student_debt := safe_num(get(student_debt_var))]
anchored_dt[, .income := safe_num(get(income_var))]
anchored_dt[, .weight := safe_num(get(weight_var))]
anchored_dt[, .race_code := safe_num(get(race_var))]
anchored_dt[, .marital_code := safe_num(get(marital_status_var))]
anchored_dt[, .filing_status_code := safe_num(get(filing_status_var))]
anchored_dt[, .origin_code := safe_num(get(origin_var))]
anchored_dt[, .hh_size := if (!is.na(household_size_var) && household_size_var %in% names(anchored_dt)) safe_num(get(household_size_var)) else NA_real_]
anchored_dt[, .age := if (!is.na(age_var) && age_var %in% names(anchored_dt)) safe_num(get(age_var)) else NA_real_]
anchored_dt[, .hh_status := if (!is.na(household_status_var) && household_status_var %in% names(anchored_dt)) safe_num(get(household_status_var)) else NA_real_]
anchored_dt[, .net_worth := if (!is.na(net_worth_var) && net_worth_var %in% names(anchored_dt)) safe_num(get(net_worth_var)) else NA_real_]
anchored_dt[, .mortgage_debt := if (!is.na(mortgage_debt_var) && mortgage_debt_var %in% names(anchored_dt)) safe_num(get(mortgage_debt_var)) else NA_real_]
anchored_dt[, .is_reference_person := !is.na(.person_id) & safe_num(.person_id) == reference_person_code_default]

data.table::setorderv(anchored_dt, cols = c(".household_id", ".is_reference_person", ".month_order"), order = c(1, -1, -1), na.last = TRUE)
household_dt <- anchored_dt[, .SD[1], by = .household_id]

progress_msg("Deduplication complete", dedupe_start)
cat("Rows after deduplication to one household observation:", format(nrow(household_dt), big.mark = ","), "\n")

analysis_df <- as_tibble(household_dt) %>%
  transmute(
    year = analysis_year,
    household_id = .household_id,
    student_debt = coalesce(.student_debt, 0),
    student_debt_missing_raw = is.na(.student_debt),
    income = .income,
    race_code = .race_code,
    ethnicity = hispanic_label(.origin_code),
    race_detail = race_label(.race_code),
    race = race_label_scf_aligned(race_detail, ethnicity),
    black_nonblack = black_nonblack_label(race_detail),
    marital_code = .marital_code,
    filing_status_observed = filing_status_label(.filing_status_code),
    household_size = .hh_size,
    age = .age,
    total_net_worth = .net_worth,
    mortgage_debt = .mortgage_debt,
    household_status = .hh_status,
    weight = .weight,
    household_group = case_when(
      !is.na(marital_code) & marital_code == sipp_married_code_default ~ "Married household",
      !is.na(marital_code) ~ "Unmarried household",
      TRUE ~ NA_character_
    ),
    filing_status = filing_status_proxy_from_household_group(household_group),
    zero_payment_cutoff = zero_payment_income_cutoff(household_size),
    ibr_payment_rate = ibr_payment_rate_from_age(age),
    ibr_repayment_years = ibr_repayment_years_from_age(age),
    annual_ibr_payment = annual_ibr_payment_from_income(income, household_size, age),
    monthly_ibr_payment = annual_ibr_payment / 12
  )

if (use_replicate_weights) {
  for (rc in replicate_weight_cols) {
    analysis_df[[rc]] <- safe_num(household_dt[[rc]])
  }
}

rule("Diagnostics")
cat("Identified student debt variable:", student_debt_var, "\n")
cat("Identified race variable:", race_var, "\n")
cat("Identified marital status variable:", marital_status_var, "\n")
cat("Identified filing status variable:", filing_status_var, "\n")
cat("Identified income variable:", income_var, "\n")
cat("Identified weight variable:", weight_var, "\n")
cat("Identified origin variable:", origin_var, "\n")
cat("MAGI proxy used for tax deduction phaseout:", income_var, "\n")
cat("Number of raw rows:", format(raw_rows, big.mark = ","), "\n")
cat("Number of rows after anchoring on wealth/debt observations:", format(nrow(anchored_dt), big.mark = ","), "\n")
cat("Number of rows after deduplication:", format(nrow(analysis_df), big.mark = ","), "\n")
cat("Share missing student debt:", round(mean(analysis_df$student_debt_missing_raw), 4), "\n")
cat("Share with positive student debt:", round(mean(analysis_df$student_debt > 0, na.rm = TRUE), 4), "\n")
if (use_replicate_weights) {
  cat("Number of replicate weights found:", length(replicate_weight_cols), "\n")
  cat("Merge success rate:", round(100 * merge_success_rate, 2), "%\n")
  cat("Replicate-weight variance formula used:", replicate_formula_notes, "\n")
  cat("Fay/scaling adjustment applied: Fay coefficient 0.5\n")
} else {
  cat("Replicate weights used: FALSE\n")
}

eligible_base <- analysis_df %>%
  filter(
    !is.na(household_group),
    !is.na(income),
    !is.na(age),
    !is.na(household_size),
    !is.na(race),
    !is.na(ethnicity),
    !is.na(filing_status),
    income > zero_payment_cutoff,
    (
      household_group == "Unmarried household" & income < single_hoh_qss_phaseout_end
    ) |
      (
        household_group == "Married household" & income < mfj_phaseout_end
      )
  )

cat("Eligible rows before interest-rate scenarios:", format(nrow(eligible_base), big.mark = ","), "\n")

prefilter_harmonized_output_file <- file.path(paths$sipp_harmonized_dir, paste0("sipp_household_year_", analysis_year, "_prefilter.rds"))
saveRDS(analysis_df, prefilter_harmonized_output_file)
cat("Wrote pre-filter SIPP household-year file:", prefilter_harmonized_output_file, "\n")

harmonized_output_file <- file.path(paths$sipp_harmonized_dir, paste0("sipp_household_year_", analysis_year, ".rds"))
saveRDS(eligible_base, harmonized_output_file)
cat("Wrote harmonized SIPP household-year file:", harmonized_output_file, "\n")

summarise_rate_scenario <- function(df, rate_scenario_id, rate_scenario_name, annual_interest_rate, rep_cols, variance_scale, use_rep_weights) {
  scenario_df <- df %>%
    mutate(
      rate_scenario_id = rate_scenario_id,
      rate_scenario = rate_scenario_name,
      annual_interest_rate = annual_interest_rate,
      positive_student_debt = student_debt > 0,
      accrued_interest = if_else(positive_student_debt, student_debt * annual_interest_rate, 0),
      interest_paid = if_else(positive_student_debt, pmin(accrued_interest, annual_ibr_payment), 0),
      max_potential_deduction = pmin(interest_paid, student_loan_interest_deduction_max),
      allowable_deduction = allowable_deduction_from_magi(max_potential_deduction, income, filing_status),
      marginal_tax_rate = marginal_tax_rate_from_income(income, filing_status),
      estimated_tax_savings = allowable_deduction * marginal_tax_rate,
      any_allowable_deduction = allowable_deduction > 0,
      full_deduction = allowable_deduction >= max_potential_deduction & max_potential_deduction > 0
    )

  cat("Eligible households/persons in", rate_scenario_name, ":", format(nrow(scenario_df), big.mark = ","), "\n")

  se_issue_groups <- character(0)

  result <- scenario_df %>%
    group_by(rate_scenario_id, rate_scenario, annual_interest_rate, ethnicity, race, household_group) %>%
    group_modify(function(.x, .y) {
      mean_student_debt <- estimate_weighted_mean_positive(.x, "weight")
      mean_interest_paid <- estimate_weighted_mean_col_positive(.x, "interest_paid", "weight")
      mean_max_potential_deduction <- estimate_weighted_mean_col_positive(.x, "max_potential_deduction", "weight")
      mean_allowable_deduction <- estimate_weighted_mean_col_positive(.x, "allowable_deduction", "weight")
      mean_estimated_tax_savings <- estimate_weighted_mean_col_positive(.x, "estimated_tax_savings", "weight")
      pct_borrowers_with_any_allowable_deduction <- estimate_weighted_pct_positive(.x, "any_allowable_deduction", "weight")
      pct_borrowers_with_full_deduction <- estimate_weighted_pct_positive(.x, "full_deduction", "weight")
      weighted_population <- estimate_weighted_population(.x, "weight")

      se_weighted_population <- NA_real_
      se_mean_student_debt <- NA_real_
      se_mean_interest_paid <- NA_real_
      se_mean_max_potential_deduction <- NA_real_
      se_mean_allowable_deduction <- NA_real_
      se_mean_estimated_tax_savings <- NA_real_
      se_pct_borrowers_with_any_allowable_deduction <- NA_real_
      se_pct_borrowers_with_full_deduction <- NA_real_

      if (use_rep_weights) {
        weighted_population_reps <- build_replicate_estimates(.x, rep_cols, estimate_weighted_population)
        student_debt_reps <- build_replicate_estimates(.x, rep_cols, estimate_weighted_mean_positive)
        interest_paid_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) estimate_weighted_mean_col_positive(d, "interest_paid", rc))
        max_deduction_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) estimate_weighted_mean_col_positive(d, "max_potential_deduction", rc))
        allowable_deduction_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) estimate_weighted_mean_col_positive(d, "allowable_deduction", rc))
        tax_savings_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) estimate_weighted_mean_col_positive(d, "estimated_tax_savings", rc))
        any_allowable_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) estimate_weighted_pct_positive(d, "any_allowable_deduction", rc))
        full_deduction_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) estimate_weighted_pct_positive(d, "full_deduction", rc))

        se_weighted_population <- replicate_se_from_estimates(weighted_population, weighted_population_reps, variance_scale)
        se_mean_student_debt <- replicate_se_from_estimates(mean_student_debt, student_debt_reps, variance_scale)
        se_mean_interest_paid <- replicate_se_from_estimates(mean_interest_paid, interest_paid_reps, variance_scale)
        se_mean_max_potential_deduction <- replicate_se_from_estimates(mean_max_potential_deduction, max_deduction_reps, variance_scale)
        se_mean_allowable_deduction <- replicate_se_from_estimates(mean_allowable_deduction, allowable_deduction_reps, variance_scale)
        se_mean_estimated_tax_savings <- replicate_se_from_estimates(mean_estimated_tax_savings, tax_savings_reps, variance_scale)
        se_pct_borrowers_with_any_allowable_deduction <- replicate_se_from_estimates(pct_borrowers_with_any_allowable_deduction, any_allowable_reps, variance_scale)
        se_pct_borrowers_with_full_deduction <- replicate_se_from_estimates(pct_borrowers_with_full_deduction, full_deduction_reps, variance_scale)
      }

      if (!is.finite(se_mean_allowable_deduction)) {
        se_issue_groups <<- c(se_issue_groups, paste(.y$rate_scenario, .y$race, .y$household_group, sep = " | "))
      }

      tibble(
        n_obs = nrow(.x),
        n_positive_student_debt = sum(.x$positive_student_debt, na.rm = TRUE),
        weighted_population = weighted_population,
        se_weighted_population = se_weighted_population,
        mean_student_debt = mean_student_debt,
        se_mean_student_debt = se_mean_student_debt,
        mean_interest_paid = mean_interest_paid,
        se_mean_interest_paid = se_mean_interest_paid,
        mean_max_potential_deduction = mean_max_potential_deduction,
        se_mean_max_potential_deduction = se_mean_max_potential_deduction,
        mean_allowable_deduction = mean_allowable_deduction,
        se_mean_allowable_deduction = se_mean_allowable_deduction,
        mean_estimated_tax_savings = mean_estimated_tax_savings,
        se_mean_estimated_tax_savings = se_mean_estimated_tax_savings,
        pct_borrowers_with_any_allowable_deduction = pct_borrowers_with_any_allowable_deduction,
        se_pct_borrowers_with_any_allowable_deduction = se_pct_borrowers_with_any_allowable_deduction,
        pct_borrowers_with_full_deduction = pct_borrowers_with_full_deduction,
        se_pct_borrowers_with_full_deduction = se_pct_borrowers_with_full_deduction
      )
    }) %>%
    ungroup() %>%
    arrange(rate_scenario_id, household_group, ethnicity, race)

  attr(result, "se_issue_groups") <- unique(se_issue_groups)
  result
}

build_married_cap_comparison <- function(df, group_var, rep_cols, variance_scale, use_rep_weights) {
  scenario_results <- vector("list", nrow(interest_rate_scenarios))
  se_issue_groups <- character(0)

  for (i in seq_len(nrow(interest_rate_scenarios))) {
    scenario_df <- df %>%
      mutate(
        rate_scenario_id = interest_rate_scenarios$rate_scenario_id[[i]],
        rate_scenario = interest_rate_scenarios$rate_scenario[[i]],
        annual_interest_rate = interest_rate_scenarios$annual_interest_rate[[i]],
        comparison_group = .data[[group_var]],
        positive_student_debt = student_debt > 0,
        accrued_interest = if_else(positive_student_debt, student_debt * annual_interest_rate, 0),
        interest_paid = if_else(positive_student_debt, pmin(accrued_interest, annual_ibr_payment), 0),
        baseline_cap = deduction_cap_from_household_group(household_group, married_cap = student_loan_interest_deduction_max),
        proposed_cap = deduction_cap_from_household_group(household_group, married_cap = student_loan_interest_deduction_max_married_proposed),
        allowable_deduction_baseline = allowable_deduction_from_magi(pmin(interest_paid, baseline_cap), income, filing_status),
        allowable_deduction_proposed = allowable_deduction_from_magi(pmin(interest_paid, proposed_cap), income, filing_status),
        marginal_tax_rate = marginal_tax_rate_from_income(income, filing_status),
        deduction_gain = allowable_deduction_proposed - allowable_deduction_baseline,
        tax_savings_baseline = allowable_deduction_baseline * marginal_tax_rate,
        tax_savings_proposed = allowable_deduction_proposed * marginal_tax_rate,
        tax_savings_gain = tax_savings_proposed - tax_savings_baseline,
        at_baseline_2500_limit = positive_student_debt & abs(allowable_deduction_baseline - student_loan_interest_deduction_max) < 1e-8,
        positive_gain = deduction_gain > 0
      ) %>%
      filter(!is.na(comparison_group))

    scenario_results[[i]] <- scenario_df %>%
      group_by(rate_scenario_id, rate_scenario, annual_interest_rate, household_group, comparison_group) %>%
      group_modify(function(.x, .y) {
        weighted_population <- sum(.x$weight[!is.na(.x$weight) & is.finite(.x$weight) & .x$weight > 0], na.rm = TRUE)
        mean_student_debt <- weighted_mean_safe(.x$student_debt, .x$weight)
        median_student_debt <- weighted_median_safe(.x$student_debt, .x$weight)
        mean_allowable_deduction_baseline <- weighted_mean_safe(.x$allowable_deduction_baseline, .x$weight)
        mean_allowable_deduction_proposed <- weighted_mean_safe(.x$allowable_deduction_proposed, .x$weight)
        mean_deduction_gain <- weighted_mean_safe(.x$deduction_gain, .x$weight)
        median_deduction_gain <- weighted_median_safe(.x$deduction_gain, .x$weight)
        pct_borrowers_at_2500_limit_baseline <- weighted_percent_safe(if_else(.x$positive_student_debt, .x$at_baseline_2500_limit, NA), .x$weight)
        pct_households_with_positive_deduction_gain <- weighted_percent_safe(.x$positive_gain, .x$weight)
        mean_tax_savings_baseline <- weighted_mean_safe(.x$tax_savings_baseline, .x$weight)
        mean_tax_savings_proposed <- weighted_mean_safe(.x$tax_savings_proposed, .x$weight)
        mean_tax_savings_gain <- weighted_mean_safe(.x$tax_savings_gain, .x$weight)
        median_tax_savings_gain <- weighted_median_safe(.x$tax_savings_gain, .x$weight)
        pct_households_with_positive_tax_savings_gain <- weighted_percent_safe(.x$positive_gain, .x$weight)

        se_weighted_population <- NA_real_
        se_mean_student_debt <- NA_real_
        se_mean_allowable_deduction_baseline <- NA_real_
        se_mean_allowable_deduction_proposed <- NA_real_
        se_mean_deduction_gain <- NA_real_
        se_pct_borrowers_at_2500_limit_baseline <- NA_real_
        se_mean_tax_savings_baseline <- NA_real_
        se_mean_tax_savings_proposed <- NA_real_
        se_mean_tax_savings_gain <- NA_real_

        if (use_rep_weights && length(rep_cols) > 0) {
          weighted_population_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) sum(d[[rc]][!is.na(d[[rc]]) & is.finite(d[[rc]]) & d[[rc]] > 0], na.rm = TRUE))
          student_debt_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) weighted_mean_safe(d$student_debt, d[[rc]]))
          baseline_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) weighted_mean_safe(d$allowable_deduction_baseline, d[[rc]]))
          proposed_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) weighted_mean_safe(d$allowable_deduction_proposed, d[[rc]]))
          gain_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) weighted_mean_safe(d$deduction_gain, d[[rc]]))
          cap_share_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) weighted_percent_safe(if_else(d$positive_student_debt, d$at_baseline_2500_limit, NA), d[[rc]]))
          tax_baseline_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) weighted_mean_safe(d$tax_savings_baseline, d[[rc]]))
          tax_proposed_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) weighted_mean_safe(d$tax_savings_proposed, d[[rc]]))
          tax_gain_reps <- build_replicate_estimates(.x, rep_cols, function(d, rc) weighted_mean_safe(d$tax_savings_gain, d[[rc]]))

          se_weighted_population <- replicate_se_from_estimates(weighted_population, weighted_population_reps, variance_scale)
          se_mean_student_debt <- replicate_se_from_estimates(mean_student_debt, student_debt_reps, variance_scale)
          se_mean_allowable_deduction_baseline <- replicate_se_from_estimates(mean_allowable_deduction_baseline, baseline_reps, variance_scale)
          se_mean_allowable_deduction_proposed <- replicate_se_from_estimates(mean_allowable_deduction_proposed, proposed_reps, variance_scale)
          se_mean_deduction_gain <- replicate_se_from_estimates(mean_deduction_gain, gain_reps, variance_scale)
          se_pct_borrowers_at_2500_limit_baseline <- replicate_se_from_estimates(pct_borrowers_at_2500_limit_baseline, cap_share_reps, variance_scale)
          se_mean_tax_savings_baseline <- replicate_se_from_estimates(mean_tax_savings_baseline, tax_baseline_reps, variance_scale)
          se_mean_tax_savings_proposed <- replicate_se_from_estimates(mean_tax_savings_proposed, tax_proposed_reps, variance_scale)
          se_mean_tax_savings_gain <- replicate_se_from_estimates(mean_tax_savings_gain, tax_gain_reps, variance_scale)
        }

        if (!is.finite(se_mean_deduction_gain) && use_rep_weights) {
          se_issue_groups <<- c(se_issue_groups, paste(.y$rate_scenario, .y$comparison_group, .y$household_group, sep = " | "))
        }

        tibble(
          n_obs = nrow(.x),
          n_positive_student_debt = sum(.x$positive_student_debt, na.rm = TRUE),
          weighted_population = weighted_population,
          `SE: weighted population` = se_weighted_population,
          mean_student_debt = mean_student_debt,
          `SE: mean student debt` = se_mean_student_debt,
          median_student_debt = median_student_debt,
          mean_allowable_deduction_baseline = mean_allowable_deduction_baseline,
          `SE: mean allowable deduction baseline` = se_mean_allowable_deduction_baseline,
          mean_allowable_deduction_proposed = mean_allowable_deduction_proposed,
          `SE: mean allowable deduction proposed` = se_mean_allowable_deduction_proposed,
          mean_deduction_gain = mean_deduction_gain,
          `SE: mean deduction gain` = se_mean_deduction_gain,
          median_deduction_gain = median_deduction_gain,
          pct_borrowers_at_2500_limit_baseline = pct_borrowers_at_2500_limit_baseline,
          `SE: % borrowers at $2500 limit baseline` = se_pct_borrowers_at_2500_limit_baseline,
          pct_households_with_positive_deduction_gain = pct_households_with_positive_deduction_gain,
          mean_tax_savings_baseline = mean_tax_savings_baseline,
          `SE: mean tax savings baseline` = se_mean_tax_savings_baseline,
          mean_tax_savings_proposed = mean_tax_savings_proposed,
          `SE: mean tax savings proposed` = se_mean_tax_savings_proposed,
          mean_tax_savings_gain = mean_tax_savings_gain,
          `SE: mean tax savings gain` = se_mean_tax_savings_gain,
          median_tax_savings_gain = median_tax_savings_gain,
          pct_households_with_positive_tax_savings_gain = pct_households_with_positive_tax_savings_gain
        )
      }) %>%
      ungroup() %>%
      arrange(rate_scenario_id, household_group, comparison_group)
  }

  result <- bind_rows(scenario_results)
  attr(result, "se_issue_groups") <- unique(se_issue_groups)
  result
}

rule("Interest deduction summaries")
summary_start <- Sys.time()
progress_msg("Computing interest deduction summaries...")

scenario_results <- vector("list", nrow(interest_rate_scenarios))
se_issue_groups_all <- character(0)

for (i in seq_len(nrow(interest_rate_scenarios))) {
  res <- summarise_rate_scenario(
    df = eligible_base,
    rate_scenario_id = interest_rate_scenarios$rate_scenario_id[[i]],
    rate_scenario_name = interest_rate_scenarios$rate_scenario[[i]],
    annual_interest_rate = interest_rate_scenarios$annual_interest_rate[[i]],
    rep_cols = replicate_weight_cols,
    variance_scale = replicate_scale,
    use_rep_weights = use_replicate_weights
  )
  scenario_results[[i]] <- res
  se_issue_groups_all <- c(se_issue_groups_all, attr(res, "se_issue_groups"))
}

summary_combined <- bind_rows(scenario_results)

summary_combined <- summary_combined %>%
  mutate(year = analysis_year, source = "SIPP") %>%
  rename(
    `SE: weighted population` = se_weighted_population,
    `SE: mean student debt` = se_mean_student_debt,
    `Estimated annual interest paid` = mean_interest_paid,
    `SE: estimated annual interest paid` = se_mean_interest_paid,
    `Max potential deduction` = mean_max_potential_deduction,
    `SE: max potential deduction` = se_mean_max_potential_deduction,
    `Allowable deduction after MAGI phaseout` = mean_allowable_deduction,
    `SE: allowable deduction after MAGI phaseout` = se_mean_allowable_deduction,
    `Estimated tax savings` = mean_estimated_tax_savings,
    `SE: estimated tax savings` = se_mean_estimated_tax_savings,
    `% of borrowers with any allowable deduction` = pct_borrowers_with_any_allowable_deduction,
    `SE: % of borrowers with any allowable deduction` = se_pct_borrowers_with_any_allowable_deduction,
    `% of borrowers with full deduction` = pct_borrowers_with_full_deduction,
    `SE: % of borrowers with full deduction` = se_pct_borrowers_with_full_deduction
  )

summary_rate_275 <- summary_combined %>% filter(rate_scenario_id == "R275")
summary_rate_680 <- summary_combined %>% filter(rate_scenario_id == "R680")

progress_msg("Interest deduction summaries complete", summary_start)

rule("Married-cap comparison summaries")
comparison_start <- Sys.time()
progress_msg("Computing married-cap comparison summaries...")

married_cap_comparison_by_race <- build_married_cap_comparison(
  eligible_base,
  "race",
  rep_cols = replicate_weight_cols,
  variance_scale = replicate_scale,
  use_rep_weights = use_replicate_weights
) %>%
  mutate(year = analysis_year, source = "SIPP") %>%
  rename(race = comparison_group)

married_cap_comparison_black_nonblack <- build_married_cap_comparison(
  eligible_base,
  "black_nonblack",
  rep_cols = replicate_weight_cols,
  variance_scale = replicate_scale,
  use_rep_weights = use_replicate_weights
) %>%
  mutate(year = analysis_year, source = "SIPP") %>%
  rename(black_nonblack = comparison_group)

progress_msg("Married-cap comparison summaries complete", comparison_start)

rule("Console output: Estimated interest at 2.75% annual rate")
print(summary_rate_275, n = nrow(summary_rate_275), width = Inf)

rule("Console output: Estimated interest at 6.8% annual rate")
print(summary_rate_680, n = nrow(summary_rate_680), width = Inf)

rule("SE diagnostics")
if (!use_replicate_weights) {
  cat("Replicate weights disabled; SE columns are NA.\n")
} else if (length(unique(se_issue_groups_all)) == 0) {
  cat("All grouped standard errors were computed.\n")
} else {
  cat("Groups where one or more SEs could not be computed:\n")
  cat(paste0("  - ", unique(se_issue_groups_all)), sep = "\n")
  cat("\n")
}

rule("Writing CSV outputs")
write_start <- Sys.time()
progress_msg("Writing output CSVs...")
readr::write_csv(summary_rate_275, output_rate_275, na = "")
readr::write_csv(summary_rate_680, output_rate_680, na = "")
readr::write_csv(summary_combined, output_combined, na = "")
readr::write_csv(married_cap_comparison_by_race, output_married_cap_full_race, na = "")
readr::write_csv(married_cap_comparison_black_nonblack, output_married_cap_black_nonblack, na = "")
if (analysis_year == 2024) {
  readr::write_csv(married_cap_comparison_by_race, current_output_married_cap_full_race, na = "")
  readr::write_csv(married_cap_comparison_black_nonblack, current_output_married_cap_black_nonblack, na = "")
  readr::write_csv(summary_rate_275, outmoded_output_rate_275, na = "")
  readr::write_csv(summary_rate_680, outmoded_output_rate_680, na = "")
  readr::write_csv(summary_combined, outmoded_output_combined, na = "")
}
progress_msg("Output CSVs written", write_start)

cat("Wrote:", output_rate_275, "\n")
cat("Wrote:", output_rate_680, "\n")
cat("Wrote:", output_combined, "\n")
cat("Wrote:", output_married_cap_full_race, "\n")
cat("Wrote:", output_married_cap_black_nonblack, "\n")

rule("Timing")
cat("Total runtime:", round(as.numeric(difftime(Sys.time(), script_start_time, units = "secs")), 2), "seconds\n")

rule("Notes")
cat(
  "This script loads headers first, identifies variables, and then reloads only the required columns.\n",
  "It uses data.table::fread() for the main SIPP file and replicate-weight file.\n",
  "Replicate weights are optional via use_replicate_weights <- TRUE/FALSE.\n",
  "Student loan interest deduction calculations use IRS 2025 MAGI phaseout rules and estimated annual interest paid based on the rate scenario.\n",
  "The script also compares the baseline $2,500 deduction cap with a proposed policy that raises the married-household cap to $5,000 while keeping the nonmarried cap at $2,500.\n",
  sep = ""
)
