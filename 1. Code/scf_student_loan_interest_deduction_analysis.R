#!/usr/bin/env Rscript

raw_script_path <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1])
script_path <- normalizePath(gsub("~\\+~", " ", raw_script_path), winslash = "/", mustWork = FALSE)
code_dir <- dirname(script_path)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(code_dir, "student_loan_interest_workflow_utils.R"))
paths <- get_workflow_paths(project_dir)
analysis_year <- parse_year_arg(2022)
year_suffix <- substr(as.character(analysis_year), 3, 4)
data_dir <- file.path(paths$scf_raw_dir, analysis_year)
legacy_data_dir <- file.path(project_dir, "0. Data", "SCF")
output_csv_dir_scf <- paths$scf_csv_dir
output_csv_dir_scf_year <- file.path(output_csv_dir_scf, "by-year", analysis_year)
output_csv_dir_scf_outmoded <- paths$outmoded_scf_csv_dir

ensure_dir(output_csv_dir_scf)
ensure_dir(output_csv_dir_scf_year)
ensure_dir(output_csv_dir_scf_outmoded)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(readr)
})

main_file <- if (file.exists(file.path(data_dir, paste0("p", year_suffix, "i6.dta")))) {
  file.path(data_dir, paste0("p", year_suffix, "i6.dta"))
} else {
  file.path(legacy_data_dir, paste0("p", year_suffix, "i6.dta"))
}
replicate_file <- if (file.exists(file.path(data_dir, paste0("p", year_suffix, "_rw1.dta")))) {
  file.path(data_dir, paste0("p", year_suffix, "_rw1.dta"))
} else {
  file.path(legacy_data_dir, paste0("p", year_suffix, "_rw1.dta"))
}
rds_file <- file.path(data_dir, paste0("scf", analysis_year, ".rds"))

output_rate_275 <- file.path(output_csv_dir_scf_year, paste0("scf_", analysis_year, "_student_loan_interest_deduction_2_75.csv"))
output_rate_680 <- file.path(output_csv_dir_scf_year, paste0("scf_", analysis_year, "_student_loan_interest_deduction_6_80.csv"))
output_combined <- file.path(output_csv_dir_scf_year, paste0("scf_", analysis_year, "_student_loan_interest_deduction_combined.csv"))
output_married_cap_full_race <- file.path(output_csv_dir_scf_year, paste0("scf_", analysis_year, "_student_loan_interest_deduction_married_cap_comparison_by_race.csv"))
output_married_cap_black_nonblack <- file.path(output_csv_dir_scf_year, paste0("scf_", analysis_year, "_student_loan_interest_deduction_married_cap_comparison_black_nonblack.csv"))
current_output_combined <- file.path(output_csv_dir_scf, "scf_student_loan_interest_deduction_combined.csv")
current_output_married_cap_full_race <- file.path(output_csv_dir_scf, "scf_student_loan_interest_deduction_married_cap_comparison_by_race.csv")
current_output_married_cap_black_nonblack <- file.path(output_csv_dir_scf, "scf_student_loan_interest_deduction_married_cap_comparison_black_nonblack.csv")
outmoded_output_rate_275 <- file.path(output_csv_dir_scf_outmoded, "scf_student_loan_interest_deduction_2_75.csv")
outmoded_output_rate_680 <- file.path(output_csv_dir_scf_outmoded, "scf_student_loan_interest_deduction_6_80.csv")
outmoded_output_combined <- file.path(output_csv_dir_scf_outmoded, "scf_student_loan_interest_deduction_combined.csv")

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
  case_when(
    is.na(household_size) ~ NA_real_,
    household_size <= 1 ~ 23500,
    TRUE ~ 23500 + (household_size - 1) * 8200
  )
}

ibr_payment_rate_from_age <- function(age) {
  case_when(
    is.na(age) ~ NA_real_,
    age < 34 ~ 0.10,
    TRUE ~ 0.15
  )
}

ibr_repayment_years_from_age <- function(age) {
  case_when(
    is.na(age) ~ NA_real_,
    age < 34 ~ 20,
    TRUE ~ 25
  )
}

annual_ibr_payment_from_income <- function(income, household_size, age) {
  discretionary_income <- pmax(income - zero_payment_income_cutoff(household_size), 0)
  discretionary_income * ibr_payment_rate_from_age(age)
}

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

race_ethnicity_label <- function(hispanic_flag, race_1, race_2) {
  case_when(
    hispanic_flag == 1 ~ "Hispanic",
    race_1 == 1 & race_2 == 5 ~ "White alone",
    race_1 == 2 & race_2 == 5 ~ "Black alone",
    race_1 == 4 & race_2 == 5 ~ "Other",
    race_1 == 3 | race_2 == 1 | race_1 %in% c(-7, 5) ~ "Other",
    TRUE ~ "Other"
  )
}

black_nonblack_label <- function(race_1, race_2) {
  case_when(
    is.na(race_1) & is.na(race_2) ~ NA_character_,
    race_1 == 2 | race_2 == 2 ~ "Black",
    TRUE ~ "Non-Black"
  )
}

ethnicity_label <- function(hispanic_flag) {
  case_when(
    hispanic_flag == 1 ~ "Hispanic",
    hispanic_flag == 5 ~ "Non-Hispanic",
    TRUE ~ NA_character_
  )
}

filing_status_proxy <- function(marital_status_recode) {
  case_when(
    marital_status_recode %in% c(1, 2, 3, 4, 5, 6) ~ "Married filing jointly",
    marital_status_recode %in% c(7, 8, 9, 10, 11, 12) ~ "Single/Head of household",
    TRUE ~ "Single/Head of household"
  )
}

household_group_label <- function(marital_status_recode) {
  case_when(
    marital_status_recode %in% c(1, 2, 3, 4, 5, 6) ~ "Married household",
    !is.na(marital_status_recode) ~ "Unmarried household",
    TRUE ~ NA_character_
  )
}

allowable_deduction_from_magi <- function(max_potential_deduction, magi, filing_status) {
  case_when(
    is.na(max_potential_deduction) | is.na(magi) | is.na(filing_status) ~ NA_real_,
    filing_status == "Single/Head of household" & magi <= single_hoh_qss_full_magi_max ~ max_potential_deduction,
    filing_status == "Single/Head of household" & magi >= single_hoh_qss_phaseout_end ~ 0,
    filing_status == "Married filing jointly" & magi <= mfj_full_magi_max ~ max_potential_deduction,
    filing_status == "Married filing jointly" & magi >= mfj_phaseout_end ~ 0,
    filing_status == "Single/Head of household" ~ max_potential_deduction * (1 - (magi - single_hoh_qss_full_magi_max) / (single_hoh_qss_phaseout_end - single_hoh_qss_full_magi_max)),
    filing_status == "Married filing jointly" ~ max_potential_deduction * (1 - (magi - mfj_full_magi_max) / (mfj_phaseout_end - mfj_full_magi_max)),
    TRUE ~ NA_real_
  ) %>%
    pmax(0)
}

marginal_tax_rate_from_income <- function(taxable_income_proxy, filing_status) {
  case_when(
    is.na(taxable_income_proxy) | is.na(filing_status) ~ NA_real_,
    filing_status == "Single/Head of household" & taxable_income_proxy <= single_bracket_10_top ~ 0.10,
    filing_status == "Single/Head of household" & taxable_income_proxy <= single_bracket_12_top ~ 0.12,
    filing_status == "Single/Head of household" & taxable_income_proxy <= single_bracket_22_top ~ 0.22,
    filing_status == "Single/Head of household" & taxable_income_proxy <= single_bracket_24_top ~ 0.24,
    filing_status == "Single/Head of household" & taxable_income_proxy <= single_bracket_32_top ~ 0.32,
    filing_status == "Single/Head of household" & taxable_income_proxy <= single_bracket_35_top ~ 0.35,
    filing_status == "Single/Head of household" ~ 0.37,
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

deduction_cap_from_household_group <- function(household_group, married_cap, nonmarried_cap = student_loan_interest_deduction_max) {
  case_when(
    household_group == "Married household" ~ married_cap,
    household_group == "Unmarried household" ~ nonmarried_cap,
    TRUE ~ NA_real_
  )
}

rule("Load SCF")
load_start <- Sys.time()
progress_msg("Reading SCF implicates and replicate weights...")
if (file.exists(rds_file)) {
  scf_list <- readRDS(rds_file)
  scf_raw <- bind_rows(scf_list)
  rw_raw <- NULL
} else {
  scf_raw <- read_dta(main_file)
  rw_raw <- read_dta(replicate_file)
}

progress_msg("SCF files loaded", load_start)

rule("Prepare SCF")

loan_balance_vars <- c("x7824", "x7847", "x7870", "x7924", "x7947", "x7970", "x8440")
loan_payment_vars <- c("x7815", "x7838", "x7861", "x7915", "x7938", "x7961", "x8441")
loan_rate_vars <- c("x7822", "x7845", "x7868", "x7922", "x7945", "x7968")

available_balance_vars <- loan_balance_vars[loan_balance_vars %in% names(scf_raw)]
available_payment_vars <- loan_payment_vars[loan_payment_vars %in% names(scf_raw)]
available_rate_vars <- loan_rate_vars[loan_rate_vars %in% names(scf_raw)]
replicate_cols_raw <- names(scf_raw)[grepl("^wt1b[0-9]+$", names(scf_raw))]

scf_df <- scf_raw %>%
  transmute(
    year = analysis_year,
    family_id = as.numeric(yy1),
    implicate_id = as.numeric(y1),
    implicate_number = implicate_id %% 10,
    weight = as.numeric(x42001),
    age = as.numeric(x14),
    income = as.numeric(x5729),
    household_size = as.numeric(x101),
    filed_return = as.numeric(x5744),
    marital_status_recode = as.numeric(x7019),
    hispanic_flag = as.numeric(x7004),
    race_1 = as.numeric(x6809),
    race_2 = as.numeric(x6810),
    student_debt = rowSums(across(all_of(available_balance_vars), ~ pmax(as.numeric(.x), 0, na.rm = TRUE)), na.rm = TRUE),
    total_net_worth = as.numeric(networth),
    mortgage_debt = as.numeric(nh_mort),
    annual_payment_proxy = rowSums(across(all_of(available_payment_vars), ~ pmax(as.numeric(.x), 0, na.rm = TRUE)), na.rm = TRUE),
    avg_apr_reported = if (length(available_rate_vars) > 0) rowMeans(across(all_of(available_rate_vars), ~ na_if(as.numeric(.x), 0)), na.rm = TRUE) else NA_real_,
    ethnicity = ethnicity_label(hispanic_flag),
    race = race_ethnicity_label(hispanic_flag, race_1, race_2),
    black_nonblack = black_nonblack_label(race_1, race_2),
    household_group = household_group_label(marital_status_recode),
    filing_status = filing_status_proxy(marital_status_recode),
    across(all_of(replicate_cols_raw), as.numeric)
  ) %>%
  mutate(
    avg_apr_reported = ifelse(is.nan(avg_apr_reported), NA_real_, avg_apr_reported),
    positive_student_debt = student_debt > 0,
    zero_payment_cutoff = zero_payment_income_cutoff(household_size),
    ibr_payment_rate = ibr_payment_rate_from_age(age),
    ibr_repayment_years = ibr_repayment_years_from_age(age),
    annual_ibr_payment = annual_ibr_payment_from_income(income, household_size, age),
    monthly_ibr_payment = annual_ibr_payment / 12
  ) %>%
  filter(
    !is.na(filing_status),
    !is.na(race),
    !is.na(ethnicity),
    !is.na(household_group),
    !is.na(income),
    !is.na(age),
    !is.na(household_size),
    income > zero_payment_cutoff,
    (
      household_group == "Unmarried household" & income < single_hoh_qss_phaseout_end
    ) |
      (
        household_group == "Married household" & income < mfj_phaseout_end
      )
  )

cat("Rows after SCF preparation:", format(nrow(scf_df), big.mark = ","), "\n")
cat("Unique families:", format(n_distinct(scf_df$family_id), big.mark = ","), "\n")

replicate_cols <- if (length(replicate_cols_raw) > 0) replicate_cols_raw else names(rw_raw)[grepl("^wt1b[0-9]+$", names(rw_raw))]

if (!is.null(rw_raw)) {
  rw_df <- rw_raw %>%
    transmute(
      implicate_id = as.numeric(y1),
      across(all_of(replicate_cols), as.numeric)
    )
} else {
  rw_df <- NULL
}

replicate_count <- length(replicate_cols)
cat("Replicate-weight count:", replicate_count, "\n")

if (!is.null(rw_df)) {
  scf_df <- scf_df %>%
    left_join(rw_df, by = "implicate_id")
}

harmonized_output_file <- file.path(paths$scf_harmonized_dir, paste0("scf_household_year_", analysis_year, ".rds"))
saveRDS(scf_df, harmonized_output_file)
cat("Wrote harmonized SCF household-year file:", harmonized_output_file, "\n")

estimate_metric_names <- c(
  "weighted_population",
  "mean_student_debt",
  "mean_interest_paid",
  "mean_max_potential_deduction",
  "mean_allowable_deduction",
  "mean_estimated_tax_savings",
  "pct_borrowers_with_any_allowable_deduction",
  "pct_borrowers_with_full_deduction"
)

summarise_implicate <- function(df, annual_interest_rate) {
  df %>%
    mutate(
      accrued_interest = if_else(positive_student_debt, student_debt * annual_interest_rate, 0),
      interest_paid = if_else(positive_student_debt, pmin(accrued_interest, annual_ibr_payment), 0),
      max_potential_deduction = pmin(interest_paid, student_loan_interest_deduction_max),
      allowable_deduction = allowable_deduction_from_magi(max_potential_deduction, income, filing_status),
      marginal_tax_rate = marginal_tax_rate_from_income(income, filing_status),
      estimated_tax_savings = allowable_deduction * marginal_tax_rate,
      any_allowable_deduction = allowable_deduction > 0,
      full_deduction = allowable_deduction >= max_potential_deduction & max_potential_deduction > 0
    ) %>%
    group_by(implicate_number, ethnicity, race, household_group) %>%
    summarise(
      n_obs = n(),
      n_positive_student_debt = sum(positive_student_debt, na.rm = TRUE),
      weighted_population = sum(weight[weight > 0], na.rm = TRUE),
      mean_student_debt = weighted_mean_safe(if_else(positive_student_debt, student_debt, NA_real_), weight),
      mean_interest_paid = weighted_mean_safe(if_else(positive_student_debt, interest_paid, NA_real_), weight),
      mean_max_potential_deduction = weighted_mean_safe(if_else(positive_student_debt, max_potential_deduction, NA_real_), weight),
      mean_allowable_deduction = weighted_mean_safe(if_else(positive_student_debt, allowable_deduction, NA_real_), weight),
      mean_estimated_tax_savings = weighted_mean_safe(if_else(positive_student_debt, estimated_tax_savings, NA_real_), weight),
      pct_borrowers_with_any_allowable_deduction = weighted_percent_safe(if_else(positive_student_debt, any_allowable_deduction, NA), weight),
      pct_borrowers_with_full_deduction = weighted_percent_safe(if_else(positive_student_debt, full_deduction, NA), weight),
      .groups = "drop"
    )
}

summarise_one_weight <- function(df, weight_var) {
  w <- df[[weight_var]]
  tibble(
    weighted_population = sum(w[w > 0], na.rm = TRUE),
    mean_student_debt = weighted_mean_safe(if_else(df$positive_student_debt, df$student_debt, NA_real_), w),
    mean_interest_paid = weighted_mean_safe(if_else(df$positive_student_debt, df$interest_paid, NA_real_), w),
    mean_max_potential_deduction = weighted_mean_safe(if_else(df$positive_student_debt, df$max_potential_deduction, NA_real_), w),
    mean_allowable_deduction = weighted_mean_safe(if_else(df$positive_student_debt, df$allowable_deduction, NA_real_), w),
    mean_estimated_tax_savings = weighted_mean_safe(if_else(df$positive_student_debt, df$estimated_tax_savings, NA_real_), w),
    pct_borrowers_with_any_allowable_deduction = weighted_percent_safe(if_else(df$positive_student_debt, df$any_allowable_deduction, NA), w),
    pct_borrowers_with_full_deduction = weighted_percent_safe(if_else(df$positive_student_debt, df$full_deduction, NA), w)
  )
}

build_married_cap_comparison <- function(df, group_var) {
  scenario_results <- vector("list", nrow(interest_rate_scenarios))
  comparison_metric_names <- c(
    "weighted_population",
    "mean_student_debt",
    "mean_allowable_deduction_baseline",
    "mean_allowable_deduction_proposed",
    "mean_deduction_gain",
    "pct_borrowers_at_2500_limit_baseline",
    "mean_tax_savings_baseline",
    "mean_tax_savings_proposed",
    "mean_tax_savings_gain"
  )

  summarise_implicate_comparison <- function(data) {
    data %>%
      group_by(implicate_number, rate_scenario_id, rate_scenario, annual_interest_rate, household_group, comparison_group) %>%
      summarise(
        n_obs = n(),
        n_positive_student_debt = sum(positive_student_debt, na.rm = TRUE),
        weighted_population = sum(weight[weight > 0], na.rm = TRUE),
        mean_student_debt = weighted_mean_safe(student_debt, weight),
        median_student_debt = weighted_median_safe(student_debt, weight),
        mean_allowable_deduction_baseline = weighted_mean_safe(allowable_deduction_baseline, weight),
        mean_allowable_deduction_proposed = weighted_mean_safe(allowable_deduction_proposed, weight),
        mean_deduction_gain = weighted_mean_safe(deduction_gain, weight),
        median_deduction_gain = weighted_median_safe(deduction_gain, weight),
        pct_borrowers_at_2500_limit_baseline = weighted_percent_safe(if_else(positive_student_debt, at_baseline_2500_limit, NA), weight),
        pct_households_with_positive_deduction_gain = weighted_percent_safe(positive_gain, weight),
        mean_tax_savings_baseline = weighted_mean_safe(tax_savings_baseline, weight),
        mean_tax_savings_proposed = weighted_mean_safe(tax_savings_proposed, weight),
        mean_tax_savings_gain = weighted_mean_safe(tax_savings_gain, weight),
        median_tax_savings_gain = weighted_median_safe(tax_savings_gain, weight),
        pct_households_with_positive_tax_savings_gain = weighted_percent_safe(positive_gain, weight),
        .groups = "drop"
      )
  }

  summarise_one_weight_comparison <- function(data, weight_var) {
    w <- data[[weight_var]]
    tibble(
      weighted_population = sum(w[w > 0], na.rm = TRUE),
      mean_student_debt = weighted_mean_safe(data$student_debt, w),
      mean_allowable_deduction_baseline = weighted_mean_safe(data$allowable_deduction_baseline, w),
      mean_allowable_deduction_proposed = weighted_mean_safe(data$allowable_deduction_proposed, w),
      mean_deduction_gain = weighted_mean_safe(data$deduction_gain, w),
      pct_borrowers_at_2500_limit_baseline = weighted_percent_safe(if_else(data$positive_student_debt, data$at_baseline_2500_limit, NA), w),
      mean_tax_savings_baseline = weighted_mean_safe(data$tax_savings_baseline, w),
      mean_tax_savings_proposed = weighted_mean_safe(data$tax_savings_proposed, w),
      mean_tax_savings_gain = weighted_mean_safe(data$tax_savings_gain, w)
    )
  }

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

    point_estimates <- summarise_implicate_comparison(scenario_df)

    sampling_variances <- scenario_df %>%
      filter(implicate_number == 1) %>%
      group_by(rate_scenario_id, rate_scenario, annual_interest_rate, household_group, comparison_group) %>%
      group_modify(function(.x, .y) {
        replicate_results <- vector("list", length(replicate_cols))
        for (j in seq_along(replicate_cols)) {
          rc <- replicate_cols[[j]]
          replicate_results[[j]] <- summarise_one_weight_comparison(.x, rc) %>%
            mutate(replicate = rc)
        }

        bind_rows(replicate_results) %>%
          pivot_longer(
            cols = all_of(comparison_metric_names),
            names_to = "metric",
            values_to = "replicate_estimate"
          ) %>%
          group_by(metric) %>%
          summarise(sampling_variance = stats::var(replicate_estimate, na.rm = TRUE), .groups = "drop") %>%
          mutate(sampling_variance = ifelse(is.na(sampling_variance), 0, sampling_variance))
      }) %>%
      ungroup()

    comparison_long <- point_estimates %>%
      pivot_longer(
        cols = all_of(comparison_metric_names),
        names_to = "metric",
        values_to = "estimate"
      )

    point_summary <- comparison_long %>%
      group_by(rate_scenario_id, rate_scenario, annual_interest_rate, household_group, comparison_group, metric) %>%
      summarise(
        estimate = mean(estimate, na.rm = TRUE),
        imputation_variance = stats::var(estimate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(imputation_variance = ifelse(is.na(imputation_variance), 0, imputation_variance))

    comparison_estimates <- point_summary %>%
      left_join(sampling_variances, by = c("rate_scenario_id", "rate_scenario", "annual_interest_rate", "household_group", "comparison_group", "metric")) %>%
      mutate(
        sampling_variance = ifelse(is.na(sampling_variance), 0, sampling_variance),
        total_variance = sampling_variance + (6 / 5) * imputation_variance,
        se = sqrt(pmax(total_variance, 0))
      ) %>%
      select(rate_scenario_id, rate_scenario, annual_interest_rate, household_group, comparison_group, metric, estimate, se) %>%
      pivot_wider(
        names_from = metric,
        values_from = c(estimate, se),
        names_glue = "{.value}__{metric}"
      )

    sample_counts <- point_estimates %>%
      group_by(rate_scenario_id, rate_scenario, annual_interest_rate, household_group, comparison_group) %>%
      summarise(
        n_obs = mean(n_obs, na.rm = TRUE),
        n_positive_student_debt = mean(n_positive_student_debt, na.rm = TRUE),
        median_student_debt = mean(median_student_debt, na.rm = TRUE),
        median_deduction_gain = mean(median_deduction_gain, na.rm = TRUE),
        pct_households_with_positive_deduction_gain = mean(pct_households_with_positive_deduction_gain, na.rm = TRUE),
        median_tax_savings_gain = mean(median_tax_savings_gain, na.rm = TRUE),
        pct_households_with_positive_tax_savings_gain = mean(pct_households_with_positive_tax_savings_gain, na.rm = TRUE),
        .groups = "drop"
      )

    scenario_results[[i]] <- sample_counts %>%
      left_join(comparison_estimates, by = c("rate_scenario_id", "rate_scenario", "annual_interest_rate", "household_group", "comparison_group")) %>%
      transmute(
        rate_scenario_id,
        rate_scenario,
        annual_interest_rate,
        household_group,
        comparison_group,
        n_obs,
        n_positive_student_debt,
        weighted_population = estimate__weighted_population,
        `SE: weighted population` = se__weighted_population,
        mean_student_debt = estimate__mean_student_debt,
        `SE: mean student debt` = se__mean_student_debt,
        median_student_debt,
        mean_allowable_deduction_baseline = estimate__mean_allowable_deduction_baseline,
        `SE: mean allowable deduction baseline` = se__mean_allowable_deduction_baseline,
        mean_allowable_deduction_proposed = estimate__mean_allowable_deduction_proposed,
        `SE: mean allowable deduction proposed` = se__mean_allowable_deduction_proposed,
        mean_deduction_gain = estimate__mean_deduction_gain,
        `SE: mean deduction gain` = se__mean_deduction_gain,
        median_deduction_gain,
        pct_borrowers_at_2500_limit_baseline = estimate__pct_borrowers_at_2500_limit_baseline,
        `SE: % borrowers at $2500 limit baseline` = se__pct_borrowers_at_2500_limit_baseline,
        pct_households_with_positive_deduction_gain,
        mean_tax_savings_baseline = estimate__mean_tax_savings_baseline,
        `SE: mean tax savings baseline` = se__mean_tax_savings_baseline,
        mean_tax_savings_proposed = estimate__mean_tax_savings_proposed,
        `SE: mean tax savings proposed` = se__mean_tax_savings_proposed,
        mean_tax_savings_gain = estimate__mean_tax_savings_gain,
        `SE: mean tax savings gain` = se__mean_tax_savings_gain,
        median_tax_savings_gain,
        pct_households_with_positive_tax_savings_gain
      ) %>%
      arrange(rate_scenario_id, household_group, comparison_group)
  }

  bind_rows(scenario_results)
}

summarise_replicate_variance <- function(df, annual_interest_rate, replicate_cols) {
  scenario_df <- df %>%
    filter(implicate_number == 1) %>%
    mutate(
      accrued_interest = if_else(positive_student_debt, student_debt * annual_interest_rate, 0),
      interest_paid = if_else(positive_student_debt, pmin(accrued_interest, annual_ibr_payment), 0),
      max_potential_deduction = pmin(interest_paid, student_loan_interest_deduction_max),
      allowable_deduction = allowable_deduction_from_magi(max_potential_deduction, income, filing_status),
      marginal_tax_rate = marginal_tax_rate_from_income(income, filing_status),
      estimated_tax_savings = allowable_deduction * marginal_tax_rate,
      any_allowable_deduction = allowable_deduction > 0,
      full_deduction = allowable_deduction >= max_potential_deduction & max_potential_deduction > 0
    )

  replicate_results <- vector("list", length(replicate_cols))

  for (i in seq_along(replicate_cols)) {
    rc <- replicate_cols[[i]]
    replicate_results[[i]] <- scenario_df %>%
      group_by(ethnicity, race, household_group) %>%
      group_modify(~ summarise_one_weight(.x, rc)) %>%
      ungroup() %>%
      mutate(replicate = rc)
  }

  bind_rows(replicate_results) %>%
    pivot_longer(
      cols = all_of(estimate_metric_names),
      names_to = "metric",
      values_to = "replicate_estimate"
    ) %>%
    group_by(ethnicity, race, household_group, metric) %>%
    summarise(
      sampling_variance = stats::var(replicate_estimate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(sampling_variance = ifelse(is.na(sampling_variance), 0, sampling_variance))
}

combine_scf_estimates <- function(point_df, sampling_var_df, group_cols) {
  point_long <- point_df %>%
    pivot_longer(
      cols = all_of(estimate_metric_names),
      names_to = "metric",
      values_to = "estimate"
    )

  point_summary <- point_long %>%
    group_by(across(all_of(group_cols)), metric) %>%
    summarise(
      estimate = mean(estimate, na.rm = TRUE),
      imputation_variance = stats::var(estimate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(imputation_variance = ifelse(is.na(imputation_variance), 0, imputation_variance))

  point_summary %>%
    left_join(sampling_var_df, by = c("ethnicity", "race", "household_group", "metric")) %>%
    mutate(
      sampling_variance = ifelse(is.na(sampling_variance), 0, sampling_variance),
      total_variance = sampling_variance + (6 / 5) * imputation_variance,
      se = sqrt(pmax(total_variance, 0))
    ) %>%
    select(all_of(group_cols), metric, estimate, se) %>%
    pivot_wider(
      names_from = metric,
      values_from = c(estimate, se),
      names_glue = "{.value}__{metric}"
    )
}

rule("SCF deduction summaries")
summary_start <- Sys.time()
progress_msg("Computing SCF interest deduction summaries...")

scenario_results <- vector("list", nrow(interest_rate_scenarios))

for (i in seq_len(nrow(interest_rate_scenarios))) {
  point_estimates <- summarise_implicate(
    scf_df,
    annual_interest_rate = interest_rate_scenarios$annual_interest_rate[[i]]
  ) %>%
    mutate(
      rate_scenario_id = interest_rate_scenarios$rate_scenario_id[[i]],
      rate_scenario = interest_rate_scenarios$rate_scenario[[i]],
      annual_interest_rate = interest_rate_scenarios$annual_interest_rate[[i]]
    )

  sampling_variances <- summarise_replicate_variance(
    scf_df,
    annual_interest_rate = interest_rate_scenarios$annual_interest_rate[[i]],
    replicate_cols = replicate_cols
  )

  combined_estimates <- combine_scf_estimates(
    point_estimates,
    sampling_variances,
    group_cols = c("rate_scenario_id", "rate_scenario", "annual_interest_rate", "ethnicity", "race", "household_group")
  )

  sample_counts <- point_estimates %>%
    group_by(rate_scenario_id, rate_scenario, annual_interest_rate, ethnicity, race, household_group) %>%
    summarise(
      n_obs = mean(n_obs, na.rm = TRUE),
      n_positive_student_debt = mean(n_positive_student_debt, na.rm = TRUE),
      .groups = "drop"
    )

  scenario_results[[i]] <- sample_counts %>%
    left_join(combined_estimates, by = c("rate_scenario_id", "rate_scenario", "annual_interest_rate", "ethnicity", "race", "household_group"))
}

summary_combined <- bind_rows(scenario_results)

summary_final <- summary_combined %>%
  transmute(
    source = "SCF",
    year = analysis_year,
    rate_scenario_id,
    rate_scenario,
    annual_interest_rate,
    ethnicity,
    race,
    household_group,
    n_obs,
    n_positive_student_debt,
    weighted_population = estimate__weighted_population,
    `SE: weighted population` = se__weighted_population,
    mean_student_debt = estimate__mean_student_debt,
    `SE: mean student debt` = se__mean_student_debt,
    `Estimated annual interest paid` = estimate__mean_interest_paid,
    `SE: estimated annual interest paid` = se__mean_interest_paid,
    `Max potential deduction` = estimate__mean_max_potential_deduction,
    `SE: max potential deduction` = se__mean_max_potential_deduction,
    `Allowable deduction after MAGI phaseout` = estimate__mean_allowable_deduction,
    `SE: allowable deduction after MAGI phaseout` = se__mean_allowable_deduction,
    `Estimated tax savings` = estimate__mean_estimated_tax_savings,
    `SE: estimated tax savings` = se__mean_estimated_tax_savings,
    `% of borrowers with any allowable deduction` = estimate__pct_borrowers_with_any_allowable_deduction,
    `SE: % of borrowers with any allowable deduction` = se__pct_borrowers_with_any_allowable_deduction,
    `% of borrowers with full deduction` = estimate__pct_borrowers_with_full_deduction,
    `SE: % of borrowers with full deduction` = se__pct_borrowers_with_full_deduction
  ) %>%
  arrange(rate_scenario_id, household_group, ethnicity, race)

summary_rate_275 <- summary_final %>% filter(rate_scenario_id == "R275")
summary_rate_680 <- summary_final %>% filter(rate_scenario_id == "R680")

progress_msg("SCF interest deduction summaries complete", summary_start)

rule("SCF married-cap comparison summaries")
comparison_start <- Sys.time()
progress_msg("Computing SCF married-cap comparison summaries...")

scf_comparison_base <- scf_df

married_cap_comparison_by_race <- build_married_cap_comparison(scf_comparison_base, "race") %>%
  mutate(source = "SCF", year = analysis_year) %>%
  rename(race = comparison_group)

married_cap_comparison_black_nonblack <- build_married_cap_comparison(scf_comparison_base, "black_nonblack") %>%
  mutate(source = "SCF", year = analysis_year) %>%
  rename(black_nonblack = comparison_group)

progress_msg("SCF married-cap comparison summaries complete", comparison_start)

rule("Writing SCF CSV outputs")
write_start <- Sys.time()
progress_msg("Writing SCF output CSVs...")
write_csv(summary_rate_275, output_rate_275, na = "")
write_csv(summary_rate_680, output_rate_680, na = "")
write_csv(summary_final, output_combined, na = "")
write_csv(married_cap_comparison_by_race, output_married_cap_full_race, na = "")
write_csv(married_cap_comparison_black_nonblack, output_married_cap_black_nonblack, na = "")
if (analysis_year == 2022) {
  write_csv(married_cap_comparison_by_race, current_output_married_cap_full_race, na = "")
  write_csv(married_cap_comparison_black_nonblack, current_output_married_cap_black_nonblack, na = "")
  write_csv(summary_rate_275, outmoded_output_rate_275, na = "")
  write_csv(summary_rate_680, outmoded_output_rate_680, na = "")
  write_csv(summary_final, outmoded_output_combined, na = "")
}
progress_msg("SCF output CSVs written", write_start)

cat("Wrote:", output_rate_275, "\n")
cat("Wrote:", output_rate_680, "\n")
cat("Wrote:", output_combined, "\n")
cat("Wrote:", output_married_cap_full_race, "\n")
cat("Wrote:", output_married_cap_black_nonblack, "\n")

rule("Notes")
cat(
  "SCF analysis is parallel to the SIPP tax-deduction analysis but uses SCF-specific public variables.\n",
  "MAGI is proxied with X5729 total income; filing status is proxied as married filers = MFJ and other filers = Single/Head of household.\n",
  "The public 2022 SCF does not expose an exact IRS filing-status field, so married filing separately cannot be directly identified.\n",
  "Education-loan balances are built from the SCF education-loan amount-owed variables plus the mop-up education-loan balance.\n",
  "Standard errors combine bootstrap sampling variance from the 999 replicate weights on implicate 1 with imputation variance across the 5 implicates using the SCF 6/5 rule.\n",
  sep = ""
)
