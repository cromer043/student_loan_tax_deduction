#!/usr/bin/env Rscript

raw_script_path <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1])
script_path <- normalizePath(gsub("~\\+~", " ", raw_script_path), winslash = "/", mustWork = FALSE)
code_dir <- dirname(script_path)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(code_dir, "student_loan_interest_workflow_utils.R"))
paths <- get_workflow_paths(project_dir)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
  library(scales)
})

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

interest_rate_scenarios <- tribble(
  ~rate_scenario_id, ~rate_scenario, ~annual_interest_rate,
  "R275", "Estimated interest at 2.75% annual rate", 0.0275,
  "R680", "Estimated interest at 6.8% annual rate", 0.0680
)

allowable_deduction_from_magi <- function(max_potential_deduction, magi, filing_status) {
  dplyr::case_when(
    is.na(max_potential_deduction) | is.na(magi) | is.na(filing_status) ~ NA_real_,
    filing_status == "Married filing separately" ~ 0,
    filing_status %in% c("Single", "Head of household", "Single/Head of household") & magi <= single_hoh_qss_full_magi_max ~ max_potential_deduction,
    filing_status %in% c("Single", "Head of household", "Single/Head of household") & magi >= single_hoh_qss_phaseout_end ~ 0,
    filing_status == "Married filing jointly" & magi <= mfj_full_magi_max ~ max_potential_deduction,
    filing_status == "Married filing jointly" & magi >= mfj_phaseout_end ~ 0,
    filing_status %in% c("Single", "Head of household", "Single/Head of household") ~ max_potential_deduction * (1 - (magi - single_hoh_qss_full_magi_max) / (single_hoh_qss_phaseout_end - single_hoh_qss_full_magi_max)),
    filing_status == "Married filing jointly" ~ max_potential_deduction * (1 - (magi - mfj_full_magi_max) / (mfj_phaseout_end - mfj_full_magi_max)),
    TRUE ~ NA_real_
  ) %>%
    pmax(0)
}

marginal_tax_rate_from_income <- function(taxable_income_proxy, filing_status) {
  dplyr::case_when(
    is.na(taxable_income_proxy) | is.na(filing_status) ~ NA_real_,
    filing_status == "Married filing separately" ~ NA_real_,
    filing_status %in% c("Single", "Single/Head of household") & taxable_income_proxy <= single_bracket_10_top ~ 0.10,
    filing_status %in% c("Single", "Single/Head of household") & taxable_income_proxy <= single_bracket_12_top ~ 0.12,
    filing_status %in% c("Single", "Single/Head of household") & taxable_income_proxy <= single_bracket_22_top ~ 0.22,
    filing_status %in% c("Single", "Single/Head of household") & taxable_income_proxy <= single_bracket_24_top ~ 0.24,
    filing_status %in% c("Single", "Single/Head of household") & taxable_income_proxy <= single_bracket_32_top ~ 0.32,
    filing_status %in% c("Single", "Single/Head of household") & taxable_income_proxy <= single_bracket_35_top ~ 0.35,
    filing_status %in% c("Single", "Single/Head of household") ~ 0.37,
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

deduction_cap_from_household_group <- function(household_group, married_cap, nonmarried_cap = student_loan_interest_deduction_max) {
  dplyr::case_when(
    household_group == "Married household" ~ married_cap,
    household_group == "Unmarried household" ~ nonmarried_cap,
    TRUE ~ NA_real_
  )
}

example_single_income <- single_hoh_qss_full_magi_max
example_married_income <- mfj_full_magi_max

memo_examples <- interest_rate_scenarios %>%
  rowwise() %>%
  mutate(
    minimum_student_debt_for_2500_interest = student_loan_interest_deduction_max / annual_interest_rate,
    single_interest_paid = minimum_student_debt_for_2500_interest * annual_interest_rate,
    unmarried_cap = deduction_cap_from_household_group("Unmarried household", married_cap = student_loan_interest_deduction_max),
    married_cap_current = deduction_cap_from_household_group("Married household", married_cap = student_loan_interest_deduction_max),
    married_cap_proposed = deduction_cap_from_household_group("Married household", married_cap = student_loan_interest_deduction_max_married_proposed),
    single_allowable_deduction = allowable_deduction_from_magi(pmin(single_interest_paid, unmarried_cap), example_single_income, "Single"),
    current_married_allowable_deduction = allowable_deduction_from_magi(pmin(single_interest_paid * 2, married_cap_current), example_married_income, "Married filing jointly"),
    proposed_married_allowable_deduction = allowable_deduction_from_magi(pmin(single_interest_paid * 2, married_cap_proposed), example_married_income, "Married filing jointly"),
    total_unmarried_allowable_deduction = single_allowable_deduction * 2,
    deduction_lost_when_two_unmarried_borrowers_marry = total_unmarried_allowable_deduction - current_married_allowable_deduction,
    project_single_marginal_tax_rate = marginal_tax_rate_from_income(example_single_income, "Single"),
    project_married_marginal_tax_rate = marginal_tax_rate_from_income(example_married_income, "Married filing jointly"),
    total_unmarried_tax_savings = total_unmarried_allowable_deduction * project_single_marginal_tax_rate,
    current_married_tax_savings = current_married_allowable_deduction * project_married_marginal_tax_rate,
    proposed_married_tax_savings = proposed_married_allowable_deduction * project_married_marginal_tax_rate,
    estimated_federal_tax_savings_loss_current_law = total_unmarried_tax_savings - current_married_tax_savings,
    tax_savings_restored_under_proposed_5000_married_cap = proposed_married_tax_savings - current_married_tax_savings
  ) %>%
  ungroup() %>%
  transmute(
    rate_scenario,
    annual_interest_rate,
    minimum_student_debt_for_2500_interest,
    single_borrower_interest_paid_example = single_interest_paid,
    project_single_marginal_tax_rate,
    project_married_marginal_tax_rate,
    total_unmarried_allowable_deduction,
    current_married_allowable_deduction,
    proposed_married_allowable_deduction,
    deduction_lost_when_two_unmarried_borrowers_marry,
    total_unmarried_tax_savings,
    current_married_tax_savings,
    proposed_married_tax_savings,
    estimated_federal_tax_savings_loss_current_law,
    tax_savings_restored_under_proposed_5000_married_cap
  )

output_file <- file.path(paths$output_csv_dir, "memo_marriage_penalty_examples.csv")
write_csv(memo_examples, output_file, na = "")

cat("Wrote:", output_file, "\n\n")
cat("Memo-ready summary\n")
cat("------------------\n")

for (i in seq_len(nrow(memo_examples))) {
  row <- memo_examples[i, ]
  cat(
    paste0(
      row$rate_scenario[[1]], ": borrowers need about ",
      dollar(row$minimum_student_debt_for_2500_interest[[1]], accuracy = 0.01),
      " in student debt to generate $2,500 in annual interest. ",
      "Under the project's current-law cap, two unmarried borrowers who each pay $2,500 in annual interest can deduct ",
      dollar(row$total_unmarried_allowable_deduction[[1]], accuracy = 0.01),
      " combined, but only ",
      dollar(row$current_married_allowable_deduction[[1]], accuracy = 0.01),
      " after marrying and filing jointly, a deduction loss of ",
      dollar(row$deduction_lost_when_two_unmarried_borrowers_marry[[1]], accuracy = 0.01),
      ". Using the project's own marginal-rate assignment, that implies a federal tax-savings loss of ",
      dollar(row$estimated_federal_tax_savings_loss_current_law[[1]], accuracy = 0.01),
      ". With a $5,000 married cap, the married couple's tax savings would be ",
      dollar(row$proposed_married_tax_savings[[1]], accuracy = 0.01),
      ", restoring ",
      dollar(row$tax_savings_restored_under_proposed_5000_married_cap[[1]], accuracy = 0.01),
      " relative to current law."
    ),
    "\n"
  )
}
