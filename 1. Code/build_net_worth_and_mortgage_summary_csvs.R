#!/usr/bin/env Rscript

raw_script_path <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1])
script_path <- normalizePath(gsub("~\\+~", " ", raw_script_path), winslash = "/", mustWork = FALSE)
code_dir <- dirname(script_path)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(code_dir, "student_loan_interest_workflow_utils.R"))
paths <- get_workflow_paths(project_dir)

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tibble)
})

interest_rate_scenarios <- tibble::tribble(
  ~rate_scenario_id, ~rate_scenario, ~annual_interest_rate,
  "R275", "Estimated interest at 2.75% annual rate", 0.0275,
  "R680", "Estimated interest at 6.8% annual rate", 0.0680
)

student_loan_interest_deduction_max <- 2500
single_hoh_qss_full_magi_max <- 85000
single_hoh_qss_phaseout_end <- 100000
mfj_full_magi_max <- 170000
mfj_phaseout_end <- 200000

weighted_mean_safe <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

weighted_median_safe <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) return(NA_real_)
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cum_w <- cumsum(w) / sum(w)
  x[which(cum_w >= 0.5)[1]]
}

weighted_percent_safe <- function(indicator, w) {
  keep <- !is.na(indicator) & !is.na(w) & w > 0
  if (!any(keep)) return(NA_real_)
  100 * sum(w[keep] * as.numeric(indicator[keep])) / sum(w[keep])
}

allowable_deduction_from_magi <- function(max_potential_deduction, magi, filing_status) {
  dplyr::case_when(
    filing_status == "Married filing separately" ~ 0,
    filing_status == "Married filing jointly" & magi <= mfj_full_magi_max ~ max_potential_deduction,
    filing_status == "Married filing jointly" & magi >= mfj_phaseout_end ~ 0,
    filing_status == "Married filing jointly" ~ max_potential_deduction * (1 - (magi - mfj_full_magi_max) / (mfj_phaseout_end - mfj_full_magi_max)),
    filing_status %in% c("Single", "Head of household", "Qualifying surviving spouse", "Single/Head of household") & magi <= single_hoh_qss_full_magi_max ~ max_potential_deduction,
    filing_status %in% c("Single", "Head of household", "Qualifying surviving spouse", "Single/Head of household") & magi >= single_hoh_qss_phaseout_end ~ 0,
    filing_status %in% c("Single", "Head of household", "Qualifying surviving spouse", "Single/Head of household") ~ max_potential_deduction * (1 - (magi - single_hoh_qss_full_magi_max) / (single_hoh_qss_phaseout_end - single_hoh_qss_full_magi_max)),
    TRUE ~ 0
  )
}

population_groups_filtered <- function(df) {
  if (!"positive_student_debt" %in% names(df)) {
    df <- df %>% mutate(positive_student_debt = student_debt > 0)
  }
  list(
    `Filtered households with positive student debt` = df %>% filter(positive_student_debt),
    `Filtered married households with positive student debt` = df %>% filter(positive_student_debt, household_group == "Married household"),
    `Filtered unmarried households with positive student debt` = df %>% filter(positive_student_debt, household_group == "Unmarried household")
  )
}

population_groups_mortgage <- function(all_df, filtered_df) {
  if (!"positive_student_debt" %in% names(filtered_df)) {
    filtered_df <- filtered_df %>% mutate(positive_student_debt = student_debt > 0)
  }
  list(
    `All households` = all_df,
    `Filtered households with positive student debt` = filtered_df %>% filter(positive_student_debt),
    `Filtered married households with positive student debt` = filtered_df %>% filter(positive_student_debt, household_group == "Married household"),
    `Filtered unmarried households with positive student debt` = filtered_df %>% filter(positive_student_debt, household_group == "Unmarried household")
  )
}

summarise_df <- function(df, use_implicates = FALSE, metric_fn) {
  if (nrow(df) == 0) return(metric_fn(df))
  if (!use_implicates) return(metric_fn(df))

  df %>%
    group_by(implicate_number) %>%
    group_modify(~ metric_fn(.x)) %>%
    ungroup() %>%
    select(-any_of("implicate_number")) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
}

make_networth_binding_summary <- function(filtered_df, source_id, year_value, use_implicates = FALSE) {
  pops <- population_groups_filtered(filtered_df)

  bind_rows(lapply(names(pops), function(pop_name) {
    pop_df <- pops[[pop_name]]

    networth_stats <- summarise_df(
      pop_df,
      use_implicates = use_implicates,
      metric_fn = function(d) {
        tibble(
          weighted_mean_total_net_worth = weighted_mean_safe(d$total_net_worth, d$weight),
          weighted_median_total_net_worth = weighted_median_safe(d$total_net_worth, d$weight),
          unweighted_n = nrow(d),
          weighted_population = sum(d$weight, na.rm = TRUE)
        )
      }
    )

    binding_stats <- lapply(seq_len(nrow(interest_rate_scenarios)), function(i) {
      rate_row <- interest_rate_scenarios[i, ]
      scenario_df <- pop_df %>%
        mutate(
          accrued_interest = if_else(positive_student_debt, student_debt * rate_row$annual_interest_rate[[1]], 0),
          interest_paid = if_else(positive_student_debt, pmin(accrued_interest, annual_ibr_payment), 0),
          allowable_deduction_baseline = allowable_deduction_from_magi(
            pmin(interest_paid, student_loan_interest_deduction_max),
            income,
            filing_status
          ),
          at_binding_constraint = positive_student_debt & abs(allowable_deduction_baseline - student_loan_interest_deduction_max) < 1e-8
        )

      stat <- summarise_df(
        scenario_df,
        use_implicates = use_implicates,
        metric_fn = function(d) {
          tibble(pct_at_binding_constraint = weighted_percent_safe(d$at_binding_constraint, d$weight))
        }
      )
      stat$rate_scenario_id <- rate_row$rate_scenario_id[[1]]
      stat
    })

    binding_wide <- bind_rows(binding_stats) %>%
      select(rate_scenario_id, pct_at_binding_constraint) %>%
      tidyr::pivot_wider(
        names_from = rate_scenario_id,
        values_from = pct_at_binding_constraint,
        names_prefix = "pct_at_binding_constraint_"
      )

    bind_cols(
      tibble(source = source_id, year = year_value, population = pop_name),
      networth_stats,
      binding_wide
    )
  }))
}

make_mortgage_summary <- function(all_df, filtered_df, source_id, year_value, use_implicates = FALSE) {
  pops <- population_groups_mortgage(all_df, filtered_df)

  bind_rows(lapply(names(pops), function(pop_name) {
    pop_df <- pops[[pop_name]]
    stats <- summarise_df(
      pop_df,
      use_implicates = use_implicates,
      metric_fn = function(d) {
        tibble(
          pct_mortgage_ge_750k = weighted_percent_safe(d$mortgage_debt >= 750000, d$weight),
          unweighted_n = nrow(d),
          weighted_population = sum(d$weight, na.rm = TRUE)
        )
      }
    )
    bind_cols(tibble(source = source_id, year = year_value, population = pop_name), stats)
  }))
}

load_sipp_all_households <- function() {
  raw_file <- file.path(paths$sipp_raw_dir, "2024", "pu2024.csv")
  dt <- data.table::fread(
    raw_file,
    select = c("SSUID", "PNUM", "MONTHCODE", "WPFINWGT", "THNETWORTH", "THDEBT_HOME", "THDEBT_RE"),
    showProgress = FALSE
  )
  dt[, SSUID := as.character(SSUID)]
  dt[, PNUM := suppressWarnings(as.numeric(PNUM))]
  dt[, MONTHCODE := suppressWarnings(as.numeric(MONTHCODE))]
  dt[, WPFINWGT := suppressWarnings(as.numeric(WPFINWGT))]
  dt[, THNETWORTH := suppressWarnings(as.numeric(THNETWORTH))]
  dt[, THDEBT_HOME := suppressWarnings(as.numeric(THDEBT_HOME))]
  dt[, THDEBT_RE := suppressWarnings(as.numeric(THDEBT_RE))]
  dt[, is_reference_person := !is.na(PNUM) & PNUM == 101]
  data.table::setorderv(dt, cols = c("SSUID", "is_reference_person", "MONTHCODE"), order = c(1, -1, -1), na.last = TRUE)
  hh <- dt[, .SD[1], by = SSUID]
  tibble(
    household_id = hh$SSUID,
    weight = hh$WPFINWGT,
    total_net_worth = hh$THNETWORTH,
    mortgage_debt = hh$THDEBT_HOME + hh$THDEBT_RE
  )
}

load_scf_all_households <- function() {
  raw_list <- readRDS(file.path(paths$scf_raw_dir, "2022", "scf2022.rds"))
  bind_rows(lapply(seq_along(raw_list), function(i) {
    df <- raw_list[[i]]
    tibble(
      implicate_number = i,
      weight = as.numeric(df$wgt),
      total_net_worth = as.numeric(df$networth),
      mortgage_debt = as.numeric(df$mrthel) + as.numeric(df$resdbt)
    )
  }))
}

make_median_net_worth_threshold_summary <- function(all_df, filtered_df, source_id, year_value, use_implicates = FALSE) {
  if (!"positive_student_debt" %in% names(filtered_df)) {
    filtered_df <- filtered_df %>% mutate(positive_student_debt = student_debt > 0)
  }

  low_binding <- filtered_df %>%
    mutate(
      positive_student_debt = student_debt > 0,
      accrued_interest = if_else(positive_student_debt, student_debt * 0.0275, 0),
      interest_paid = if_else(positive_student_debt, pmin(accrued_interest, annual_ibr_payment), 0),
      allowable_deduction_baseline = allowable_deduction_from_magi(
        pmin(interest_paid, student_loan_interest_deduction_max),
        income,
        filing_status
      ),
      threshold_flag = positive_student_debt & abs(allowable_deduction_baseline - student_loan_interest_deduction_max) < 1e-8
    )

  high_binding <- filtered_df %>%
    mutate(
      positive_student_debt = student_debt > 0,
      accrued_interest = if_else(positive_student_debt, student_debt * 0.0680, 0),
      interest_paid = if_else(positive_student_debt, pmin(accrued_interest, annual_ibr_payment), 0),
      allowable_deduction_baseline = allowable_deduction_from_magi(
        pmin(interest_paid, student_loan_interest_deduction_max),
        income,
        filing_status
      ),
      threshold_flag = positive_student_debt & abs(allowable_deduction_baseline - student_loan_interest_deduction_max) < 1e-8
    )

  mortgage_thresh <- all_df %>%
    mutate(threshold_flag = mortgage_debt >= 750000)

  groups <- list(
    `Low student debt interest binding constraint` = low_binding,
    `High student debt interest binding constraint` = high_binding,
    `$750k home mortgage threshold` = mortgage_thresh
  )

  bind_rows(lapply(names(groups), function(group_name) {
    df <- groups[[group_name]] %>% filter(threshold_flag)
    stats <- summarise_df(
      df,
      use_implicates = use_implicates,
      metric_fn = function(d) {
        tibble(
          weighted_median_total_net_worth = weighted_median_safe(d$total_net_worth, d$weight),
          unweighted_n = nrow(d),
          weighted_population = sum(d$weight, na.rm = TRUE)
        )
      }
    )
    bind_cols(tibble(source = source_id, year = year_value, threshold_group = group_name), stats)
  }))
}

sipp_filtered <- readRDS(file.path(paths$sipp_harmonized_dir, "sipp_household_year_2024.rds"))
sipp_all <- readRDS(file.path(paths$sipp_harmonized_dir, "sipp_household_year_2024_prefilter.rds"))
scf_filtered <- readRDS(file.path(paths$scf_harmonized_dir, "scf_household_year_2022.rds"))
scf_all <- readRDS(file.path(paths$scf_harmonized_dir, "scf_household_year_2022_prefilter.rds"))

if (!all(c("mortgage_debt") %in% names(sipp_all))) {
  sipp_all <- load_sipp_all_households()
}

if (!"household_id" %in% names(sipp_all)) {
  sipp_all <- load_sipp_all_households()
}

sipp_financials <- if (all(c("household_id", "total_net_worth", "mortgage_debt") %in% names(sipp_all))) {
  sipp_all %>% select(household_id, total_net_worth, mortgage_debt)
} else {
  load_sipp_all_households() %>% select(household_id, total_net_worth, mortgage_debt)
}

sipp_filtered <- sipp_filtered %>%
  mutate(household_id = as.character(household_id)) %>%
  left_join(
    sipp_financials %>%
      mutate(household_id = as.character(household_id)) %>%
      rename(total_net_worth_raw = total_net_worth, mortgage_debt_raw = mortgage_debt),
    by = "household_id"
  ) %>%
  mutate(
    total_net_worth = coalesce(total_net_worth_raw, total_net_worth),
    mortgage_debt = coalesce(mortgage_debt_raw, mortgage_debt)
  ) %>%
  select(-total_net_worth_raw, -mortgage_debt_raw)

if (!all(c("mortgage_debt", "implicate_number") %in% names(scf_all))) {
  scf_all <- load_scf_all_households()
}

sipp_networth_binding <- make_networth_binding_summary(sipp_filtered, "SIPP", 2024, use_implicates = FALSE)
scf_networth_binding <- make_networth_binding_summary(scf_filtered, "SCF", 2022, use_implicates = TRUE)
sipp_mortgage <- make_mortgage_summary(sipp_all, sipp_filtered, "SIPP", 2024, use_implicates = FALSE)
scf_mortgage <- make_mortgage_summary(scf_all, scf_filtered, "SCF", 2022, use_implicates = TRUE)
sipp_median_net_worth_thresholds <- make_median_net_worth_threshold_summary(sipp_all, sipp_filtered, "SIPP", 2024, use_implicates = FALSE)
scf_median_net_worth_thresholds <- make_median_net_worth_threshold_summary(scf_all, scf_filtered, "SCF", 2022, use_implicates = TRUE)

sipp_networth_binding_out <- file.path(paths$sipp_csv_dir, "sipp_filtered_net_worth_binding_constraint_summary.csv")
scf_networth_binding_out <- file.path(paths$scf_csv_dir, "scf_filtered_net_worth_binding_constraint_summary.csv")
sipp_mortgage_out <- file.path(paths$sipp_csv_dir, "sipp_mortgage_750k_summary.csv")
scf_mortgage_out <- file.path(paths$scf_csv_dir, "scf_mortgage_750k_summary.csv")
sipp_median_net_worth_thresholds_out <- file.path(paths$sipp_csv_dir, "sipp_median_net_worth_threshold_groups.csv")
scf_median_net_worth_thresholds_out <- file.path(paths$scf_csv_dir, "scf_median_net_worth_threshold_groups.csv")

write_csv(sipp_networth_binding, sipp_networth_binding_out, na = "")
write_csv(scf_networth_binding, scf_networth_binding_out, na = "")
write_csv(sipp_mortgage, sipp_mortgage_out, na = "")
write_csv(scf_mortgage, scf_mortgage_out, na = "")
write_csv(sipp_median_net_worth_thresholds, sipp_median_net_worth_thresholds_out, na = "")
write_csv(scf_median_net_worth_thresholds, scf_median_net_worth_thresholds_out, na = "")

cat("Wrote:", sipp_networth_binding_out, "\n")
cat("Wrote:", scf_networth_binding_out, "\n")
cat("Wrote:", sipp_mortgage_out, "\n")
cat("Wrote:", scf_mortgage_out, "\n")
cat("Wrote:", sipp_median_net_worth_thresholds_out, "\n")
cat("Wrote:", scf_median_net_worth_thresholds_out, "\n")
