#!/usr/bin/env Rscript

raw_script_path <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1])
script_path <- normalizePath(gsub("~\\+~", " ", raw_script_path), winslash = "/", mustWork = FALSE)
code_dir <- dirname(script_path)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(code_dir, "student_loan_interest_workflow_utils.R"))
paths <- get_workflow_paths(project_dir)

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(tibble)
  library(jsonlite)
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

interest_rate_scenarios <- tibble::tribble(
  ~rate_scenario_id, ~rate_scenario,                            ~annual_interest_rate,
  "R275",            "Estimated interest at 2.75% annual rate", 0.0275,
  "R680",            "Estimated interest at 6.8% annual rate",  0.068
)

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

replicate_se_from_estimates <- function(point_estimate, replicate_estimates, variance_scale) {
  ok <- is.finite(replicate_estimates)
  if (!any(ok) || !is.finite(point_estimate)) return(NA_real_)
  sqrt(variance_scale * sum((replicate_estimates[ok] - point_estimate)^2))
}

build_replicate_estimates <- function(df, rep_cols, estimator_fn) {
  vapply(rep_cols, function(rc) estimator_fn(df, rc), numeric(1))
}

marginal_tax_rate_from_income <- function(income, filing_status) {
  dplyr::case_when(
    filing_status %in% c("Single", "Single/Head of household") & income <= single_bracket_10_top ~ 0.10,
    filing_status %in% c("Single", "Single/Head of household") & income <= single_bracket_12_top ~ 0.12,
    filing_status %in% c("Single", "Single/Head of household") & income <= single_bracket_22_top ~ 0.22,
    filing_status %in% c("Single", "Single/Head of household") & income <= single_bracket_24_top ~ 0.24,
    filing_status %in% c("Single", "Single/Head of household") & income <= single_bracket_32_top ~ 0.32,
    filing_status %in% c("Single", "Single/Head of household") & income <= single_bracket_35_top ~ 0.35,
    filing_status %in% c("Single", "Single/Head of household") ~ 0.37,
    filing_status == "Head of household" & income <= hoh_bracket_10_top ~ 0.10,
    filing_status == "Head of household" & income <= hoh_bracket_12_top ~ 0.12,
    filing_status == "Head of household" & income <= hoh_bracket_22_top ~ 0.22,
    filing_status == "Head of household" & income <= hoh_bracket_24_top ~ 0.24,
    filing_status == "Head of household" & income <= hoh_bracket_32_top ~ 0.32,
    filing_status == "Head of household" & income <= hoh_bracket_35_top ~ 0.35,
    filing_status == "Head of household" ~ 0.37,
    filing_status == "Married filing jointly" & income <= mfj_bracket_10_top ~ 0.10,
    filing_status == "Married filing jointly" & income <= mfj_bracket_12_top ~ 0.12,
    filing_status == "Married filing jointly" & income <= mfj_bracket_22_top ~ 0.22,
    filing_status == "Married filing jointly" & income <= mfj_bracket_24_top ~ 0.24,
    filing_status == "Married filing jointly" & income <= mfj_bracket_32_top ~ 0.32,
    filing_status == "Married filing jointly" & income <= mfj_bracket_35_top ~ 0.35,
    filing_status == "Married filing jointly" ~ 0.37,
    TRUE ~ 0.22
  )
}

allowable_deduction_from_magi <- function(max_potential_deduction, magi, filing_status) {
  out <- dplyr::case_when(
    filing_status %in% c("Single", "Head of household", "Single/Head of household") & magi <= single_hoh_qss_full_magi_max ~ max_potential_deduction,
    filing_status == "Married filing jointly" & magi <= mfj_full_magi_max ~ max_potential_deduction,
    filing_status %in% c("Married filing separately", "MFS") ~ 0,
    filing_status %in% c("Single", "Head of household", "Single/Head of household") & magi >= single_hoh_qss_phaseout_end ~ 0,
    filing_status == "Married filing jointly" & magi >= mfj_phaseout_end ~ 0,
    filing_status %in% c("Single", "Head of household", "Single/Head of household") ~ max_potential_deduction * (1 - (magi - single_hoh_qss_full_magi_max) / (single_hoh_qss_phaseout_end - single_hoh_qss_full_magi_max)),
    filing_status == "Married filing jointly" ~ max_potential_deduction * (1 - (magi - mfj_full_magi_max) / (mfj_phaseout_end - mfj_full_magi_max)),
    TRUE ~ 0
  )
  pmax(out, 0)
}

get_census_black_share <- function() {
  query_url <- "https://api.census.gov/data/2024/acs/acs5?get=NAME,B02009_001E,B01003_001E&for=us:1"
  cache_file <- file.path(paths$data_dir, "Census", "acs_2024_us_black_population_share_b02009.json")
  payload <- if (file.exists(cache_file)) {
    jsonlite::fromJSON(cache_file)
  } else {
    jsonlite::fromJSON(query_url)
  }
  values <- payload[2, ]
  black_population <- as.numeric(values[2])
  total_population <- as.numeric(values[3])
  tibble(
    census_vintage = "2024 ACS 5-year",
    geography = values[1],
    black_population = black_population,
    total_population = total_population,
    black_population_share = black_population / total_population,
    nonblack_population_share = 1 - (black_population / total_population),
    census_api_url = query_url
  )
}

compute_group_summary <- function(data, weight_var, black_pop_share, nonblack_pop_share) {
  w <- data[[weight_var]]
  total_relief_all <- sum(w * data$tax_savings_gain, na.rm = TRUE)

  data %>%
    filter(!is.na(black_nonblack)) %>%
    group_by(black_nonblack) %>%
    summarise(
      weighted_population = sum(.data[[weight_var]][.data[[weight_var]] > 0], na.rm = TRUE),
      mean_deduction_gain = weighted_mean_safe(deduction_gain, .data[[weight_var]]),
      mean_tax_savings_gain = weighted_mean_safe(tax_savings_gain, .data[[weight_var]]),
      total_tax_savings_gain = sum(.data[[weight_var]] * tax_savings_gain, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      total_tax_savings_all_groups = total_relief_all,
      tax_relief_share = if (isTRUE(total_relief_all > 0)) total_tax_savings_gain / total_relief_all else NA_real_,
      population_share = case_when(
        black_nonblack == "Black" ~ black_pop_share,
        black_nonblack == "Non-Black" ~ nonblack_pop_share,
        TRUE ~ NA_real_
      ),
      tax_relief_to_population_ratio = tax_relief_share / population_share
    )
}

compute_sipp_binding_outputs <- function(df, census_shares, source_id = "SIPP") {
  rep_cols <- names(df)[grepl("^REPWGT[0-9]+$", names(df))]
  variance_scale <- 1 / (length(rep_cols) * 0.5^2)

  group_rows <- list()
  table_rows <- list()

  for (i in seq_len(nrow(interest_rate_scenarios))) {
    scenario_df <- df %>%
      mutate(
        rate_scenario_id = interest_rate_scenarios$rate_scenario_id[[i]],
        rate_scenario = interest_rate_scenarios$rate_scenario[[i]],
        annual_interest_rate = interest_rate_scenarios$annual_interest_rate[[i]],
        positive_student_debt = student_debt > 0,
        interest_paid = if_else(positive_student_debt, student_debt * annual_interest_rate, 0),
        baseline_cap = student_loan_interest_deduction_max,
        proposed_cap = student_loan_interest_deduction_max_married_proposed,
        allowable_deduction_baseline = allowable_deduction_from_magi(pmin(interest_paid, baseline_cap), income, filing_status),
        allowable_deduction_proposed = allowable_deduction_from_magi(pmin(interest_paid, proposed_cap), income, filing_status),
        marginal_tax_rate = marginal_tax_rate_from_income(income, filing_status),
        deduction_gain = allowable_deduction_proposed - allowable_deduction_baseline,
        tax_savings_gain = deduction_gain * marginal_tax_rate,
        at_baseline_2500_limit = positive_student_debt & abs(allowable_deduction_baseline - student_loan_interest_deduction_max) < 1e-8
      ) %>%
      filter(household_group == "Married household", at_baseline_2500_limit, !is.na(black_nonblack))

    point_groups <- compute_group_summary(
      scenario_df,
      "weight",
      census_shares$black_population_share[[1]],
      census_shares$nonblack_population_share[[1]]
    )

    rep_group_estimates <- map_dfr(rep_cols, function(rc) {
      compute_group_summary(
        scenario_df,
        rc,
        census_shares$black_population_share[[1]],
        census_shares$nonblack_population_share[[1]]
      ) %>%
        transmute(
          replicate = rc,
          black_nonblack,
          weighted_population,
          mean_deduction_gain,
          mean_tax_savings_gain,
          total_tax_savings_gain,
          tax_relief_share,
          tax_relief_to_population_ratio
        )
    })

    group_with_se <- point_groups %>%
      rowwise() %>%
      mutate(
        `SE: weighted population` = replicate_se_from_estimates(weighted_population, rep_group_estimates$weighted_population[rep_group_estimates$black_nonblack == black_nonblack], variance_scale),
        `SE: mean deduction gain` = replicate_se_from_estimates(mean_deduction_gain, rep_group_estimates$mean_deduction_gain[rep_group_estimates$black_nonblack == black_nonblack], variance_scale),
        `SE: mean tax savings gain` = replicate_se_from_estimates(mean_tax_savings_gain, rep_group_estimates$mean_tax_savings_gain[rep_group_estimates$black_nonblack == black_nonblack], variance_scale),
        `SE: total tax savings gain` = replicate_se_from_estimates(total_tax_savings_gain, rep_group_estimates$total_tax_savings_gain[rep_group_estimates$black_nonblack == black_nonblack], variance_scale),
        `SE: tax relief share` = replicate_se_from_estimates(tax_relief_share, rep_group_estimates$tax_relief_share[rep_group_estimates$black_nonblack == black_nonblack], variance_scale),
        `SE: tax relief to population ratio` = replicate_se_from_estimates(tax_relief_to_population_ratio, rep_group_estimates$tax_relief_to_population_ratio[rep_group_estimates$black_nonblack == black_nonblack], variance_scale)
      ) %>%
      ungroup() %>%
      mutate(
        source = source_id,
        year = unique(df$year)[1],
        rate_scenario_id = interest_rate_scenarios$rate_scenario_id[[i]],
        rate_scenario = interest_rate_scenarios$rate_scenario[[i]],
        annual_interest_rate = interest_rate_scenarios$annual_interest_rate[[i]],
        household_group = "Married household",
        n_obs = nrow(scenario_df),
        n_positive_student_debt = sum(scenario_df$positive_student_debt, na.rm = TRUE)
      ) %>%
      relocate(source, year, rate_scenario_id, rate_scenario, annual_interest_rate, household_group, black_nonblack)

    total_relief_all <- unique(point_groups$total_tax_savings_all_groups)[1]
    total_relief_reps <- map_dbl(rep_cols, function(rc) {
      sum(scenario_df[[rc]] * scenario_df$tax_savings_gain, na.rm = TRUE)
    })

    table_rows[[i]] <- bind_rows(
      tibble(
        source = source_id,
        year = unique(df$year)[1],
        rate_scenario_id = interest_rate_scenarios$rate_scenario_id[[i]],
        rate_scenario = interest_rate_scenarios$rate_scenario[[i]],
        group = "All binding-constraint married households",
        impacted_households = sum(scenario_df$weight[scenario_df$weight > 0], na.rm = TRUE),
        `SE: impacted households` = replicate_se_from_estimates(
          sum(scenario_df$weight[scenario_df$weight > 0], na.rm = TRUE),
          map_dbl(rep_cols, function(rc) sum(scenario_df[[rc]][scenario_df[[rc]] > 0], na.rm = TRUE)),
          variance_scale
        ),
        total_estimated_tax_savings_gain = total_relief_all,
        `SE: total estimated tax savings gain` = replicate_se_from_estimates(total_relief_all, total_relief_reps, variance_scale),
        tax_relief_share = 1,
        `SE: tax relief share` = 0,
        population_share = NA_real_,
        tax_relief_to_population_ratio = NA_real_,
        `SE: tax relief to population ratio` = NA_real_
      ),
      group_with_se %>%
        transmute(
          source,
          year,
          rate_scenario_id,
          rate_scenario,
          group = black_nonblack,
          impacted_households = weighted_population,
          `SE: impacted households` = `SE: weighted population`,
          total_estimated_tax_savings_gain = total_tax_savings_gain,
          `SE: total estimated tax savings gain` = `SE: total tax savings gain`,
          tax_relief_share,
          `SE: tax relief share`,
          population_share,
          tax_relief_to_population_ratio,
          `SE: tax relief to population ratio`
        )
    )

    group_rows[[i]] <- group_with_se
  }

  list(
    binding_groups = bind_rows(group_rows),
    relief_summary = bind_rows(table_rows)
  )
}

compute_scf_binding_outputs <- function(df, census_shares, source_id = "SCF") {
  rep_cols <- names(df)[grepl("^wt1b[0-9]+$", names(df))]
  year_value <- unique(df$year)[1]

  summarise_binding_groups <- function(data, weight_var) {
    w <- data[[weight_var]]
    total_relief_all <- sum(w * data$tax_savings_gain, na.rm = TRUE)

    data %>%
      filter(!is.na(black_nonblack)) %>%
      group_by(black_nonblack) %>%
      summarise(
        weighted_population = sum(.data[[weight_var]][.data[[weight_var]] > 0], na.rm = TRUE),
        mean_deduction_gain = weighted_mean_safe(deduction_gain, .data[[weight_var]]),
        mean_tax_savings_gain = weighted_mean_safe(tax_savings_gain, .data[[weight_var]]),
        total_tax_savings_gain = sum(.data[[weight_var]] * tax_savings_gain, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        total_tax_savings_all_groups = total_relief_all,
        tax_relief_share = if (isTRUE(total_relief_all > 0)) total_tax_savings_gain / total_relief_all else NA_real_,
        population_share = case_when(
          black_nonblack == "Black" ~ census_shares$black_population_share[[1]],
          black_nonblack == "Non-Black" ~ census_shares$nonblack_population_share[[1]],
          TRUE ~ NA_real_
        ),
        tax_relief_to_population_ratio = tax_relief_share / population_share
      )
  }

  group_rows <- list()
  table_rows <- list()

  comparison_metric_names <- c(
    "weighted_population",
    "mean_deduction_gain",
    "mean_tax_savings_gain",
    "total_tax_savings_gain",
    "tax_relief_share",
    "tax_relief_to_population_ratio"
  )

  for (i in seq_len(nrow(interest_rate_scenarios))) {
    scenario_df <- df %>%
      mutate(
        rate_scenario_id = interest_rate_scenarios$rate_scenario_id[[i]],
        rate_scenario = interest_rate_scenarios$rate_scenario[[i]],
        annual_interest_rate = interest_rate_scenarios$annual_interest_rate[[i]],
        positive_student_debt = student_debt > 0,
        interest_paid = if_else(positive_student_debt, student_debt * annual_interest_rate, 0),
        baseline_cap = student_loan_interest_deduction_max,
        proposed_cap = student_loan_interest_deduction_max_married_proposed,
        allowable_deduction_baseline = allowable_deduction_from_magi(pmin(interest_paid, baseline_cap), income, filing_status),
        allowable_deduction_proposed = allowable_deduction_from_magi(pmin(interest_paid, proposed_cap), income, filing_status),
        marginal_tax_rate = marginal_tax_rate_from_income(income, filing_status),
        deduction_gain = allowable_deduction_proposed - allowable_deduction_baseline,
        tax_savings_gain = deduction_gain * marginal_tax_rate,
        at_baseline_2500_limit = positive_student_debt & abs(allowable_deduction_baseline - student_loan_interest_deduction_max) < 1e-8
      ) %>%
      filter(household_group == "Married household", at_baseline_2500_limit, !is.na(black_nonblack))

    point_estimates <- scenario_df %>%
      group_by(implicate_number) %>%
      group_modify(~ summarise_binding_groups(.x, "weight")) %>%
      ungroup()

    point_long <- point_estimates %>%
      pivot_longer(
        cols = all_of(comparison_metric_names),
        names_to = "metric",
        values_to = "estimate"
      )

    point_summary <- point_long %>%
      group_by(black_nonblack, metric) %>%
      summarise(
        estimate = mean(estimate, na.rm = TRUE),
        imputation_variance = stats::var(estimate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(imputation_variance = ifelse(is.na(imputation_variance), 0, imputation_variance))

    sampling_variances <- scenario_df %>%
      filter(implicate_number == 1) %>%
      group_split() %>%
      { bind_rows(lapply(rep_cols, function(rc) {
          summarise_binding_groups(.[[1]], rc) %>%
            pivot_longer(
              cols = all_of(comparison_metric_names),
              names_to = "metric",
              values_to = "replicate_estimate"
            ) %>%
            mutate(replicate = rc)
        })) } %>%
      group_by(black_nonblack, metric) %>%
      summarise(sampling_variance = stats::var(replicate_estimate, na.rm = TRUE), .groups = "drop") %>%
      mutate(sampling_variance = ifelse(is.na(sampling_variance), 0, sampling_variance))

    group_with_se <- point_summary %>%
      left_join(sampling_variances, by = c("black_nonblack", "metric")) %>%
      mutate(
        sampling_variance = ifelse(is.na(sampling_variance), 0, sampling_variance),
        total_variance = sampling_variance + (6 / 5) * imputation_variance,
        se = sqrt(pmax(total_variance, 0))
      ) %>%
      select(black_nonblack, metric, estimate, se) %>%
      pivot_wider(names_from = metric, values_from = c(estimate, se), names_glue = "{.value}__{metric}") %>%
      left_join(
        scenario_df %>%
          filter(!is.na(black_nonblack)) %>%
          group_by(black_nonblack) %>%
          summarise(
            n_obs = n(),
            n_positive_student_debt = sum(positive_student_debt, na.rm = TRUE),
            .groups = "drop"
          ),
        by = "black_nonblack"
      ) %>%
      transmute(
        source = source_id,
        year = year_value,
        rate_scenario_id = interest_rate_scenarios$rate_scenario_id[[i]],
        rate_scenario = interest_rate_scenarios$rate_scenario[[i]],
        annual_interest_rate = interest_rate_scenarios$annual_interest_rate[[i]],
        household_group = "Married household",
        black_nonblack,
        weighted_population = estimate__weighted_population,
        `SE: weighted population` = se__weighted_population,
        mean_deduction_gain = estimate__mean_deduction_gain,
        `SE: mean deduction gain` = se__mean_deduction_gain,
        mean_tax_savings_gain = estimate__mean_tax_savings_gain,
        `SE: mean tax savings gain` = se__mean_tax_savings_gain,
        total_tax_savings_gain = estimate__total_tax_savings_gain,
        `SE: total tax savings gain` = se__total_tax_savings_gain,
        tax_relief_share = estimate__tax_relief_share,
        `SE: tax relief share` = se__tax_relief_share,
        population_share = case_when(
          black_nonblack == "Black" ~ census_shares$black_population_share[[1]],
          black_nonblack == "Non-Black" ~ census_shares$nonblack_population_share[[1]],
          TRUE ~ NA_real_
        ),
        tax_relief_to_population_ratio = estimate__tax_relief_to_population_ratio,
        `SE: tax relief to population ratio` = se__tax_relief_to_population_ratio,
        n_obs,
        n_positive_student_debt
      )

    total_by_implicate <- scenario_df %>%
      group_by(implicate_number) %>%
      summarise(total_estimated_tax_savings_gain = sum(weight * tax_savings_gain, na.rm = TRUE), .groups = "drop")

    total_point <- mean(total_by_implicate$total_estimated_tax_savings_gain, na.rm = TRUE)
    total_imp_var <- stats::var(total_by_implicate$total_estimated_tax_savings_gain, na.rm = TRUE)
    if (is.na(total_imp_var)) total_imp_var <- 0

    total_sampling_reps <- vapply(rep_cols, function(rc) {
      data <- scenario_df %>% filter(implicate_number == 1)
      sum(data[[rc]] * data$tax_savings_gain, na.rm = TRUE)
    }, numeric(1))
    total_sampling_var <- stats::var(total_sampling_reps, na.rm = TRUE)
    if (is.na(total_sampling_var)) total_sampling_var <- 0

    table_rows[[i]] <- bind_rows(
      tibble(
        source = source_id,
        year = year_value,
        rate_scenario_id = interest_rate_scenarios$rate_scenario_id[[i]],
        rate_scenario = interest_rate_scenarios$rate_scenario[[i]],
        group = "All binding-constraint married households",
        impacted_households = mean(total_by_implicate$total_estimated_tax_savings_gain * 0 + scenario_df %>%
          group_by(implicate_number) %>%
          summarise(weighted_population = sum(weight[weight > 0], na.rm = TRUE), .groups = "drop") %>%
          pull(weighted_population), na.rm = TRUE),
        `SE: impacted households` = {
          total_pop_by_imp <- scenario_df %>%
            group_by(implicate_number) %>%
            summarise(weighted_population = sum(weight[weight > 0], na.rm = TRUE), .groups = "drop")
          total_pop_point <- mean(total_pop_by_imp$weighted_population, na.rm = TRUE)
          total_pop_imp_var <- stats::var(total_pop_by_imp$weighted_population, na.rm = TRUE)
          if (is.na(total_pop_imp_var)) total_pop_imp_var <- 0
          total_pop_sampling_reps <- vapply(rep_cols, function(rc) {
            data <- scenario_df %>% filter(implicate_number == 1)
            sum(data[[rc]][data[[rc]] > 0], na.rm = TRUE)
          }, numeric(1))
          total_pop_sampling_var <- stats::var(total_pop_sampling_reps, na.rm = TRUE)
          if (is.na(total_pop_sampling_var)) total_pop_sampling_var <- 0
          sqrt(pmax(total_pop_sampling_var + (6 / 5) * total_pop_imp_var, 0))
        },
        total_estimated_tax_savings_gain = total_point,
        `SE: total estimated tax savings gain` = sqrt(pmax(total_sampling_var + (6 / 5) * total_imp_var, 0)),
        tax_relief_share = 1,
        `SE: tax relief share` = 0,
        population_share = NA_real_,
        tax_relief_to_population_ratio = NA_real_,
        `SE: tax relief to population ratio` = NA_real_
      ),
      group_with_se %>%
        transmute(
          source,
          year,
          rate_scenario_id,
          rate_scenario,
          group = black_nonblack,
          impacted_households = weighted_population,
          `SE: impacted households` = `SE: weighted population`,
          total_estimated_tax_savings_gain = total_tax_savings_gain,
          `SE: total estimated tax savings gain` = `SE: total tax savings gain`,
          tax_relief_share,
          `SE: tax relief share`,
          population_share,
          tax_relief_to_population_ratio,
          `SE: tax relief to population ratio`
        )
    )

    group_rows[[i]] <- group_with_se
  }

  list(
    binding_groups = bind_rows(group_rows),
    relief_summary = bind_rows(table_rows)
  )
}

add_ci_columns <- function(df, estimate_col, se_col, prefix, floor_zero = TRUE) {
  ci <- compute_95_ci(df[[estimate_col]], df[[se_col]], floor_zero = floor_zero)
  df[[paste0("lower_95_", prefix)]] <- ci$lower_95
  df[[paste0("upper_95_", prefix)]] <- ci$upper_95
  df
}

census_shares <- get_census_black_share()

sipp_file <- file.path(paths$sipp_harmonized_dir, "sipp_household_year_2024.rds")
scf_file <- file.path(paths$scf_harmonized_dir, "scf_household_year_2022.rds")

sipp_outputs <- compute_sipp_binding_outputs(readRDS(sipp_file), census_shares, "SIPP")
scf_outputs <- compute_scf_binding_outputs(readRDS(scf_file), census_shares, "SCF")

binding_groups <- bind_rows(sipp_outputs$binding_groups, scf_outputs$binding_groups) %>%
  add_ci_columns("mean_deduction_gain", "SE: mean deduction gain", "mean_deduction_gain", floor_zero = TRUE) %>%
  add_ci_columns("mean_tax_savings_gain", "SE: mean tax savings gain", "mean_tax_savings_gain", floor_zero = TRUE) %>%
  add_ci_columns("tax_relief_to_population_ratio", "SE: tax relief to population ratio", "tax_relief_to_population_ratio", floor_zero = TRUE)

relief_summary <- bind_rows(sipp_outputs$relief_summary, scf_outputs$relief_summary) %>%
  add_ci_columns("impacted_households", "SE: impacted households", "impacted_households", floor_zero = TRUE) %>%
  add_ci_columns("total_estimated_tax_savings_gain", "SE: total estimated tax savings gain", "total_estimated_tax_savings_gain", floor_zero = TRUE) %>%
  add_ci_columns("tax_relief_share", "SE: tax relief share", "tax_relief_share", floor_zero = TRUE) %>%
  add_ci_columns("tax_relief_to_population_ratio", "SE: tax relief to population ratio", "tax_relief_to_population_ratio", floor_zero = TRUE) %>%
  group_by(source, year, rate_scenario_id, rate_scenario) %>%
  mutate(
    total_impacted_households = impacted_households[group == "All binding-constraint married households"][1],
    total_impacted_households_se = `SE: impacted households`[group == "All binding-constraint married households"][1],
    percent_of_impacted_households = dplyr::if_else(
      group == "All binding-constraint married households",
      100,
      100 * impacted_households / total_impacted_households
    ),
    `SE: percent of impacted households` = dplyr::if_else(
      group == "All binding-constraint married households",
      0,
      100 * abs(impacted_households / total_impacted_households) * sqrt(
        (`SE: impacted households` / impacted_households)^2 +
          (total_impacted_households_se / total_impacted_households)^2
      )
    )
  ) %>%
  ungroup() %>%
  add_ci_columns(
    "percent_of_impacted_households",
    "SE: percent of impacted households",
    "percent_of_impacted_households",
    floor_zero = TRUE
  ) %>%
  select(-total_impacted_households, -total_impacted_households_se)

sipp_binding_out <- file.path(paths$outmoded_sipp_csv_dir, "sipp_binding_constraint_married_cap_black_nonblack.csv")
scf_binding_out <- file.path(paths$scf_csv_dir, "scf_binding_constraint_married_cap_black_nonblack.csv")
summary_out <- file.path(paths$output_csv_dir, "binding_constraint_tax_relief_summary.csv")
census_out <- file.path(paths$output_csv_dir, "black_population_share_census_b02009.csv")

write_csv(binding_groups %>% filter(source == "SIPP"), sipp_binding_out, na = "")
write_csv(binding_groups %>% filter(source == "SCF"), scf_binding_out, na = "")
write_csv(relief_summary, summary_out, na = "")
write_csv(census_shares, census_out, na = "")

cat("Wrote:", sipp_binding_out, "\n")
cat("Wrote:", scf_binding_out, "\n")
cat("Wrote:", summary_out, "\n")
cat("Wrote:", census_out, "\n")
