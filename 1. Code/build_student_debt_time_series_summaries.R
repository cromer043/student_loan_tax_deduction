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
  library(tibble)
})

sipp_years <- 2018:2024
scf_years <- c(2016, 2019, 2022)

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

summarise_sipp_group <- function(df, rep_cols) {
  if (!"positive_student_debt" %in% names(df)) {
    df <- df %>% mutate(positive_student_debt = student_debt > 0)
  }
  positive_df <- df %>% filter(positive_student_debt)
  point <- tibble(
    unweighted_n = nrow(df),
    weighted_denominator = sum(df$weight[df$weight > 0], na.rm = TRUE),
    percent_with_student_debt = weighted_percent_safe(df$positive_student_debt, df$weight),
    median_student_debt_positive = weighted_median_safe(positive_df$student_debt, positive_df$weight),
    mean_student_debt_positive = weighted_mean_safe(positive_df$student_debt, positive_df$weight)
  )

  if (length(rep_cols) == 0) {
    return(bind_cols(
      point,
      tibble(
        se_percent_with_student_debt = NA_real_,
        se_median_student_debt_positive = NA_real_,
        se_mean_student_debt_positive = NA_real_
      )
    ))
  }

  scale <- 1 / (length(rep_cols) * 0.5^2)
  rep_estimates <- map_dfr(rep_cols, function(rc) {
    positive_rep <- positive_df %>% filter(!is.na(.data[[rc]]))
    tibble(
      replicate = rc,
      percent_with_student_debt = weighted_percent_safe(df$positive_student_debt, df[[rc]]),
      median_student_debt_positive = weighted_median_safe(positive_rep$student_debt, positive_rep[[rc]]),
      mean_student_debt_positive = weighted_mean_safe(positive_rep$student_debt, positive_rep[[rc]])
    )
  })

  se_percent <- sqrt(scale * sum((rep_estimates$percent_with_student_debt - point$percent_with_student_debt)^2, na.rm = TRUE))
  se_median <- sqrt(scale * sum((rep_estimates$median_student_debt_positive - point$median_student_debt_positive)^2, na.rm = TRUE))
  se_mean <- sqrt(scale * sum((rep_estimates$mean_student_debt_positive - point$mean_student_debt_positive)^2, na.rm = TRUE))

  bind_cols(
    point,
    tibble(
      se_percent_with_student_debt = se_percent,
      se_median_student_debt_positive = se_median,
      se_mean_student_debt_positive = se_mean
    )
  )
}

build_sipp_time_series <- function(group_var) {
  rows <- list()

  for (year in sipp_years) {
    path <- file.path(paths$sipp_harmonized_dir, paste0("sipp_household_year_", year, ".rds"))
    if (!file.exists(path)) next
    df <- readRDS(path)
    rep_cols <- names(df)[grepl("^REPWGT[0-9]+$", names(df))]

    grouped <- df %>%
      filter(!is.na(.data[[group_var]]), !is.na(household_group)) %>%
      group_by(year, household_group, group_value = .data[[group_var]]) %>%
      group_modify(~ summarise_sipp_group(.x, rep_cols)) %>%
      ungroup() %>%
      mutate(source = "SIPP")

    rows[[as.character(year)]] <- grouped
  }

  bind_rows(rows)
}

summarise_scf_implicate_group <- function(df) {
  if (!"positive_student_debt" %in% names(df)) {
    df <- df %>% mutate(positive_student_debt = student_debt > 0)
  }
  positive_df <- df %>% filter(positive_student_debt)
  tibble(
    unweighted_n = nrow(df),
    weighted_denominator = sum(df$weight[df$weight > 0], na.rm = TRUE),
    percent_with_student_debt = weighted_percent_safe(df$positive_student_debt, df$weight),
    median_student_debt_positive = weighted_median_safe(positive_df$student_debt, positive_df$weight),
    mean_student_debt_positive = weighted_mean_safe(positive_df$student_debt, positive_df$weight)
  )
}

summarise_scf_replicate_group <- function(df, rep_cols) {
  if (!"positive_student_debt" %in% names(df)) {
    df <- df %>% mutate(positive_student_debt = student_debt > 0)
  }
  positive_df <- df %>% filter(positive_student_debt)
  map_dfr(rep_cols, function(rc) {
    tibble(
      replicate = rc,
      percent_with_student_debt = weighted_percent_safe(df$positive_student_debt, df[[rc]]),
      median_student_debt_positive = weighted_median_safe(positive_df$student_debt, positive_df[[rc]]),
      mean_student_debt_positive = weighted_mean_safe(positive_df$student_debt, positive_df[[rc]])
    )
  })
}

build_scf_time_series <- function(group_var) {
  rows <- list()

  for (year in scf_years) {
    path <- file.path(paths$scf_harmonized_dir, paste0("scf_household_year_", year, ".rds"))
    if (!file.exists(path)) next
    df <- readRDS(path)
    rep_cols <- names(df)[grepl("^wt1b[0-9]+$", names(df))]

    point_by_implicate <- df %>%
      filter(!is.na(.data[[group_var]]), !is.na(household_group)) %>%
      group_by(year, implicate_number, household_group, group_value = .data[[group_var]]) %>%
      group_modify(~ summarise_scf_implicate_group(.x)) %>%
      ungroup()

    point_summary <- point_by_implicate %>%
      group_by(year, household_group, group_value) %>%
      summarise(
        unweighted_n = mean(unweighted_n, na.rm = TRUE),
        weighted_denominator = mean(weighted_denominator, na.rm = TRUE),
        percent_with_student_debt = mean(percent_with_student_debt, na.rm = TRUE),
        percent_imp_var = stats::var(percent_with_student_debt, na.rm = TRUE),
        median_student_debt_positive = mean(median_student_debt_positive, na.rm = TRUE),
        median_imp_var = stats::var(median_student_debt_positive, na.rm = TRUE),
        mean_student_debt_positive = mean(mean_student_debt_positive, na.rm = TRUE),
        mean_imp_var = stats::var(mean_student_debt_positive, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(across(ends_with("_imp_var"), ~ ifelse(is.na(.x), 0, .x)))

    sampling_groups <- df %>%
      filter(implicate_number == 1, !is.na(.data[[group_var]]), !is.na(household_group)) %>%
      group_by(year, household_group, group_value = .data[[group_var]]) %>%
      group_split()

    sampling_summary <- bind_rows(lapply(sampling_groups, function(g) {
      rep_est <- summarise_scf_replicate_group(g, rep_cols)
      tibble(
        year = unique(g$year)[1],
        household_group = unique(g$household_group)[1],
        group_value = unique(g[[group_var]])[1],
        percent_sampling_var = stats::var(rep_est$percent_with_student_debt, na.rm = TRUE),
        median_sampling_var = stats::var(rep_est$median_student_debt_positive, na.rm = TRUE),
        mean_sampling_var = stats::var(rep_est$mean_student_debt_positive, na.rm = TRUE)
      )
    })) %>%
      mutate(across(ends_with("_sampling_var"), ~ ifelse(is.na(.x), 0, .x)))

    rows[[as.character(year)]] <- point_summary %>%
      left_join(sampling_summary, by = c("year", "household_group", "group_value")) %>%
      mutate(
        se_percent_with_student_debt = sqrt(pmax(percent_sampling_var + (6 / 5) * percent_imp_var, 0)),
        se_median_student_debt_positive = sqrt(pmax(median_sampling_var + (6 / 5) * median_imp_var, 0)),
        se_mean_student_debt_positive = sqrt(pmax(mean_sampling_var + (6 / 5) * mean_imp_var, 0)),
        source = "SCF"
      ) %>%
      select(
        source, year, household_group, group_value,
        percent_with_student_debt, se_percent_with_student_debt,
        median_student_debt_positive, se_median_student_debt_positive,
        mean_student_debt_positive, se_mean_student_debt_positive,
        unweighted_n, weighted_denominator
      )
  }

  bind_rows(rows)
}

finalize_time_series <- function(df, grouping) {
  if (nrow(df) == 0) return(df)
  df <- df %>%
    bind_cols(compute_95_ci(.$percent_with_student_debt, .$se_percent_with_student_debt, floor_zero = TRUE) %>% rename(lower_95_percent_with_student_debt = lower_95, upper_95_percent_with_student_debt = upper_95)) %>%
    bind_cols(compute_95_ci(.$median_student_debt_positive, .$se_median_student_debt_positive, floor_zero = TRUE) %>% rename(lower_95_median_student_debt_positive = lower_95, upper_95_median_student_debt_positive = upper_95)) %>%
    bind_cols(compute_95_ci(.$mean_student_debt_positive, .$se_mean_student_debt_positive, floor_zero = TRUE) %>% rename(lower_95_mean_student_debt_positive = lower_95, upper_95_mean_student_debt_positive = upper_95))

  if (grouping == "race") {
    df <- df %>% mutate(race = group_value, black_nonblack = NA_character_)
  } else {
    df <- df %>% mutate(black_nonblack = group_value, race = NA_character_)
  }

  df %>%
    select(
      source, year, household_group, race, black_nonblack,
      percent_with_student_debt, se_percent_with_student_debt, lower_95_percent_with_student_debt, upper_95_percent_with_student_debt,
      median_student_debt_positive, se_median_student_debt_positive, lower_95_median_student_debt_positive, upper_95_median_student_debt_positive,
      mean_student_debt_positive, se_mean_student_debt_positive, lower_95_mean_student_debt_positive, upper_95_mean_student_debt_positive,
      unweighted_n, weighted_denominator
    ) %>%
    arrange(year, household_group, coalesce(race, black_nonblack))
}

sipp_black_nonblack <- finalize_time_series(build_sipp_time_series("black_nonblack"), "black_nonblack")
sipp_race <- finalize_time_series(build_sipp_time_series("race"), "race")
scf_black_nonblack <- finalize_time_series(build_scf_time_series("black_nonblack"), "black_nonblack")
scf_race <- finalize_time_series(build_scf_time_series("race"), "race")

write_csv(sipp_black_nonblack, file.path(paths$outmoded_sipp_csv_dir, "sipp_student_debt_time_series_black_nonblack.csv"), na = "")
write_csv(sipp_race, file.path(paths$outmoded_sipp_csv_dir, "sipp_student_debt_time_series_by_race.csv"), na = "")
write_csv(scf_black_nonblack, file.path(paths$scf_csv_dir, "scf_student_debt_time_series_black_nonblack.csv"), na = "")
write_csv(scf_race, file.path(paths$outmoded_scf_csv_dir, "scf_student_debt_time_series_by_race.csv"), na = "")

cat("Wrote:", file.path(paths$outmoded_sipp_csv_dir, "sipp_student_debt_time_series_black_nonblack.csv"), "\n")
cat("Wrote:", file.path(paths$outmoded_sipp_csv_dir, "sipp_student_debt_time_series_by_race.csv"), "\n")
cat("Wrote:", file.path(paths$scf_csv_dir, "scf_student_debt_time_series_black_nonblack.csv"), "\n")
cat("Wrote:", file.path(paths$outmoded_scf_csv_dir, "scf_student_debt_time_series_by_race.csv"), "\n")
