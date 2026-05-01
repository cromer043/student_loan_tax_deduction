#!/usr/bin/env Rscript

raw_script_path <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1])
script_path <- normalizePath(gsub("~\\+~", " ", raw_script_path), winslash = "/", mustWork = FALSE)
code_dir <- dirname(script_path)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(code_dir, "student_loan_interest_workflow_utils.R"))
paths <- get_workflow_paths(project_dir)

suppressPackageStartupMessages({
  library(dplyr)
  library(gt)
  library(readr)
  library(scales)
  library(tidyr)
})

summary_file <- file.path(paths$output_csv_dir, "binding_constraint_tax_relief_summary.csv")
summary_df <- read_csv(summary_file, show_col_types = FALSE)

fmt_dollar_ci <- function(est, lo, hi) {
  paste0(dollar(est), " (", dollar(lo), ", ", dollar(hi), ")")
}

fmt_count_ci <- function(est, lo, hi) {
  paste0(
    comma(round(est)),
    " (",
    comma(round(lo)),
    ", ",
    comma(round(hi)),
    ")"
  )
}

fmt_percent_ci <- function(est, lo, hi, accuracy = 0.1) {
  paste0(number(est, accuracy = accuracy), "% (", number(lo, accuracy = accuracy), ", ", number(hi, accuracy = accuracy), ")")
}

build_source_table <- function(source_id, out_dir) {
  source_year <- unique(summary_df$year[summary_df$source == source_id])[1]
  source_label <- if (source_id == "SCF") {
    paste0("SCF ", source_year)
  } else {
    paste0("SIPP ", source_year)
  }

  table_df <- summary_df %>%
    filter(source == source_id) %>%
    mutate(
      rate = case_when(
        grepl("2.75%", rate_scenario, fixed = TRUE) ~ "2.75%",
        grepl("6.8%", rate_scenario, fixed = TRUE) ~ "6.8%",
        TRUE ~ rate_scenario
      ),
      group = factor(group, levels = c("All binding-constraint married households", "Black", "Non-Black")),
      impacted_households_display = fmt_count_ci(impacted_households, lower_95_impacted_households, upper_95_impacted_households),
      percent_households_display = fmt_percent_ci(percent_of_impacted_households, lower_95_percent_of_impacted_households, upper_95_percent_of_impacted_households),
      tax_relief_display = fmt_dollar_ci(total_estimated_tax_savings_gain, lower_95_total_estimated_tax_savings_gain, upper_95_total_estimated_tax_savings_gain),
      tax_relief_share_display = fmt_percent_ci(100 * tax_relief_share, 100 * lower_95_tax_relief_share, 100 * upper_95_tax_relief_share),
      ratio_display = if_else(
        is.na(tax_relief_to_population_ratio),
        "",
        paste0(
          number(tax_relief_to_population_ratio, accuracy = 0.01),
          " (",
          number(lower_95_tax_relief_to_population_ratio, accuracy = 0.01),
          ", ",
          number(upper_95_tax_relief_to_population_ratio, accuracy = 0.01),
          ")"
        )
      )
    ) %>%
    select(
      rate,
      group,
      impacted_households_display,
      percent_households_display,
      tax_relief_display,
      tax_relief_share_display,
      ratio_display
    ) %>%
    arrange(rate, group)

  gt_tbl <- table_df %>%
    gt(groupname_col = "rate", rowname_col = "group") %>%
    tab_header(
      title = md(paste0("**Binding-Constraint Married-Cap Summary: ", source_label, "**")),
      subtitle = md("Married households at the current $2,500 binding allowable-deduction constraint")
    ) %>%
    cols_label(
      impacted_households_display = "Impacted Households",
      percent_households_display = "Percent of Impacted Households",
      tax_relief_display = "Total Estimated Tax Relief",
      tax_relief_share_display = "Share of Tax Relief",
      ratio_display = "Relief Share / Population Share"
    ) %>%
    tab_spanner(label = "Estimate (95% CI)", columns = c(impacted_households_display, percent_households_display, tax_relief_display, tax_relief_share_display, ratio_display)) %>%
    tab_source_note(
      source_note = md(
        paste0(
          "Author's analysis of ", source_year, " ",
          if (source_id == "SCF") "Survey of Consumer Finances" else "Survey of Income and Program Participation",
          " data. Census population benchmark uses 2024 ACS 5-year table B02009 divided by B01003. ",
          "Confidence intervals are approximate 95% confidence intervals."
        )
      )
    ) %>%
    opt_row_striping() %>%
    tab_options(
      table.font.names = c("Avenir Next", "Helvetica Neue", "Arial", "sans-serif"),
      table.font.size = px(14),
      heading.title.font.size = px(22),
      heading.subtitle.font.size = px(14),
      data_row.padding = px(8),
      source_notes.font.size = px(12),
      row_group.font.weight = "bold",
      table.border.top.color = "#1f2937",
      table.border.bottom.color = "#1f2937",
      heading.border.bottom.color = "#d1d5db",
      column_labels.border.bottom.color = "#d1d5db",
      column_labels.font.weight = "bold",
      column_labels.font.size = px(13),
      table.width = pct(100)
    ) %>%
    tab_style(
      style = list(cell_fill(color = "#f3f4f6"), cell_text(weight = "bold")),
      locations = cells_row_groups()
    ) %>%
    cols_align(align = "center", columns = everything()) %>%
    cols_width(
      impacted_households_display ~ px(180),
      percent_households_display ~ px(170),
      tax_relief_display ~ px(180),
      tax_relief_share_display ~ px(150),
      ratio_display ~ px(170)
    )

  html_file <- file.path(out_dir, paste0("Binding Constraint Married-Cap Summary Table - ", source_id, ".html"))
  rtf_file <- file.path(out_dir, paste0("Binding Constraint Married-Cap Summary Table - ", source_id, ".rtf"))

  gtsave(gt_tbl, html_file)
  gtsave(gt_tbl, rtf_file)

  cat("Wrote:", html_file, "\n")
  cat("Wrote:", rtf_file, "\n")
}

build_source_table("SIPP", paths$sipp_graph_dir)
build_source_table("SCF", paths$scf_graph_dir)
