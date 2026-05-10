#!/usr/bin/env Rscript

raw_script_path <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1])
script_path <- normalizePath(gsub("~\\+~", " ", raw_script_path), winslash = "/", mustWork = FALSE)
code_dir <- dirname(script_path)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(code_dir, "student_loan_interest_workflow_utils.R"))
paths <- get_workflow_paths(project_dir)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(tidyr)
  library(tibble)
})

latest_years <- c(SCF = 2022, SIPP = 2024)
rate_levels <- c("Estimated interest at 2.75% annual rate", "Estimated interest at 6.8% annual rate")
household_levels <- c("Married household", "Unmarried household")
fill_values <- c(
  "Estimated interest at 2.75% annual rate" = "#2C7FB8",
  "Estimated interest at 6.8% annual rate" = "#D95F0E"
)
group_colors <- c(
  "Black" = "#1b4332",
  "Non-Black" = "#c1121f",
  "White alone" = "#577590",
  "Black alone" = "#1b4332",
  "Hispanic" = "#ee9b00",
  "Other" = "#9c6644"
)

source_note_text <- function(source_id) {
  source_year <- latest_years[[source_id]]
  source_label <- dplyr::case_when(
    source_id == "SCF" ~ "Survey of Consumer Finances",
    source_id == "SIPP" ~ "Survey of Income and Program Participation",
    TRUE ~ "survey"
  )
  paste0("Author's analysis of ", source_year, " ", source_label, " data.")
}

wrap_plot_title <- function(x, width = 52) stringr::str_wrap(x, width = width)
wrap_plot_subtitle <- function(x, width = 78) stringr::str_wrap(x, width = width)
wrap_plot_caption <- function(x, width = 118) stringr::str_wrap(x, width = width)

add_plot_ci <- function(df, estimate_col, se_col, floor_zero = TRUE) {
  ci <- compute_95_ci(df[[estimate_col]], df[[se_col]], floor_zero = floor_zero)
  df$lower_95 <- ci$lower_95
  df$upper_95 <- ci$upper_95
  df
}

build_caption_text <- function(source_id, body_lines) {
  wrap_plot_caption(paste(c(source_note_text(source_id), body_lines), collapse = " "))
}

save_plot_dual <- function(output_file, plot_obj, width, height, dpi = 300) {
  pdf_file <- sub("\\.[Pp][Dd][Ff]$", ".pdf", output_file)
  png_file <- sub("\\.[Pp][Dd][Ff]$", ".png", output_file)

  ggsave(pdf_file, plot_obj, width = width, height = height, dpi = dpi)
  ggsave(png_file, plot_obj, width = width, height = height, dpi = dpi)

  cat("Wrote:", pdf_file, "\n")
  cat("Wrote:", png_file, "\n")
}

add_within_household_order <- function(df, group_col, ranking_col) {
  ordering <- df %>%
    filter(rate_scenario == "Estimated interest at 6.8% annual rate") %>%
    group_by(household_group, .data[[group_col]]) %>%
    summarise(order_value = mean(.data[[ranking_col]], na.rm = TRUE), .groups = "drop") %>%
    arrange(household_group, desc(order_value), .data[[group_col]]) %>%
    mutate(order_key = paste(household_group, .data[[group_col]], sep = "___"))

  df %>%
    mutate(order_key = paste(household_group, .data[[group_col]], sep = "___")) %>%
    mutate(order_key = factor(order_key, levels = rev(ordering$order_key)))
}

comparison_caption_body <- paste(
  "Population: subgroup means among eligible households with positive student debt;",
  "unmarried households have zero modeled gain by design because their cap is unchanged.",
  "Tax savings equal allowable deduction times an estimated 2025 federal marginal tax rate.",
  "The 2.75% and 6.8% rates are the minimum and maximum undergraduate federal loan rates",
  "from 2000 through 2025; because borrower-specific loan terms are not observed,",
  "they show a plausible range of impacts. Error bars show 95% confidence intervals."
)

outmoded_caption_body <- paste(
  "Population: subgroup means among eligible households with positive student debt.",
  "The 2.75% and 6.8% rates are the minimum and maximum undergraduate federal loan rates",
  "from 2000 through 2025; because borrower-specific loan terms are not observed,",
  "they show a plausible range of impacts. Error bars show 95% confidence intervals."
)

binding_caption_body <- paste(
  "Population: married households at the current $2,500 binding allowable-deduction constraint only.",
  "Tax savings equal allowable deduction times an estimated 2025 federal marginal tax rate.",
  "The 2.75% and 6.8% rates are the minimum and maximum undergraduate federal loan rates",
  "from 2000 through 2025; because borrower-specific loan terms are not observed,",
  "they show a plausible range of impacts. Error bars show 95% confidence intervals."
)

ratio_caption_body <- function(source_id) {
  build_caption_text(
    source_id,
    paste(
      "Uses 2024 ACS 5-year Census table B02009 divided by B01003 for United States population shares.",
      "Population: married households at the current $2,500 binding allowable-deduction constraint only.",
      "Bars show each group's share of modeled tax relief divided by its Census population share;",
      "a value above 1 means the group receives a larger share of modeled relief than its population share.",
      "Error bars show 95% confidence intervals."
    )
  )
}

make_horizontal_bar_plot <- function(df, group_col, estimate_col, se_col, title_text, subtitle_text, x_label, output_file, source_id, percent_axis = FALSE, household_filter = NULL, caption_body = comparison_caption_body) {
  plot_df <- df %>%
    { if (!is.null(household_filter)) filter(., household_group == household_filter) else . } %>%
    transmute(
      household_group,
      rate_scenario,
      group_value = .data[[group_col]],
      estimate = .data[[estimate_col]],
      se = .data[[se_col]]
    ) %>%
    filter(!is.na(group_value), !is.na(estimate)) %>%
    add_plot_ci(estimate_col = "estimate", se_col = "se", floor_zero = TRUE)

  plot_df <- add_within_household_order(plot_df, "group_value", "estimate")
  axis_upper <- max(plot_df$upper_95, na.rm = TRUE) * 1.05

  p <- ggplot(plot_df, aes(x = estimate, y = order_key, fill = rate_scenario)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.62) +
    geom_errorbar(
      aes(xmin = lower_95, xmax = upper_95),
      position = position_dodge(width = 0.75),
      width = 0.18,
      linewidth = 0.35,
      alpha = 0.75
    ) +
    facet_wrap(~ household_group, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = fill_values, labels = c("2.75%", "6.8%"), name = "Interest rate") +
    scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
    labs(
      title = wrap_plot_title(title_text),
      subtitle = wrap_plot_subtitle(subtitle_text),
      x = x_label,
      y = NULL,
      caption = build_caption_text(source_id, caption_body)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, size = 8.7, lineheight = 1.05),
      plot.margin = margin(10, 12, 22, 10)
    )

  if (percent_axis) {
    p <- p + scale_x_continuous(
      labels = label_percent(scale = 1),
      limits = c(0, axis_upper),
      expand = expansion(mult = c(0, 0.01))
    )
  } else {
    p <- p + scale_x_continuous(
      labels = label_dollar(),
      limits = c(0, axis_upper),
      expand = expansion(mult = c(0, 0.01))
    )
  }

  save_plot_dual(output_file, p, width = 10, height = 7.5, dpi = 300)
}

make_gain_two_panel_plot <- function(df, output_file, source_id) {
  plot_df <- bind_rows(
    df %>%
      filter(household_group == "Married household") %>%
      transmute(
        household_group,
        rate_scenario,
        group_value = black_nonblack,
        measure = "Allowable deduction gain",
        estimate = mean_deduction_gain,
        se = `SE: mean deduction gain`
      ),
    df %>%
      filter(household_group == "Married household") %>%
      transmute(
        household_group,
        rate_scenario,
        group_value = black_nonblack,
        measure = "Estimated tax savings gain",
        estimate = mean_tax_savings_gain,
        se = `SE: mean tax savings gain`
      )
  ) %>%
    filter(!is.na(group_value), !is.na(estimate)) %>%
    add_plot_ci(estimate_col = "estimate", se_col = "se", floor_zero = TRUE)

  ordering <- plot_df %>%
    filter(rate_scenario == "Estimated interest at 6.8% annual rate", measure == "Allowable deduction gain") %>%
    group_by(group_value) %>%
    summarise(order_value = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(order_value), group_value)

  plot_df <- plot_df %>%
    mutate(
      group_value = factor(group_value, levels = rev(ordering$group_value)),
      measure = factor(measure, levels = c("Allowable deduction gain", "Estimated tax savings gain"))
    )

  axis_upper <- max(plot_df$upper_95, na.rm = TRUE) * 1.05

  p <- ggplot(plot_df, aes(x = estimate, y = group_value, fill = rate_scenario)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.62) +
    geom_errorbar(
      aes(xmin = lower_95, xmax = upper_95),
      position = position_dodge(width = 0.75),
      width = 0.18,
      linewidth = 0.35,
      alpha = 0.75
    ) +
    facet_wrap(~ measure, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = fill_values, labels = c("2.75%", "6.8%"), name = "Interest rate") +
    scale_x_continuous(
      trans = "sqrt",
      breaks = c(0, 25, 50, 100, 250, 500),
      labels = label_dollar(),
      limits = c(0, axis_upper),
      expand = expansion(mult = c(0, 0.01))
    ) +
    labs(
      title = wrap_plot_title("Figure 4: A Higher Married Cap Raises Deductions and Tax Savings for Married Black Households"),
      subtitle = wrap_plot_subtitle("Estimated allowable-deduction gains and estimated tax-savings gains from raising the married-household deduction cap to $5,000 for Black and Non-Black households."),
      x = "Dollars",
      y = NULL,
      caption = build_caption_text(source_id, comparison_caption_body)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, size = 8.7, lineheight = 1.05),
      plot.margin = margin(10, 12, 22, 10)
    )

  save_plot_dual(output_file, p, width = 10, height = 9.2, dpi = 300)
}

make_appendix_components_plot <- function(df, output_file, source_id) {
  measure_specs <- tribble(
    ~measure, ~estimate_col, ~se_col,
    "Estimated annual interest paid", "Estimated annual interest paid", "SE: estimated annual interest paid",
    "Max potential deduction", "Max potential deduction", "SE: max potential deduction",
    "Allowable deduction after MAGI phaseout", "Allowable deduction after MAGI phaseout", "SE: allowable deduction after MAGI phaseout",
    "Estimated tax savings", "Estimated tax savings", "SE: estimated tax savings"
  )

  appendix_df <- bind_rows(lapply(seq_len(nrow(measure_specs)), function(i) {
    spec <- measure_specs[i, ]
    df %>%
      transmute(
        household_group,
        race,
        rate_scenario,
        measure = spec$measure[[1]],
        estimate = .data[[spec$estimate_col[[1]]]],
        se = .data[[spec$se_col[[1]]]]
      )
  })) %>%
    filter(!is.na(estimate)) %>%
    add_plot_ci(estimate_col = "estimate", se_col = "se", floor_zero = TRUE) %>%
    mutate(measure = factor(measure, levels = measure_specs$measure)) %>%
    add_within_household_order(group_col = "race", ranking_col = "estimate")

  p <- ggplot(appendix_df, aes(x = estimate, y = order_key, fill = rate_scenario)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.62) +
    geom_errorbar(
      aes(xmin = lower_95, xmax = upper_95),
      position = position_dodge(width = 0.75),
      width = 0.18,
      linewidth = 0.3,
      alpha = 0.7
    ) +
    facet_grid(measure ~ household_group, scales = "free_x") +
    scale_fill_manual(values = fill_values, labels = c("2.75%", "6.8%"), name = "Interest rate") +
    scale_x_continuous(labels = label_dollar(), expand = expansion(mult = c(0, 0.01))) +
    scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
    labs(
      title = wrap_plot_title("Appendix Figure: The Deduction Pipeline Narrows from Interest Paid to Tax Savings"),
      subtitle = wrap_plot_subtitle("Estimated annual interest paid, the deduction cap, the MAGI-phased allowable deduction, and tax savings by race and household type."),
      x = "Dollars",
      y = NULL,
      caption = build_caption_text(source_id, outmoded_caption_body)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, size = 8.4, lineheight = 1.05),
      plot.margin = margin(10, 12, 24, 10)
    )

  save_plot_dual(output_file, p, width = 13, height = 12, dpi = 300)
}

make_comparison_appendix_plot <- function(df, output_file, source_id) {
  appendix_df <- df %>%
    filter(household_group == "Married household") %>%
    transmute(
      black_nonblack,
      rate_scenario,
      baseline = mean_allowable_deduction_baseline,
      baseline_se = `SE: mean allowable deduction baseline`,
      proposed = mean_allowable_deduction_proposed,
      proposed_se = `SE: mean allowable deduction proposed`,
      deduction_gain = mean_deduction_gain,
      deduction_gain_se = `SE: mean deduction gain`,
      tax_savings_gain = mean_tax_savings_gain,
      tax_savings_gain_se = `SE: mean tax savings gain`
    ) %>%
    filter(!is.na(baseline), !is.na(proposed))

  appendix_df <- appendix_df %>%
    bind_cols(compute_95_ci(.$baseline, .$baseline_se, floor_zero = TRUE) %>% rename(baseline_lower_95 = lower_95, baseline_upper_95 = upper_95)) %>%
    bind_cols(compute_95_ci(.$proposed, .$proposed_se, floor_zero = TRUE) %>% rename(proposed_lower_95 = lower_95, proposed_upper_95 = upper_95))

  axis_upper <- max(appendix_df$proposed_upper_95, na.rm = TRUE) * 1.15
  label_x <- axis_upper * 0.56

  p <- ggplot(appendix_df) +
    geom_segment(aes(x = baseline, xend = proposed, y = 0.82, yend = 0.82), linewidth = 2.2, color = "#bdbdbd") +
    geom_point(aes(x = baseline, y = 0.82), size = 3.2, color = "#2C7FB8") +
    geom_errorbar(aes(xmin = baseline_lower_95, xmax = baseline_upper_95, y = 0.82), width = 0.08, linewidth = 0.35, color = "#2C7FB8") +
    geom_point(aes(x = proposed, y = 0.82), size = 3.2, color = "#D95F0E") +
    geom_errorbar(aes(xmin = proposed_lower_95, xmax = proposed_upper_95, y = 0.82), width = 0.08, linewidth = 0.35, color = "#D95F0E") +
    geom_text(aes(x = baseline, y = 0.96, label = paste0("Baseline: ", dollar(baseline))), color = "#2C7FB8", size = 3.2, hjust = 0) +
    geom_text(aes(x = proposed, y = 0.68, label = paste0("Proposed: ", dollar(proposed))), color = "#D95F0E", size = 3.2, hjust = 0) +
    annotate("segment", x = 0, xend = axis_upper, y = 0.46, yend = 0.46, linewidth = 0.45, color = "#636363") +
    geom_text(aes(x = label_x, y = 0.56, label = paste0("Deduction gain: ", dollar(deduction_gain), " (95% CI ", dollar(pmax(deduction_gain - 1.96 * deduction_gain_se, 0)), " to ", dollar(deduction_gain + 1.96 * deduction_gain_se), ")")), size = 3.2, fontface = "bold") +
    geom_text(aes(x = label_x, y = 0.30, label = paste0("Tax savings gain: ", dollar(tax_savings_gain), " (95% CI ", dollar(pmax(tax_savings_gain - 1.96 * tax_savings_gain_se, 0)), " to ", dollar(tax_savings_gain + 1.96 * tax_savings_gain_se), ")")), size = 3.0) +
    facet_grid(black_nonblack ~ rate_scenario) +
    scale_x_continuous(labels = label_dollar(), limits = c(0, axis_upper), expand = expansion(mult = c(0, 0.01))) +
    scale_y_continuous(limits = c(0.18, 1.05), breaks = NULL) +
    labs(
      title = wrap_plot_title("Appendix Figure: The Larger Married Cap Raises Deductions More Than Tax Savings"),
      subtitle = wrap_plot_subtitle("Baseline and proposed allowable deductions for married Black and Non-Black households, with 95% confidence intervals."),
      x = "Dollars",
      y = NULL,
      caption = build_caption_text(source_id, comparison_caption_body)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, size = 8.4, lineheight = 1.05),
      plot.margin = margin(10, 12, 24, 10)
    )

  save_plot_dual(output_file, p, width = 12.5, height = 7.5, dpi = 300)
}

make_time_series_plot <- function(df, group_col, estimate_col, lower_col, upper_col, title_text, subtitle_text, y_label, output_file, source_id, percent_axis = FALSE) {
  plot_df <- df %>%
    transmute(
      year,
      household_group,
      group_value = .data[[group_col]],
      estimate = .data[[estimate_col]],
      lower_95 = .data[[lower_col]],
      upper_95 = .data[[upper_col]]
    ) %>%
    filter(!is.na(group_value), !is.na(estimate)) %>%
    mutate(
      household_group = factor(household_group, levels = household_levels),
      group_value = factor(group_value, levels = unique(group_value))
    )

  p <- ggplot(plot_df, aes(x = year, y = estimate, color = group_value, fill = group_value, group = group_value)) +
    geom_ribbon(aes(ymin = lower_95, ymax = upper_95), alpha = 0.16, linewidth = 0, show.legend = FALSE) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    facet_wrap(~ household_group, ncol = 1) +
    scale_color_manual(values = group_colors, name = NULL) +
    scale_fill_manual(values = group_colors, guide = "none") +
    scale_x_continuous(breaks = sort(unique(plot_df$year))) +
    labs(
      title = wrap_plot_title(title_text),
      subtitle = wrap_plot_subtitle(subtitle_text),
      x = NULL,
      y = y_label,
      caption = build_caption_text(
        source_id,
        paste(
          "Years shown:", paste(sort(unique(plot_df$year)), collapse = ", "), ".",
          if (estimate_col == "percent_with_student_debt") {
            paste(
              "Denominator: all eligible households after the project's income screens;",
              "numerator: households with student debt greater than zero."
            )
          } else {
            paste(
              "Sample: eligible households with positive student debt only, after the project's income screens.",
              "Eligible households are above the IDR-style $0-payment cutoff by household size",
              "and below the deduction phaseout ceiling for household type."
            )
          },
          "Estimates are weighted. Shaded bands show 95% confidence intervals."
        )
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, size = 8.6, lineheight = 1.05),
      plot.margin = margin(10, 12, 22, 10)
    )

  if (percent_axis) {
    p <- p + scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, max(plot_df$upper_95, na.rm = TRUE) * 1.05))
  } else {
    p <- p + scale_y_continuous(labels = label_dollar(), limits = c(0, max(plot_df$upper_95, na.rm = TRUE) * 1.05))
  }

  save_plot_dual(output_file, p, width = 10.5, height = 8, dpi = 300)
}

make_ratio_plot <- function(df, output_file, source_id) {
  plot_df <- df %>%
    filter(black_nonblack %in% c("Black", "Non-Black")) %>%
    transmute(
      rate_scenario,
      black_nonblack,
      estimate = tax_relief_to_population_ratio,
      lower_95 = lower_95_tax_relief_to_population_ratio,
      upper_95 = upper_95_tax_relief_to_population_ratio
    )

  axis_upper <- max(plot_df$upper_95, na.rm = TRUE) * 1.08

  p <- ggplot(plot_df, aes(x = black_nonblack, y = estimate, fill = black_nonblack)) +
    geom_col(width = 0.62) +
    geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.16, linewidth = 0.35) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "#6c757d") +
    facet_wrap(~ rate_scenario, ncol = 1) +
    scale_fill_manual(values = c("Black" = "#1b4332", "Non-Black" = "#c1121f"), guide = "none") +
    scale_y_continuous(limits = c(0, axis_upper), labels = label_number(accuracy = 0.01)) +
    labs(
      title = wrap_plot_title("Figure 10: Black Households Receive a Larger Share of Binding-Constraint Relief Than Their Population Share"),
      subtitle = wrap_plot_subtitle("Ratio of each group's modeled tax-relief share to its U.S. Census population share among married households at the $2,500 binding constraint."),
      x = NULL,
      y = "Tax-relief share / population share",
      caption = ratio_caption_body(source_id)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, size = 8.5, lineheight = 1.05),
      plot.margin = margin(10, 12, 24, 10)
    )

  save_plot_dual(output_file, p, width = 10, height = 7.6, dpi = 300)
}

main_graph_specs <- tribble(
  ~source_id, ~comparison_file, ~comparison_by_race_file, ~binding_file, ~main_dir, ~outmoded_dir,
  "SCF",
  file.path(paths$scf_csv_dir, "scf_student_loan_interest_deduction_married_cap_comparison_black_nonblack.csv"),
  file.path(paths$scf_csv_dir, "scf_student_loan_interest_deduction_married_cap_comparison_by_race.csv"),
  file.path(paths$scf_csv_dir, "scf_binding_constraint_married_cap_black_nonblack.csv"),
  paths$scf_graph_dir,
  paths$outmoded_scf_graph_dir,
  "SIPP",
  file.path(paths$sipp_csv_dir, "sipp_student_loan_interest_deduction_married_cap_comparison_black_nonblack.csv"),
  file.path(paths$sipp_csv_dir, "sipp_student_loan_interest_deduction_married_cap_comparison_by_race.csv"),
  file.path(paths$sipp_csv_dir, "sipp_binding_constraint_married_cap_black_nonblack.csv"),
  paths$sipp_graph_dir,
  paths$outmoded_sipp_graph_dir
)

main_graph_keep_files <- c(
  "Figure 1 - Percent of Households with Student Debt by Black and Non-Black Group.pdf",
  "Figure 2 - Mean Student Debt Among Debt Holders by Black and Non-Black Group.pdf",
  "Figure 3 - $2500 Cap Binds Differently for Black and Non-Black Borrowers.pdf",
  "Figure 4 - A Higher Married Cap Raises Deductions and Tax Savings for Married Black Households.pdf"
)

outmoded_combined_specs <- tribble(
  ~source_id, ~combined_file, ~outmoded_dir,
  "SCF", file.path(paths$outmoded_scf_csv_dir, "scf_student_loan_interest_deduction_combined.csv"), paths$outmoded_scf_graph_dir,
  "SIPP", file.path(paths$outmoded_sipp_csv_dir, "sipp_student_loan_interest_deduction_combined.csv"), paths$outmoded_sipp_graph_dir
)

for (i in seq_len(nrow(outmoded_combined_specs))) {
  spec <- outmoded_combined_specs[i, ]
  if (!file.exists(spec$combined_file[[1]])) next
  ensure_dir(spec$outmoded_dir[[1]])

  combined_df <- read_csv(spec$combined_file[[1]], show_col_types = FALSE) %>%
    mutate(
      rate_scenario = factor(rate_scenario, levels = rate_levels),
      household_group = factor(household_group, levels = household_levels)
    )

  make_horizontal_bar_plot(
    combined_df,
    group_col = "race",
    estimate_col = "Estimated annual interest paid",
    se_col = "SE: estimated annual interest paid",
    title_text = "Outmoded Figure: Student Loan Interest Burdens Vary Across Groups",
    subtitle_text = "Estimated annual student loan interest paid by race and household type under 2.75% and 6.8% interest assumptions.",
    x_label = "Dollars",
    output_file = file.path(spec$outmoded_dir[[1]], "Figure 1 - Estimated Annual Student Loan Interest Paid by Race and Household Type.pdf"),
    source_id = spec$source_id[[1]]
  )

  make_horizontal_bar_plot(
    combined_df,
    group_col = "race",
    estimate_col = "Estimated tax savings",
    se_col = "SE: estimated tax savings",
    title_text = "Outmoded Figure: Tax Benefits from the Interest Deduction Are Uneven",
    subtitle_text = "Estimated student loan interest deduction tax savings by race and household type under 2.75% and 6.8% interest assumptions.",
    x_label = "Dollars",
    output_file = file.path(spec$outmoded_dir[[1]], "Figure 2 - Estimated Student Loan Interest Deduction Tax Savings by Race and Household Type.pdf"),
    source_id = spec$source_id[[1]]
  )

  make_appendix_components_plot(
    combined_df,
    output_file = file.path(spec$outmoded_dir[[1]], "Figure 3 - Appendix Student Loan Interest Deduction Components by Race and Household Type.pdf"),
    source_id = spec$source_id[[1]]
  )
}

for (i in seq_len(nrow(main_graph_specs))) {
  spec <- main_graph_specs[i, ]
  if (!file.exists(spec$comparison_file[[1]]) || !file.exists(spec$comparison_by_race_file[[1]]) || !file.exists(spec$binding_file[[1]])) next

  comparison_df <- read_csv(spec$comparison_file[[1]], show_col_types = FALSE) %>%
    mutate(
      rate_scenario = factor(rate_scenario, levels = rate_levels),
      household_group = factor(household_group, levels = household_levels),
      black_nonblack = factor(black_nonblack, levels = c("Black", "Non-Black"))
    )

  comparison_race_df <- read_csv(spec$comparison_by_race_file[[1]], show_col_types = FALSE) %>%
    mutate(
      rate_scenario = factor(rate_scenario, levels = rate_levels),
      household_group = factor(household_group, levels = household_levels)
    )

  binding_df <- read_csv(spec$binding_file[[1]], show_col_types = FALSE) %>%
    mutate(
      rate_scenario = factor(rate_scenario, levels = rate_levels),
      household_group = factor(household_group, levels = household_levels),
      black_nonblack = factor(black_nonblack, levels = c("Black", "Non-Black"))
    )

  ensure_dir(spec$main_dir[[1]])
  ensure_dir(spec$outmoded_dir[[1]])
  move_existing_outputs(spec$main_dir[[1]], spec$outmoded_dir[[1]])

  make_gain_two_panel_plot(
    comparison_df,
    output_file = file.path(spec$main_dir[[1]], "Figure 4 - A Higher Married Cap Raises Deductions and Tax Savings for Married Black Households.pdf"),
    source_id = spec$source_id[[1]]
  )

  make_horizontal_bar_plot(
    comparison_df,
    group_col = "black_nonblack",
    estimate_col = "mean_deduction_gain",
    se_col = "SE: mean deduction gain",
    title_text = "Outmoded Figure: A Higher Married Cap Expands Deductions Most for Black Households",
    subtitle_text = "Estimated allowable-deduction gain from raising the married-household deduction cap to $5,000 for Black and Non-Black households.",
    x_label = "Dollars",
    output_file = file.path(spec$outmoded_dir[[1]], "Figure 4 - A Higher Married Cap Expands Deductions Most for Some Groups.pdf"),
    source_id = spec$source_id[[1]],
    household_filter = "Married household"
  )

  make_horizontal_bar_plot(
    comparison_df,
    group_col = "black_nonblack",
    estimate_col = "mean_tax_savings_gain",
    se_col = "SE: mean tax savings gain",
    title_text = "Outmoded Figure: A Higher Married Cap Disproportionately Raises Tax Savings for Married Black Households",
    subtitle_text = "Estimated tax savings gain from raising the married-household deduction cap to $5,000 for Black and Non-Black households.",
    x_label = "Dollars",
    output_file = file.path(spec$outmoded_dir[[1]], "Figure 5 - A Higher Married Cap Disproportionately Raises Tax Savings for Married Black Households.pdf"),
    source_id = spec$source_id[[1]],
    household_filter = "Married household"
  )

  make_comparison_appendix_plot(
    comparison_df,
    output_file = file.path(spec$outmoded_dir[[1]], "Figure 6 - Appendix Married-Cap Comparison for Black and Non-Black Households.pdf"),
    source_id = spec$source_id[[1]]
  )

  make_horizontal_bar_plot(
    comparison_race_df,
    group_col = "race",
    estimate_col = "pct_borrowers_at_2500_limit_baseline",
    se_col = "SE: % borrowers at $2500 limit baseline",
    title_text = "Outmoded Figure: Many Married Borrowers Still Run Into the $2,500 Cap",
    subtitle_text = "Percent of married borrowers at the current $2,500 allowable-deduction limit by modified race group.",
    x_label = "Percent of married borrowers",
    output_file = file.path(spec$outmoded_dir[[1]], "Figure 7 - Share of Married Borrowers at the $2500 Deduction Limit by Race.pdf"),
    source_id = spec$source_id[[1]],
    percent_axis = TRUE,
    household_filter = "Married household"
  )

  make_horizontal_bar_plot(
    comparison_df,
    group_col = "black_nonblack",
    estimate_col = "pct_borrowers_at_2500_limit_baseline",
    se_col = "SE: % borrowers at $2500 limit baseline",
    title_text = "Figure 3: $2,500 Cap Binds Differently for Black and Non-Black Borrowers",
    subtitle_text = "Percent of married and unmarried borrowers at the current $2,500 allowable-deduction limit for Black and Non-Black households.",
    x_label = "Percent of borrowers",
    output_file = file.path(spec$main_dir[[1]], "Figure 3 - $2500 Cap Binds Differently for Black and Non-Black Borrowers.pdf"),
    source_id = spec$source_id[[1]],
    percent_axis = TRUE
  )

  make_horizontal_bar_plot(
    comparison_race_df,
    group_col = "race",
    estimate_col = "pct_borrowers_at_2500_limit_baseline",
    se_col = "SE: % borrowers at $2500 limit baseline",
    title_text = "Outmoded Figure: Many Unmarried Borrowers Also Hit the $2,500 Cap",
    subtitle_text = "Percent of unmarried borrowers at the current $2,500 allowable-deduction limit by modified race group.",
    x_label = "Percent of unmarried borrowers",
    output_file = file.path(spec$outmoded_dir[[1]], "Figure 9 - Share of Unmarried Borrowers at the $2500 Deduction Limit by Race.pdf"),
    source_id = spec$source_id[[1]],
    percent_axis = TRUE,
    household_filter = "Unmarried household"
  )

  make_horizontal_bar_plot(
    binding_df,
    group_col = "black_nonblack",
    estimate_col = "mean_deduction_gain",
    se_col = "SE: mean deduction gain",
    title_text = "Figure 8: Married Borrowers Already at the Cap Would See the Largest Deduction Gains",
    subtitle_text = "Estimated allowable-deduction gain from raising the married cap to $5,000 among married households already at the current $2,500 binding constraint.",
    x_label = "Dollars",
    output_file = file.path(spec$outmoded_dir[[1]], "Figure 8 - Binding Constraint Married Borrowers Gain More Deduction Dollars.pdf"),
    source_id = spec$source_id[[1]],
    caption_body = binding_caption_body
  )

  make_horizontal_bar_plot(
    binding_df,
    group_col = "black_nonblack",
    estimate_col = "mean_tax_savings_gain",
    se_col = "SE: mean tax savings gain",
    title_text = "Figure 9: Married Borrowers Already at the Cap Would See the Largest Tax Savings",
    subtitle_text = "Estimated tax-savings gain from raising the married cap to $5,000 among married households already at the current $2,500 binding constraint.",
    x_label = "Dollars",
    output_file = file.path(spec$outmoded_dir[[1]], "Figure 9 - Binding Constraint Married Borrowers Gain More Tax Savings.pdf"),
    source_id = spec$source_id[[1]],
    caption_body = binding_caption_body
  )

  make_ratio_plot(
    binding_df,
    output_file = file.path(spec$outmoded_dir[[1]], "Figure 10 - Black Share of Binding-Constraint Tax Relief Relative to Population Share.pdf"),
    source_id = spec$source_id[[1]]
  )
}

time_series_specs <- tribble(
  ~source_id, ~grouping, ~input_file, ~main_output_dir, ~outmoded_output_dir,
  "SCF", "black_nonblack", file.path(paths$scf_csv_dir, "scf_student_debt_time_series_black_nonblack.csv"), paths$scf_graph_dir, paths$outmoded_scf_graph_dir,
  "SCF", "race", file.path(paths$outmoded_scf_csv_dir, "scf_student_debt_time_series_by_race.csv"), paths$scf_graph_dir, paths$outmoded_scf_graph_dir,
  "SIPP", "black_nonblack", file.path(paths$sipp_csv_dir, "sipp_student_debt_time_series_black_nonblack.csv"), paths$sipp_graph_dir, paths$outmoded_sipp_graph_dir,
  "SIPP", "race", file.path(paths$outmoded_sipp_csv_dir, "sipp_student_debt_time_series_by_race.csv"), paths$sipp_graph_dir, paths$outmoded_sipp_graph_dir
)

for (i in seq_len(nrow(time_series_specs))) {
  spec <- time_series_specs[i, ]
  if (!file.exists(spec$input_file[[1]])) next
  df <- read_csv(spec$input_file[[1]], show_col_types = FALSE)
  group_col <- if (spec$grouping[[1]] == "black_nonblack") "black_nonblack" else "race"
  out_dir <- if (spec$grouping[[1]] == "black_nonblack") spec$main_output_dir[[1]] else spec$outmoded_output_dir[[1]]
  ensure_dir(out_dir)
  ensure_dir(spec$outmoded_output_dir[[1]])

  make_time_series_plot(
    df = df,
    group_col = group_col,
    estimate_col = "percent_with_student_debt",
    lower_col = "lower_95_percent_with_student_debt",
    upper_col = "upper_95_percent_with_student_debt",
    title_text = if (spec$grouping[[1]] == "black_nonblack") {
      "Figure 1: Student Debt Touches More Black Households Than Non-Black Households Over Time"
    } else {
      "Outmoded Figure: Student Debt Reaches Households Unevenly Across Race Groups Over Time"
    },
    subtitle_text = if (spec$grouping[[1]] == "black_nonblack") {
      "Percent of eligible households with positive student debt over time for Black and Non-Black households."
    } else {
      "Percent of eligible households with positive student debt over time by collapsed race group."
    },
    y_label = "Percent of households",
    output_file = file.path(out_dir, if (spec$grouping[[1]] == "black_nonblack") {
      "Figure 1 - Percent of Households with Student Debt by Black and Non-Black Group.pdf"
    } else {
      "Time Series - Percent of Households with Student Debt by Race.pdf"
    }),
    source_id = spec$source_id[[1]],
    percent_axis = TRUE
  )

  make_time_series_plot(
    df = df,
    group_col = group_col,
    estimate_col = "median_student_debt_positive",
    lower_col = "lower_95_median_student_debt_positive",
    upper_col = "upper_95_median_student_debt_positive",
    title_text = if (spec$grouping[[1]] == "black_nonblack") {
      "Outmoded Figure: Median Student Debt Remains Higher for Black Borrowers Over Time"
    } else {
      "Outmoded Figure: Median Student Debt Moves Differently Across Race Groups"
    },
    subtitle_text = if (spec$grouping[[1]] == "black_nonblack") {
      "Median student debt among households with positive student debt over time for Black and Non-Black households."
    } else {
      "Median student debt among households with positive student debt over time by collapsed race group."
    },
    y_label = "Dollars",
    output_file = file.path(if (spec$grouping[[1]] == "black_nonblack") spec$outmoded_output_dir[[1]] else out_dir, if (spec$grouping[[1]] == "black_nonblack") {
      "Figure 2 - Median Student Debt Among Debt Holders by Black and Non-Black Group.pdf"
    } else {
      "Time Series - Median Student Debt Among Debt Holders by Race.pdf"
    }),
    source_id = spec$source_id[[1]],
    percent_axis = FALSE
  )

  make_time_series_plot(
    df = df,
    group_col = group_col,
    estimate_col = "mean_student_debt_positive",
    lower_col = "lower_95_mean_student_debt_positive",
    upper_col = "upper_95_mean_student_debt_positive",
    title_text = if (spec$grouping[[1]] == "black_nonblack") {
      "Figure 2: Average Student Debt Stays Uneven Between Black and Non-Black Households"
    } else {
      "Outmoded Figure: Average Student Debt Still Varies Across Race Groups"
    },
    subtitle_text = if (spec$grouping[[1]] == "black_nonblack") {
      "Mean student debt among households with positive student debt over time for Black and Non-Black households."
    } else {
      "Mean student debt among households with positive student debt over time by collapsed race group."
    },
    y_label = "Dollars",
    output_file = file.path(out_dir, if (spec$grouping[[1]] == "black_nonblack") {
      "Figure 2 - Mean Student Debt Among Debt Holders by Black and Non-Black Group.pdf"
    } else {
      "Time Series - Mean Student Debt Among Debt Holders by Race.pdf"
    }),
    source_id = spec$source_id[[1]],
    percent_axis = FALSE
  )
}
