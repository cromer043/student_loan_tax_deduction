library(dplyr)
library(ggplot2)
library(patchwork)
library(readxl)
library(readr)
library(scales)
library(sf)
library(sp)
library(stringr)
library(tibble)
library(viridis)

source(file.path("1. Code", "student_loan_interest_workflow_utils.R"))

project_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
paths <- get_workflow_paths(project_dir)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  token <- args[grepl(paste0("^--", flag, "="), args)]
  if (length(token) == 0) return(default)
  sub(paste0("^--", flag, "="), "", token[[1]])
}

irs_dir <- file.path(paths$data_dir, "IRS")
census_dir <- file.path(paths$data_dir, "Census")
ensure_dir(irs_dir)
ensure_dir(census_dir)

irs_xlsx <- arg_value("irs-file", file.path(irs_dir, "19in55cm.xlsx"))
census_zip <- file.path(census_dir, "cb_2024_us_state_20m.zip")
tax_year_label <- arg_value("tax-year", "2019")
outmoded_run <- identical(tolower(arg_value("outmoded", "false")), "true")

irs_csv_dir <- if (outmoded_run) paths$outmoded_irs_csv_dir else paths$irs_csv_dir
irs_graph_dir <- if (outmoded_run) paths$outmoded_irs_graph_dir else paths$irs_graph_dir

if (!file.exists(irs_xlsx)) {
  stop("Missing IRS workbook at ", irs_xlsx, ". Download it first.")
}

if (!file.exists(census_zip)) {
  stop("Missing Census state shapefile zip at ", census_zip, ". Download it first.")
}

map_crs <- 5070

save_plot_both <- function(plot_obj, file_stem, width = 12, height = 8, dpi = 320) {
  ggsave(
    filename = paste0(file_stem, ".pdf"),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    units = "in"
  )
  ggsave(
    filename = paste0(file_stem, ".png"),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    units = "in"
  )
}

trim_label <- function(x) {
  stringr::str_squish(as.character(x))
}

workbook <- suppressMessages(read_excel(irs_xlsx, sheet = 1, col_names = FALSE))
label_col <- trim_label(workbook[[1]])
state_row <- trim_label(unlist(workbook[3, ]))

returns_row <- which(label_col == "Number of returns [1]")
agi_row <- which(label_col == "Adjusted gross income (AGI) [6]")
student_deduction_number_row <- grep("^Student loan interest deduction:\\s+Number$", label_col)
student_deduction_amount_row <- student_deduction_number_row + 1L

if (length(returns_row) != 1 || length(agi_row) != 1 || length(student_deduction_number_row) != 1) {
  stop("Could not uniquely identify the needed IRS rows in the workbook.")
}

state_cols <- which(!is.na(state_row) & state_row != "")
state_headers <- state_row[state_cols]

state_lookup <- tibble(
  column_index = state_cols,
  state_name_irs = state_headers
) %>%
  filter(!state_name_irs %in% c("UNITED STATES", "PUERTO RICO", "OTHER AREAS [16]"))

extract_numeric <- function(row_index, col_indices) {
  vals <- unlist(workbook[row_index, col_indices], use.names = FALSE)
  suppressWarnings(as.numeric(vals))
}

irs_state_data <- state_lookup %>%
  mutate(
    number_of_returns = extract_numeric(returns_row, column_index),
    adjusted_gross_income_thousands = extract_numeric(agi_row, column_index),
    student_loan_deduction_returns = extract_numeric(student_deduction_number_row, column_index),
    student_loan_deduction_amount_thousands = extract_numeric(student_deduction_amount_row, column_index)
  ) %>%
  mutate(
    share_returns_with_student_loan_deduction = student_loan_deduction_returns / number_of_returns,
    student_loan_deduction_amount_share_of_agi = student_loan_deduction_amount_thousands / adjusted_gross_income_thousands,
    state_name_join = toupper(state_name_irs)
  ) %>%
  arrange(state_name_irs)

write_csv(
  irs_state_data %>%
    select(
      state_name_irs,
      number_of_returns,
      adjusted_gross_income_thousands,
      student_loan_deduction_returns,
      share_returns_with_student_loan_deduction,
      student_loan_deduction_amount_thousands,
      student_loan_deduction_amount_share_of_agi
    ),
  file.path(irs_csv_dir, "irs_student_loan_interest_deduction_state_overall.csv")
)

income_group_labels <- c(
  "Under $1",
  "$1 under $10,000",
  "$10,000 under $25,000",
  "$25,000 under $50,000",
  "$50,000 under $75,000",
  "$75,000 under $100,000",
  "$100,000 under $200,000",
  "$200,000 under $500,000",
  "$500,000 under $1,000,000",
  "$1,000,000 or more"
)

us_income_group_cols <- 3:12

us_income_group_data <- tibble(
  income_group = factor(income_group_labels, levels = income_group_labels),
  student_loan_deduction_returns = extract_numeric(student_deduction_number_row, us_income_group_cols),
  student_loan_deduction_amount_thousands = extract_numeric(student_deduction_amount_row, us_income_group_cols),
  adjusted_gross_income_thousands = extract_numeric(agi_row, us_income_group_cols)
) %>%
  mutate(
    student_loan_deduction_amount_share_of_agi = student_loan_deduction_amount_thousands / adjusted_gross_income_thousands,
    student_loan_deduction_amount_billions = student_loan_deduction_amount_thousands / 1e6
  )

write_csv(
  us_income_group_data,
  file.path(irs_csv_dir, "irs_student_loan_interest_deduction_us_income_groups.csv")
)

state_shapes <- read_sf(sprintf("/vsizip/%s", normalizePath(census_zip, winslash = "/", mustWork = TRUE))) %>%
  filter(STATEFP %in% c(sprintf("%02d", 1:56), "11")) %>%
  filter(!STATEFP %in% c("60", "66", "69", "72", "78")) %>%
  mutate(state_name_join = toupper(NAME)) %>%
  st_transform(crs = map_crs)

map_states_full <- state_shapes %>%
  left_join(irs_state_data, by = "state_name_join")

conus_states <- map_states_full %>% filter(!STATEFP %in% c("02", "15"))
alaska_state <- map_states_full %>% filter(STATEFP == "02")
hawaii_state <- map_states_full %>% filter(STATEFP == "15")
dc_point <- st_centroid(conus_states %>% filter(state_name_join == "DISTRICT OF COLUMBIA"))
dc_coords <- sf::st_coordinates(dc_point)
dc_callout_df <- tibble::tibble(
  x = dc_coords[1, "X"],
  y = dc_coords[1, "Y"],
  xend = dc_coords[1, "X"] + 240000,
  yend = dc_coords[1, "Y"] + 90000
)
dc_label_df <- tibble::tibble(
  x = dc_callout_df$xend,
  y = dc_callout_df$yend,
  label = "DC"
)

caption_base <- stringr::str_wrap(paste(
  sprintf("Author's analysis of IRS Statistics of Income, Table 2, tax year %s state-level individual income tax data.", tax_year_label),
  "Overall state columns only are used here, not the income-bin columns.",
  "The source deduction amount row is reported in thousands of dollars."
), width = 140)

theme_map <- theme_minimal(base_size = 13) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, lineheight = 1.1),
    plot.margin = margin(15, 20, 30, 20),
    plot.caption.position = "plot"
  )

build_map <- function(fill_col, scale_obj, title_text, subtitle_text) {
  conus_plot <- ggplot(conus_states) +
    geom_sf(aes(fill = .data[[fill_col]]), color = "white", linewidth = 0.2) +
    geom_segment(
      data = dc_callout_df,
      aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE,
      color = "#6b7280",
      linewidth = 0.5
    ) +
    geom_sf(
      data = dc_point,
      aes(fill = .data[[fill_col]]),
      shape = 21,
      color = "white",
      size = 3.2,
      stroke = 0.85
    ) +
    geom_point(
      data = dc_label_df,
      aes(x = x, y = y),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.4,
      stroke = 0.5,
      fill = "white",
      color = "#6b7280"
    ) +
    geom_text(
      data = dc_label_df,
      aes(x = x + 38000, y = y + 22000, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 0,
      size = 3.2,
      color = "#374151",
      fontface = "bold"
    ) +
    scale_obj +
    coord_sf(datum = NA) +
    theme_map

  inset_theme <- theme_void(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 11, hjust = 0),
      plot.margin = margin(5, 5, 5, 5)
    )

  alaska_plot <- ggplot(alaska_state) +
    geom_sf(aes(fill = .data[[fill_col]]), color = "white", linewidth = 0.15) +
    scale_obj +
    guides(fill = "none") +
    labs(title = "Alaska") +
    coord_sf(datum = NA) +
    inset_theme

  hawaii_plot <- ggplot(hawaii_state) +
    geom_sf(aes(fill = .data[[fill_col]]), color = "white", linewidth = 0.15) +
    scale_obj +
    guides(fill = "none") +
    labs(title = "Hawaii") +
    coord_sf(datum = NA) +
    inset_theme

  combined <- ((alaska_plot / hawaii_plot) | conus_plot) +
    plot_layout(widths = c(1.15, 5), guides = "collect") +
    plot_annotation(
      title = title_text,
      subtitle = subtitle_text,
      caption = caption_base,
      theme = theme(
        plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 9, lineheight = 1.1),
        plot.margin = margin(15, 20, 30, 20)
      )
    ) &
    theme(legend.position = "bottom")

  combined
}

share_map <- build_map(
  fill_col = "share_returns_with_student_loan_deduction",
  scale_obj = scale_fill_gradient(
    low = "#9CC3D5",
    high = "#00539C",
    labels = percent_format(accuracy = 0.1),
    name = "Share of returns"
  ),
  title_text = "Figure 6 Student Loan Interest Deduction Use Varies Sharply Across States",
  subtitle_text = "Share of returns claiming the student loan interest deduction"
)

amount_map <- build_map(
  fill_col = "student_loan_deduction_amount_share_of_agi",
  scale_obj = scale_fill_gradient(
    low = "#9CC3D5",
    high = "#00539C",
    labels = percent_format(accuracy = 0.01),
    breaks = pretty_breaks(n = 4),
    name = "Deduction amount\nas a share of AGI"
  ),
  title_text = "Student Loan Deduction Dollars Matter More in Some State Tax Bases",
  subtitle_text = "Student loan interest deduction amount as a share of adjusted gross income"
)

us_income_group_plot_data <- bind_rows(
  us_income_group_data %>%
    transmute(
      income_group,
      series = "Returns claiming deduction",
      value = student_loan_deduction_returns / 1e6,
      label = sprintf('%.1fM', student_loan_deduction_returns / 1e6)
    ),
  us_income_group_data %>%
    transmute(
      income_group,
      series = "Deduction amount claimed",
      value = student_loan_deduction_amount_billions,
      label = sprintf('$%.1fB', student_loan_deduction_amount_billions)
    )
) %>%
  mutate(
    series = factor(
      series,
      levels = c("Returns claiming deduction", "Deduction amount claimed")
    )
  )

us_income_group_chart <- ggplot(
  us_income_group_plot_data,
  aes(x = income_group, y = value, fill = series)
) +
  geom_col(
    position = position_dodge(width = 0.78),
    width = 0.68
  ) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.78),
    vjust = -0.25,
    size = 3.2
  ) +
  scale_fill_manual(
    values = c(
      "Returns claiming deduction" = "#2c7fb8",
      "Deduction amount claimed" = "#d95f0e"
    ),
    breaks = c("Returns claiming deduction", "Deduction amount claimed"),
    name = NULL
  ) +
  scale_y_continuous(
    name = "Millions of claims / Billions of deduction dollars",
    labels = label_number(accuracy = 0.1)
  ) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 13)) +
  labs(
    title = "Figure 5 Student Loan Interest Deduction Claims Are Concentrated in Middle-Income Bins",
    subtitle = "U.S. totals by adjusted gross income group",
    x = "Adjusted gross income group",
    caption = stringr::str_wrap(
      sprintf("Author's analysis of IRS Statistics of Income, Table 2, tax year %s national totals. Blue bars show the number of returns claiming the student loan interest deduction, in millions. Orange bars show the total deduction amount claimed, in billions of dollars. Amounts are sourced from the IRS row reported in thousands of dollars and rescaled here for readability.", tax_year_label),
      width = 140
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, lineheight = 1.1),
    plot.margin = margin(15, 20, 30, 20),
    legend.position = "top"
  )

save_plot_both(
  share_map,
  file.path(irs_graph_dir, "Figure 6 - Student Loan Interest Deduction Use Varies Sharply Across States")
)

save_plot_both(
  amount_map,
  file.path(irs_graph_dir, "irs_state_student_loan_interest_deduction_amount")
)

save_plot_both(
  us_income_group_chart,
  file.path(irs_graph_dir, "Figure 5 - Student Loan Interest Deduction Claims Are Concentrated in Middle-Income Bins")
)

cat("Created IRS overall-state deduction CSV and maps in:\n")
cat(" -", irs_csv_dir, "\n")
cat(" -", irs_graph_dir, "\n")
