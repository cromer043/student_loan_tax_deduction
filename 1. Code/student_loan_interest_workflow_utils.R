ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

get_workflow_paths <- function(project_dir) {
  outmoded_root <- file.path(project_dir, "4. outmoded")

  paths <- list(
    data_dir = file.path(project_dir, "0. Data"),
    code_dir = file.path(project_dir, "1. Code"),
    output_csv_dir = file.path(project_dir, "2. Output CSV"),
    output_graph_dir = file.path(project_dir, "3. Output Graphs"),
    outmoded_root = outmoded_root,
    outmoded_csv_dir = file.path(outmoded_root, "2. Output CSV"),
    outmoded_graph_dir = file.path(outmoded_root, "3. Output Graphs"),
    sipp_csv_dir = file.path(project_dir, "2. Output CSV", "2. A. SIPP"),
    scf_csv_dir = file.path(project_dir, "2. Output CSV", "2. B. SCF"),
    irs_csv_dir = file.path(project_dir, "2. Output CSV", "2. C. IRS"),
    sipp_graph_dir = file.path(project_dir, "3. Output Graphs", "3. A. Sipp"),
    scf_graph_dir = file.path(project_dir, "3. Output Graphs", "3. B. SCF"),
    irs_graph_dir = file.path(project_dir, "3. Output Graphs", "3. C. IRS"),
    outmoded_sipp_csv_dir = file.path(outmoded_root, "2. Output CSV", "2. A. SIPP"),
    outmoded_scf_csv_dir = file.path(outmoded_root, "2. Output CSV", "2. B. SCF"),
    outmoded_irs_csv_dir = file.path(outmoded_root, "2. Output CSV", "2. C. IRS"),
    outmoded_sipp_graph_dir = file.path(outmoded_root, "3. Output Graphs", "3. A. Sipp"),
    outmoded_scf_graph_dir = file.path(outmoded_root, "3. Output Graphs", "3. B. SCF"),
    outmoded_irs_graph_dir = file.path(outmoded_root, "3. Output Graphs", "3. C. IRS"),
    sipp_harmonized_dir = file.path(project_dir, "0. Data", "SIPP", "harmonized"),
    scf_harmonized_dir = file.path(project_dir, "0. Data", "SCF", "harmonized"),
    sipp_raw_dir = file.path(project_dir, "0. Data", "SIPP", "raw"),
    scf_raw_dir = file.path(project_dir, "0. Data", "SCF", "raw")
  )

  invisible(lapply(paths, ensure_dir))
  paths
}

compute_95_ci <- function(estimate, se, floor_zero = FALSE) {
  lower <- estimate - 1.96 * se
  upper <- estimate + 1.96 * se
  if (floor_zero) lower <- pmax(lower, 0)
  tibble::tibble(lower_95 = lower, upper_95 = upper)
}

add_95_ci_columns <- function(df, estimate_col, se_col, prefix = NULL, floor_zero = FALSE) {
  ci <- compute_95_ci(df[[estimate_col]], df[[se_col]], floor_zero = floor_zero)
  if (is.null(prefix)) {
    df$lower_95 <- ci$lower_95
    df$upper_95 <- ci$upper_95
  } else {
    df[[paste0(prefix, "_lower_95")]] <- ci$lower_95
    df[[paste0(prefix, "_upper_95")]] <- ci$upper_95
  }
  df
}

parse_year_arg <- function(default_year) {
  args <- commandArgs(trailingOnly = TRUE)
  year_arg <- default_year

  if (length(args) > 0) {
    year_tokens <- args[grepl("^--year=", args)]
    if (length(year_tokens) > 0) {
      year_arg <- sub("^--year=", "", year_tokens[[1]])
    } else if (grepl("^[0-9]{4}$", args[[1]])) {
      year_arg <- args[[1]]
    }
  }

  as.integer(year_arg)
}

move_existing_outputs <- function(from_dir, to_dir, keep_files = character(0)) {
  ensure_dir(to_dir)
  existing <- list.files(from_dir, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  if (length(existing) == 0) return(invisible(NULL))

  for (path in existing) {
    base <- basename(path)
    if (base %in% keep_files) next
    target <- file.path(to_dir, base)
    if (file.exists(target)) file.remove(target)
    file.rename(path, target)
  }

  invisible(NULL)
}
