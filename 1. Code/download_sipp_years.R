#!/usr/bin/env Rscript

raw_script_path <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1])
script_path <- normalizePath(gsub("~\\+~", " ", raw_script_path), winslash = "/", mustWork = FALSE)
code_dir <- dirname(script_path)
project_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(code_dir, "student_loan_interest_workflow_utils.R"))
paths <- get_workflow_paths(project_dir)
options(timeout = max(600, getOption("timeout")))

required_pkgs <- c("xml2", "rvest")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

suppressPackageStartupMessages({
  library(xml2)
  library(rvest)
  library(dplyr)
  library(tibble)
})

years <- 2018:2024

discover_sipp_links <- function(year) {
  page_url <- sprintf("https://www.census.gov/programs-surveys/sipp/data/datasets/%s-data/%s.html", year, year)
  doc <- read_html(page_url)
  hrefs <- html_elements(doc, "a") %>%
    html_attr("href")
  texts <- html_elements(doc, "a") %>%
    html_text2()

  links <- tibble::tibble(text = texts, href = hrefs) %>%
    filter(!is.na(href), grepl("\\.zip($|\\?)|\\.pdf($|\\?)", href, ignore.case = TRUE)) %>%
    mutate(
      href = ifelse(grepl("^https?://", href), href, xml2::url_absolute(href, page_url))
    )

  list(
    primary_zip = links %>% filter(grepl("pipe-delimited text data", text, ignore.case = TRUE)) %>% slice(1) %>% pull(href),
    replicate_zip = links %>% filter(grepl("pipe-delimited replicate weights", text, ignore.case = TRUE)) %>% slice(1) %>% pull(href),
    dictionary_pdf = links %>% filter(grepl("data dictionary", text, ignore.case = TRUE)) %>% slice(1) %>% pull(href),
    replicate_dictionary_pdf = links %>% filter(grepl("replicate weight data dictionary", text, ignore.case = TRUE)) %>% slice(1) %>% pull(href)
  )
}

default_sipp_links <- function(year) {
  list(
    primary_zip = sprintf("https://www2.census.gov/programs-surveys/sipp/data/datasets/%s/pu%s_csv.zip", year, year),
    replicate_zip = sprintf("https://www2.census.gov/programs-surveys/sipp/data/datasets/%s/rw%s_csv.zip", year, year),
    dictionary_pdf = sprintf("https://www2.census.gov/programs-surveys/sipp/tech-documentation/data-dictionaries/%s/%s_SIPP_Data_Dictionary.pdf", year, year),
    replicate_dictionary_pdf = sprintf("https://www2.census.gov/programs-surveys/sipp/tech-documentation/data-dictionaries/%s/%s_rw%s_dictionary.txt.pdf", year, year, year)
  )
}

download_if_missing <- function(url, dest) {
  if (length(url) == 0 || is.na(url) || !nzchar(url)) return(invisible(FALSE))
  if (file.exists(dest)) return(invisible(TRUE))
  ok <- tryCatch({
    download.file(url, destfile = dest, mode = "wb", quiet = FALSE)
    TRUE
  }, error = function(e) {
    message("Skipping unavailable file: ", url)
    FALSE
  })
  invisible(ok)
}

for (year in years) {
  year_dir <- file.path(paths$sipp_raw_dir, year)
  ensure_dir(year_dir)
  links <- discover_sipp_links(year)
  defaults <- default_sipp_links(year)

  if (length(links$primary_zip) == 0 || is.na(links$primary_zip) || !nzchar(links$primary_zip)) links$primary_zip <- defaults$primary_zip
  if (length(links$replicate_zip) == 0 || is.na(links$replicate_zip) || !nzchar(links$replicate_zip)) links$replicate_zip <- defaults$replicate_zip
  if (length(links$dictionary_pdf) == 0 || is.na(links$dictionary_pdf) || !nzchar(links$dictionary_pdf)) links$dictionary_pdf <- defaults$dictionary_pdf
  if (length(links$replicate_dictionary_pdf) == 0 || is.na(links$replicate_dictionary_pdf) || !nzchar(links$replicate_dictionary_pdf)) links$replicate_dictionary_pdf <- defaults$replicate_dictionary_pdf

  primary_zip <- file.path(year_dir, paste0("sipp_", year, "_primary.zip"))
  replicate_zip <- file.path(year_dir, paste0("sipp_", year, "_replicate.zip"))
  dictionary_pdf <- file.path(year_dir, paste0(year, "_SIPP_Data_Dictionary.pdf"))
  rep_dictionary_pdf <- file.path(year_dir, paste0(year, "_rw", year, "_dictionary.txt.pdf"))

  download_if_missing(links$primary_zip, primary_zip)
  download_if_missing(links$replicate_zip, replicate_zip)
  download_if_missing(links$dictionary_pdf, dictionary_pdf)
  download_if_missing(links$replicate_dictionary_pdf, rep_dictionary_pdf)

  if (file.exists(primary_zip) && !file.exists(file.path(year_dir, paste0("pu", year, ".csv")))) {
    unzip(primary_zip, exdir = year_dir)
  }
  if (file.exists(replicate_zip) && !file.exists(file.path(year_dir, paste0("rw", year, ".csv")))) {
    unzip(replicate_zip, exdir = year_dir)
  }
}

cat("SIPP download cache refreshed in:", paths$sipp_raw_dir, "\n")
