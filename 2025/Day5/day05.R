#!/usr/bin/env Rscript

###############################################################################
# Script:    day05.R
# Project:   Advent of Code (AoC) 2025
# Author:    GRID
# Created:   12-01-2025 (MM-DD-YYYY)
# Purpose:   Solve AoC 2025 Day 5 – Cafeteria
# Link:      https://adventofcode.com/2025/day/5
#
# Usage (CLI, from repo root):
#   Rscript 2025/Day5/day05.R 2025/Day5/input.txt
#
# Usage (interactive, from repo root):
#   source("2025/Day5/day05.R")
#   main("2025/Day5/input.txt")
###############################################################################

#------------------------------------------------------------------------------
# Global options
#------------------------------------------------------------------------------

options(
  warn   = 1,
  scipen = 999,
  digits = 6,
  dplyr.summarise.inform = FALSE
)

set.seed(369L)

#------------------------------------------------------------------------------
# Packages
#------------------------------------------------------------------------------

required_pkgs <- c(
  "readr",
  "stringr",
  "dplyr",
  "purrr",
  "tibble"
)

missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  stop(
    "Missing packages: ",
    paste(missing_pkgs, collapse = ", "),
    "\nInstall them first, e.g. install.packages(c(",
    paste(sprintf('\"%s\"', missing_pkgs), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages(
  invisible(lapply(required_pkgs, library, character.only = TRUE))
)

# Use only when working interactively (RStudio, etc.) and not when you use "Rscript"!!
if (interactive() && requireNamespace("conflicted", quietly = TRUE)) {
  suppressMessages(
    conflicted::conflicts_prefer(
      dplyr::filter,
      dplyr::lag,
      dplyr::summarise
    )
  )
}

#------------------------------------------------------------------------------
# Helpers: logging, paths, assertions, I/O
#------------------------------------------------------------------------------

log_info <- function(..., .time = TRUE) {
  msg <- paste0(...)
  if (.time) {
    ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    msg <- sprintf("[%s] %s", ts, msg)
  }
  message(msg)
}

# project-rooted path helper (no {here} dependency)
here <- function(...) {
  normalizePath(file.path(getwd(), ...), mustWork = FALSE)
}

assert <- function(cond, msg = "Assertion failed") {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

read_lines <- function(path) {
  assert(file.exists(path), paste0("Input file not found: ", path))
  readr::read_lines(path)
}

#------------------------------------------------------------------------------
# Parsing / Pre-processing
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Core Logic / Solvers
#------------------------------------------------------------------------------

solve_part1 <- function(lines) {
  # Read and split by blank line
  blank <- which(lines == "")
  
  # Parse ranges (before blank line)
  range_lines <- lines[1:(blank - 1)]
  ranges <- do.call(rbind, lapply(strsplit(range_lines, "-"), as.numeric))
  
  # Parse available IDs (after blank line)
  ids <- as.numeric(lines[(blank + 1):length(lines)])
  
  # Check each ID against all ranges
  is_fresh <- sapply(ids, function(id) {
    any(id >= ranges[, 1] & id <= ranges[, 2])
  })
  
  sum(is_fresh)
}

solve_part2 <- function(lines) {
  blank <- which(lines == "")
  
  # Parse ranges
  range_lines <- lines[1:(blank - 1)]
  ranges <- do.call(rbind, lapply(strsplit(range_lines, "-"), as.numeric))
  
  # Sort by start value
  ranges <- ranges[order(ranges[, 1]), ]
  
  # Merge overlapping ranges
  merged <- list()
  current <- ranges[1, ]
  
  for (i in 2:nrow(ranges)) {
    if (ranges[i, 1] <= current[2]) {
      # Overlapping - extend current range
      current[2] <- max(current[2], ranges[i, 2])
    } else {
      # No overlap - save current and start new
      merged <- c(merged, list(current))
      current <- ranges[i, ]
    }
  }
  merged <- c(merged, list(current))  # last one!!
  
  # count total IDs: sum of (end - start + 1) for each merged range
  sum(sapply(merged, function(r) r[2] - r[1] + 1))
}

#------------------------------------------------------------------------------
# Quick checks / examples
#------------------------------------------------------------------------------

run_checks <- function() {
  example_raw <- readLines(here("2025", "Day5", "example.txt"))
  example_dat <- example_raw

  stopifnot(
    solve_part1(example_dat) == 3,
    solve_part2(example_dat) == 14
  )
  invisible(TRUE)
}

#------------------------------------------------------------------------------
# Main / Orchestration
#------------------------------------------------------------------------------

main <- function(
    input_path = here("2025", "Day5", "input.txt"),
    verbose = TRUE
) {
  if (verbose) log_info("Reading input from: ", input_path)
  raw <- read_lines(input_path)
  
  if (verbose) log_info("Solving Part 1 …")
  ans1 <- solve_part1(raw)  # Pass raw directly, not dat
  
  if (verbose) log_info("Solving Part 2 …")
  ans2 <- solve_part2(raw)
  
  if (verbose) {
    log_info("Part 1 result: ", ans1)
    log_info("Part 2 result: ", ans2)
  }
  
  invisible(list(part1 = ans1, part2 = ans2))
}

#------------------------------------------------------------------------------
# Script entry point (CLI)
#------------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  default_input <- here("2025", "Day5", "input.txt")
  input_arg <- if (length(args) >= 1L) args[[1]] else default_input
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}
