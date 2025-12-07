#!/usr/bin/env Rscript

###############################################################################
# Script:    day03.R
# Project:   Advent of Code (AoC) 2025
# Author:    GRID
# Created:   12-03-2025 (MM-DD-YYYY)
# Finished:  12-07-2025
# Purpose:   Solve AoC 2025 Day 3 – Lobby
# Link:      https://adventofcode.com/2025/day/3
#
# Usage (CLI, from repo root):
#   Rscript 2025/Day3/day03.R 2025/Day3/input.txt
#
# Usage (interactive, from repo root):
#   source("2025/Day3/day03.R")
#   main("2025/Day3/input.txt")
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
  # "ggplot2",
  # "data.table",
  # "janitor",
  # "glue"
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

solve_bank <- function(dat, k) {
  my_joltages <- c()
  
  for (i in 1:ncol(dat)) {
    bank <- dat[, i]
    n <- length(bank)
    
    selected_digits <- c()
    start_idx <- 1
    
    for (pos in 1:k) {
      end_idx <- n - k + pos
      search_range <- start_idx:end_idx
      
      largest_number <- max(bank[search_range])
      largest_number_index <- start_idx + which(bank[search_range] == largest_number)[1] - 1
      
      selected_digits <- c(selected_digits, largest_number)
      start_idx <- largest_number_index + 1
    }
    
    largest_joltage <- as.numeric(paste(selected_digits, collapse = ""))
    my_joltages <- c(my_joltages, largest_joltage)
  }
  
  sum(my_joltages)
}

#------------------------------------------------------------------------------
# Parsing / Pre-processing
#------------------------------------------------------------------------------

# Turn raw lines into a structured object (tibble, list, etc.)
parse_input <- function(raw_lines) {
  split_lines <- sapply(strsplit(raw_lines, ""),as.numeric)
}

#------------------------------------------------------------------------------
# Core Logic / Solvers
#------------------------------------------------------------------------------

solve_part1 <- function(dat) solve_bank(dat, k = 2)
solve_part2 <- function(dat) solve_bank(dat, k = 12)

#------------------------------------------------------------------------------
# Quick checks / examples
#------------------------------------------------------------------------------

run_checks <- function() {
  example_raw <- read_lines(here("2025", "Day3", "example.txt"))
  example_dat <- parse_input(example_raw)

  stopifnot(
    solve_part1(example_dat) == 357,
    solve_part2(example_dat) == 3121910778619
  )
  invisible(TRUE)
}

#------------------------------------------------------------------------------
# Main / Orchestration
#------------------------------------------------------------------------------

main <- function(
    input_path = here("2025", "Day3", "input.txt"),
    verbose = TRUE
) {
  if (verbose) log_info("Reading input from: ", input_path)
  raw <- read_lines(input_path)
  
  if (verbose) log_info("Parsing input...")
  dat <- parse_input(raw)
  
  if (verbose) log_info("Solving Part 1 …")
  ans1 <- solve_part1(dat)
  
  if (verbose) log_info("Solving Part 2 …")
  ans2 <- solve_part2(dat)
  
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
  default_input <- here("2025", "Day3", "input.txt")
  input_arg <- if (length(args) >= 1L) args[[1]] else default_input
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}
