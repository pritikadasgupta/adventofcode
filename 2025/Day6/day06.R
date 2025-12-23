#!/usr/bin/env Rscript

###############################################################################
# Script:    day06.R
# Project:   Advent of Code (AoC) 2025
# Author:    GRID
# Created:   12-01-2025 (MM-DD-YYYY)
# Purpose:   Solve AoC 2025 Day 6 – Trash Compactor
# Link:      https://adventofcode.com/2025/day/6
#
# Usage (CLI, from repo root):
#   Rscript 2025/Day6/day06.R 2025/Day6/input.txt
#
# Usage (interactive, from repo root):
#   source("2025/Day6/day06.R")
#   main("2025/Day6/input.txt")
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

parse_input <- function(raw_lines) {
  # Convert to character matrix - each character in its own column
  char_matrix <- do.call(rbind, strsplit(raw_lines, ""))
  
  nrows <- nrow(char_matrix)
  ncols <- ncol(char_matrix)
  
  # Find separator columns (all spaces)
  is_separator <- sapply(1:ncols, function(j) {
    all(char_matrix[, j] == " ")
  })
  
  # Group consecutive non-separator columns into problems
  problems <- list()
  problem_cols <- c()
  
  for (j in 1:ncols) {
    if (is_separator[j]) {
      if (length(problem_cols) > 0) {
        problems <- c(problems, list(problem_cols))
        problem_cols <- c()
      }
    } else {
      problem_cols <- c(problem_cols, j)
    }
  }
  # Don't forget abt the last problem if file doesn't end with separator
  if (length(problem_cols) > 0) {
    problems <- c(problems, list(problem_cols))
  }
  
  # Extract numbers and operator for each problem
  parsed <- lapply(problems, function(cols) {
    block <- char_matrix[, cols, drop = FALSE]
    
    # Last row has the operator
    op_row <- block[nrows, ]
    operator <- op_row[op_row %in% c("+", "*")][1]
    
    # Other rows have numbers - combine characters per row and parse
    numbers <- sapply(1:(nrows - 1), function(i) {
      row_chars <- paste0(block[i, ], collapse = "")
      row_chars <- trimws(row_chars)
      if (row_chars == "" || !grepl("\\d", row_chars)) return(NA_real_)
      as.numeric(row_chars)
    })
    numbers <- numbers[!is.na(numbers)]
    
    list(numbers = numbers, operator = operator)
  })
  
  parsed
}

solve_part1 <- function(dat) {
  results <- sapply(dat, function(prob) {
    if (prob$operator == "+") {
      sum(prob$numbers)
    } else {
      prod(prob$numbers)
    }
  })
  
  sum(results)
}

solve_part2 <- function(dat) {
  # answer for Part 2
  NA_real_  # replace with actual logic
}

#------------------------------------------------------------------------------
# Quick checks / examples
#------------------------------------------------------------------------------

run_checks <- function() {
  example_raw <- read_lines(here("2025", "Day6", "example.txt"))
  example_dat <- parse_input(example_raw)

  stopifnot(
    solve_part1(example_dat) == 4277556,
    solve_part2(example_dat) == 3263827
  )
  invisible(TRUE)
}

#------------------------------------------------------------------------------
# Main / Orchestration
#------------------------------------------------------------------------------

main <- function(
    input_path = here("2025", "Day6", "input.txt"),
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
  default_input <- here("2025", "Day6", "input.txt")
  input_arg <- if (length(args) >= 1L) args[[1]] else default_input
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}
