#!/usr/bin/env Rscript

###############################################################################
# Script:    day07.R
# Project:   Advent of Code (AoC) 2025
# Author:    GRID
# Created:   12-01-2025 (MM-DD-YYYY)
# Purpose:   Solve AoC 2025 Day 7 – Laboratories
# Link:      https://adventofcode.com/2025/day/7
#
# Usage (CLI, from repo root):
#   Rscript 2025/Day7/day07.R 2025/Day7/input.txt
#
# Usage (interactive, from repo root):
#   source("2025/Day7/day07.R")
#   main("2025/Day7/input.txt")
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

# Turn raw lines into a structured object (tibble, list, etc.)
parse_input <- function(raw_lines) {
  # Convert to character matrix
  do.call(rbind, strsplit(raw_lines, ""))
}

#------------------------------------------------------------------------------
# Core Logic / Solvers
#------------------------------------------------------------------------------

solve_part1 <- function(grid) {
  nrows <- nrow(grid)
  ncols <- ncol(grid)
  
  # Find starting position S
  s_pos <- which(grid == "S", arr.ind = TRUE)
  start_row <- s_pos[1, "row"]
  start_col <- s_pos[1, "col"]
  
  # Track active beam columns (using unique handles merging)
  active_cols <- start_col
  split_count <- 0
  
  # Process row by row, starting from row after S
  for (row in (start_row + 1):nrows) {
    if (length(active_cols) == 0) break
    
    new_cols <- c()
    
    for (col in active_cols) {
      # Skip out-of-bounds
      if (col < 1 || col > ncols) next
      
      cell <- grid[row, col]
      
      if (cell == "^") {
        # Splitter: beam stops, two new beams emitted left & right
        split_count <- split_count + 1
        new_cols <- c(new_cols, col - 1, col + 1)
      } else {
        # Empty space: beam continues downward
        new_cols <- c(new_cols, col)
      }
    }
    
    # Merge beams (unique) and filter out-of-bounds
    active_cols <- unique(new_cols)
    active_cols <- active_cols[active_cols >= 1 & active_cols <= ncols]
  }
  
  split_count
}

solve_part2 <- function(grid) {
  nrows <- nrow(grid)
  ncols <- ncol(grid)
  
  # Find starting position S
  s_pos <- which(grid == "S", arr.ind = TRUE)
  start_row <- s_pos[1, "row"]
  start_col <- s_pos[1, "col"]
  
  # Track COUNT of timelines at each column (not just presence)
  counts <- rep(0, ncols)
  counts[start_col] <- 1  # Start with 1 timeline
  
  for (row in (start_row + 1):nrows) {
    if (sum(counts) == 0) break
    
    new_counts <- rep(0, ncols)
    
    for (col in 1:ncols) {
      if (counts[col] == 0) next
      
      cell <- grid[row, col]
      
      if (cell == "^") {
        # Splitter: each timeline splits into TWO timelines
        if (col > 1) new_counts[col - 1] <- new_counts[col - 1] + counts[col]
        if (col < ncols) new_counts[col + 1] <- new_counts[col + 1] + counts[col]
      } else {
        # Empty space: timelines continue downward unchanged
        new_counts[col] <- new_counts[col] + counts[col]
      }
    }
    
    counts <- new_counts
  }
  
  # Total timelines is sum of all counts across all columns
  sum(counts)
}

#------------------------------------------------------------------------------
# Quick checks / examples
#------------------------------------------------------------------------------

run_checks <- function() {
  example_raw <- read_lines(here("2025", "Day7", "example.txt"))
  example_dat <- parse_input(example_raw)

  stopifnot(
    solve_part1(example_dat) == 21,
    solve_part2(example_dat) == 40
  )
  invisible(TRUE)
}

#------------------------------------------------------------------------------
# Main / Orchestration
#------------------------------------------------------------------------------

main <- function(
    input_path = here("2025", "Day7", "input.txt"),
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
  default_input <- here("2025", "Day7", "input.txt")
  input_arg <- if (length(args) >= 1L) args[[1]] else default_input
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}
