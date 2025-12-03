#!/usr/bin/env Rscript

###############################################################################
# Script:    day02.R
# Project:   Advent of Code (AoC) 2025
# Author:    GRID
# Created:   12-01-2025 (MM-DD-YYYY)
# Purpose:   Solve AoC 2025 Day 2 – Gift Shop
# Link:      https://adventofcode.com/2025/day/2
#
# Usage (CLI, from repo root):
#   Rscript 2025/Day2/day02.R 2025/Day2/input.txt
#
# Usage (interactive, from repo root):
#   source("2025/Day2/day02.R")
#   main("2025/Day2/input.txt")
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

#------------------------------------------------------------------------------
# Parsing / Pre-processing
#------------------------------------------------------------------------------

# Turn raw line into a structured object (list)
parse_input <- function(raw_lines) {
  split_comma_lines <- strsplit(raw_lines, ",")[[1]]
  split_hyphen_lines <- strsplit(split_comma_lines, "-")
  return(split_hyphen_lines)
}

#------------------------------------------------------------------------------
# Core Logic / Solvers
#------------------------------------------------------------------------------

solve_part1 <- function(dat) {
  invalid_ids <- c()
  # Find numbers within the range

  for (i in 1:length(dat)){
    lower_bound <- as.numeric(dat[[i]][1])
    upper_bound <- as.numeric(dat[[i]][2])
    id_range <- seq.int(lower_bound, upper_bound)
    
    # Find invalid IDs: IDs cannot ONLY be made of of some 
    # sequence of digits repeated twice
    # Find invalid IDs: IDs cannot have leading zeroes
    
    # ^ - start of string
    # (.+) — capture group: one or more characters (the first half)
    # \\1 — backreference: must match exactly what was captured
    # $ — end of string
    # reference: I used https://regexr.com/ to help me test!

    numbers_with_consecutive_repeats <- id_range[grepl("^(.+)\\1$", id_range)]
    
    invalid_ids <- c(invalid_ids,numbers_with_consecutive_repeats)
    # print(numbers_with_consecutive_repeats)
  }
  # Sum of Invalid IDs
  sum(invalid_ids)
}

solve_part2 <- function(dat) {
  invalid_ids <- c()
  
  # Find numbers within the range
  for (i in 1:length(dat)){
    lower_bound <- as.numeric(dat[[i]][1])
    upper_bound <- as.numeric(dat[[i]][2])
    id_range <- seq.int(lower_bound, upper_bound)
    
    # Find invalid IDs: ID is invalid if it is made only of some 
    # sequence of digits repeated at least twice.
    
    # ^ — start of string
    # (.+) — capture group of one or more characters
    # \\1+ — if the backreference repeated one or more times 
    # (so if the pattern appears 2+ times total)
    # $ — end of string

    numbers_with_consecutive_repeats <- id_range[grepl("^(.+)\\1+$", id_range)]
    
    invalid_ids <- c(invalid_ids,numbers_with_consecutive_repeats)
    print(numbers_with_consecutive_repeats)
  }
  # Sum of Invalid IDs
  sum(invalid_ids)

}

#------------------------------------------------------------------------------
# Run and check example input
#------------------------------------------------------------------------------

run_checks <- function() {
  example_raw <- read_lines(here("2025", "Day2", "example.txt"))
  example_dat <- parse_input(example_raw)

  stopifnot(
    solve_part1(example_dat) == 1227775554,
    solve_part2(example_dat) == 4174379265
  )
  invisible(TRUE)
}

#------------------------------------------------------------------------------
# Main / Orchestration
#------------------------------------------------------------------------------

main <- function(
    input_path = here("2025", "Day2", "input.txt"),
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
  default_input <- here("2025", "Day2", "input.txt")
  input_arg <- if (length(args) >= 1L) args[[1]] else default_input
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}
