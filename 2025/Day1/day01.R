#!/usr/bin/env Rscript

###############################################################################
# Script:    day01.R
# Project:   Advent of Code (AoC) 2025
# Author:    GRID
# Created:   12-01-2025 (MM-DD-YYYY)
# Purpose:   Solve AoC 2025 Day 1 – Secret Entrance
# Link:      https://adventofcode.com/2025/day/1
#
# Usage (CLI, from repo root):
#   Rscript 2025/Day1/day01.R "2025/Day1/input.txt"
#
# Usage (interactive, from repo root):
#   source("2025/Day1/day01.R")
#   main("2025/Day1/input.txt")
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
  # "readr",
  # "stringr",
  # "dplyr",
  # "purrr",
  # "tibble"
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

# Turn raw lines into a structured object (tibble, list, etc.)
parse_input <- function(raw_lines) {
  # one line per record
  tibble(line = raw_lines)
}

#------------------------------------------------------------------------------
# Core Logic / Solvers
#------------------------------------------------------------------------------

# The actual password is the number of times the dial is left pointing at 0 
# after any rotation in the sequence.

parse_input <- function(rotation) {
  first_letter <- substr(rotation, 1, 1)
  instruction <- as.numeric(gsub("[^0-9.]", "", rotation))
  if(first_letter == "L"){
    instruction <- -instruction
  }
  return(instruction)
}

remove_empty_lines <- function(lines){
  if (length(lines) > 0 && lines[[length(lines)]] == "") {
    lines_no_empty_end <- lines[[-length(lines)]]
  } else {
    lines_no_empty_end <- lines # No empty line at the end to remove
  }
  return(lines_no_empty_end)
}

solve_part1 <- function(dat) {
  dat <- remove_empty_lines(dat) # Remove any empty lines in the txt file
  dat <- vapply(dat,parse_input,FUN.VALUE = numeric(1),USE.NAMES = FALSE) # Parse input
  new_dat <- c()
  for(i in 1:(length(dat))){
    #for rotations greater than 100
    multiplier <- ifelse(abs(dat[i]) >= 100,abs(round(dat[i]/100,0)),1)
    
    if (i==1) { # The dial starts by pointing at 50
      dat_sum <- 50 + dat[i]
      if (dat_sum < 0) {
        dat_sum <- multiplier*100 + dat_sum
      }else if (dat_sum > 99) {
        dat_sum <- dat_sum - multiplier*100
      }
      
      if (dat_sum == 100) {
        dat_sum <- 0
      }
      
    } else {
      dat_sum <- dat_sum + dat[i]
      if (dat_sum < 0) {
        dat_sum <- multiplier*100 + dat_sum
      }else if (dat_sum > 99) {
        dat_sum <- dat_sum - multiplier*100
      }
      
      if (dat_sum == 100) {
        dat_sum <- 0
      }
      
    }
    new_dat <- c(new_dat,dat_sum)
  }
  number_of_zeroes <- length(which(new_dat == 0))
  return(number_of_zeroes)
}

solve_part2 <- function(dat) {
  # answer for Part 2
  NA_real_  # replace with actual logic
}

#------------------------------------------------------------------------------
# Quick checks / examples
#------------------------------------------------------------------------------

run_checks <- function() {
  example_raw <- read_lines(here("2025", "Day1", "example.txt"))
  stopifnot(
    solve_part1(example_raw) == 3
    # solve_part2(example_dat) == <expected2>
  )
  invisible(TRUE)
}

#------------------------------------------------------------------------------
# Main / Orchestration
#------------------------------------------------------------------------------

main <- function(
    input_path = here("2025", "Day1", "input.txt"),
    verbose = TRUE
) {
  if (verbose) log_info("Reading input from: ", input_path)
  dat <- read_lines(input_path)

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
  default_input <- here("2025", "Day1", "input.txt")
  input_arg <- if (length(args) >= 1L) args[[1]] else default_input
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}
