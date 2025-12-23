#!/usr/bin/env Rscript

###############################################################################
# Script:    day10.R
# Project:   Advent of Code (AoC) 2025
# Author:    GRID
# Created:   12-01-2025 (MM-DD-YYYY)
# Purpose:   Solve AoC 2025 Day 10 – Factory
# Link:      https://adventofcode.com/2025/day/10
#
# Usage (CLI, from repo root):
#   Rscript 2025/Day10/day10.R 2025/Day10/input.txt
#
# Usage (interactive, from repo root):
#   source("2025/Day10/day10.R")
#   main("2025/Day10/input.txt")
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

# Turn raw lines into a structured object (tibble, list, etc.)
parse_input <- function(raw_lines) {
  lapply(raw_lines, function(line) {
    # Extract [.##.] pattern
    target_match <- regmatches(line, regexpr("\\[[.#]+\\]", line))
    # Remove brackets more reliably
    target_str <- substr(target_match, 2, nchar(target_match) - 1)
    target <- as.integer(strsplit(target_str, "")[[1]] == "#")
    
    # Extract all (x,y,z) button patterns
    button_matches <- gregexpr("\\([0-9,]+\\)", line)
    button_strs <- regmatches(line, button_matches)[[1]]
    
    buttons <- lapply(button_strs, function(b) {
      nums <- gsub("[()]", "", b)
      as.integer(strsplit(nums, ",")[[1]]) + 1  # Convert to 1-indexed
    })
    
    list(target = target, buttons = buttons, n_lights = length(target))
  })
}

solve_machine <- function(machine) {
  n_lights <- machine$n_lights
  target <- machine$target
  buttons <- machine$buttons
  n_buttons <- length(buttons)
  
  # Edge case: no buttons
  if (n_buttons == 0) {
    if (all(target == 0)) return(0) else return(Inf)
  }
  
  min_presses <- Inf
  
  # Try all combinations of button presses (2^n_buttons)
  for (mask in 0:(2^n_buttons - 1)) {
    state <- rep(0L, n_lights)
    presses <- 0
    
    for (b in seq_len(n_buttons)) {  # seq_len is safer than 1:n
      if (bitwAnd(mask, bitwShiftL(1L, b - 1)) > 0) {
        presses <- presses + 1
        for (light in buttons[[b]]) {
          if (light >= 1 && light <= n_lights) {
            state[light] <- 1L - state[light]
          }
        }
      }
    }
    
    if (all(state == target)) {
      min_presses <- min(min_presses, presses)
    }
  }
  
  min_presses
}

#------------------------------------------------------------------------------
# Core Logic / Solvers
#------------------------------------------------------------------------------

solve_part1 <- function(machines) {
  results <- sapply(machines, solve_machine)
  cat("Individual machine results:", results, "\n")  # Debug output
  sum(results)
}

parse_input <- function(raw_lines) {
  lapply(raw_lines, function(line) {
    # Extract [.##.] pattern (for part 1)
    target_match <- regmatches(line, regexpr("\\[[.#]+\\]", line))
    target_str <- substr(target_match, 2, nchar(target_match) - 1)
    target <- as.integer(strsplit(target_str, "")[[1]] == "#")
    
    # Extract all (x,y,z) button patterns
    button_matches <- gregexpr("\\([0-9,]+\\)", line)
    button_strs <- regmatches(line, button_matches)[[1]]
    
    buttons <- lapply(button_strs, function(b) {
      nums <- gsub("[()]", "", b)
      as.integer(strsplit(nums, ",")[[1]]) + 1 # Convert to 1-indexed
    })
    
    # Extract {x,y,z} joltage requirements (for part 2)
    joltage_match <- regmatches(line, regexpr("\\{[0-9,]+\\}", line))
    joltage_str <- gsub("[{}]", "", joltage_match)
    joltage <- as.integer(strsplit(joltage_str, ",")[[1]])
    
    list(target = target, buttons = buttons, n_lights = length(target),
         joltage = joltage, n_counters = length(joltage))
  })
}

solve_machine_part2 <- function(machine) {
  joltage <- machine$joltage
  buttons <- machine$buttons
  n_counters <- machine$n_counters
  n_buttons <- length(buttons)
  
  # Build coefficient matrix: each column is a button, each row is a counter
  # A[i,j] = 1 if button j affects counter i, else 0
  A <- matrix(0, nrow = n_counters, ncol = n_buttons)
  for (j in seq_len(n_buttons)) {
    for (counter in buttons[[j]]) {
      if (counter <= n_counters) {
        A[counter, j] <- 1
      }
    }
  }
  
  # We need to solve: A %*% x = joltage, where x >= 0 integers
  # Minimize sum(x)
  
  # Use brute force with upper bound on each button press
  # Upper bound: max(joltage) since pressing more than that is wasteful
  max_presses <- max(joltage)
  
  min_total <- Inf
  
  # Recursive search with pruning
  solve_recursive <- function(button_idx, current_state, current_presses) {
    if (current_presses >= min_total) return() # Pruning
    
    if (button_idx > n_buttons)
      if (all(current_state == joltage)) {
        min_total <<- current_presses
      }
    return()
  }
  
  # How much more do we need for each counter?
  remaining <- joltage - current_state
  if (any(remaining < 0)) return() # Overshot
  
  # Try different number of presses for this button
  button_effect <- A[, button_idx]
  
  # Max useful presses for this button
  if (sum(button_effect) > 0) {
    max_for_button <- max(ceiling(remaining / pmax(button_effect, 0.001)))
    max_for_button <- min(max_for_button, max_presses)
  } else {
    max_for_button <- 0
  }
  
  for (presses in 0:max_for_button) {
    new_state <- current_state + presses * button_effect
    if (all(new_state <= joltage)) { # Don't overshoot
      solve_recursive(button_idx + 1, new_state, current_presses + presses)
    }
  }
}

solve_recursive(1, rep(0, n_counters), 0)

if (is.infinite(min_total)) {
  warning("No solution found for machine")
  return(NA)
}
min_total
}

solve_part2 <- function(machines) {
  sum(sapply(machines, solve_machine_part2))
}

#------------------------------------------------------------------------------
# Quick checks / examples
#------------------------------------------------------------------------------

# Debugging!!
run_checks <- function() {
  example_raw <- read_lines(here("2025", "Day10", "example.txt"))
  cat("Raw lines:\n")
  print(example_raw)
  
  example_dat <- parse_input(example_raw)
  cat("\nParsed machines:\n")
  print(example_dat)
  
  stopifnot(
    solve_part1(example_dat) == 7
  )
  invisible(TRUE)
}

#------------------------------------------------------------------------------
# Main / Orchestration
#------------------------------------------------------------------------------

main <- function(
    input_path = here("2025", "Day10", "input.txt"),
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
  default_input <- here("2025", "Day10", "input.txt")
  input_arg <- if (length(args) >= 1L) args[[1]] else default_input
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}
