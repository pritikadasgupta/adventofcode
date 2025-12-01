#!/usr/bin/env Rscript
# ==============================================================================
# Script:    day04.R
# Project:   Advent of Code (AoC) 2021
# Author:    GRID
# Created:   11-30-2025 (MM-DD-YYYY)
# Purpose:   Solve AoC 2021 Day 4 ("Giant Squid") – bingo boards
# Link:      https://adventofcode.com/2021/day/4
# Run as:    Rscript day04.R input.txt
# ==============================================================================

#------------------------------------------------------------------------------
# Global options
#------------------------------------------------------------------------------
options(
  warn = 1,
  scipen = 999,
  dplyr.summarise.inform = FALSE
)

set.seed(2025)

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
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages(
  invisible(lapply(required_pkgs, library, character.only = TRUE))
)

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
# Path helpers & I/O
#------------------------------------------------------------------------------

here <- function(...) {
  normalizePath(file.path(getwd(), ...), mustWork = FALSE)
}

read_lines <- function(path) {
  readr::read_lines(path)
}

#------------------------------------------------------------------------------
# Parsing
#------------------------------------------------------------------------------

#' Parse AoC 2021 Day 4 input into draws + boards
#'
#' @param x character vector of lines from the input file.
#' @return list(draws = numeric(), boards = 5 x 5 x n_boards array)

# The actual password is the number of times the dial is left pointing at 0 
# after any rotation in the sequence.

# The dial starts by pointing at 50.
parse_input <- function(x) {
  # Drop completely blank lines
  x <- x[x != ""]
  
  # First line: comma-separated draws
  draws <- x[1] |>
    stringr::str_split(",", simplify = TRUE) |>
    as.numeric()
  
  # Remaining lines: boards as 5x5 blocks
  board_lines <- x[-1]
  
  board_numbers <- board_lines |>
    stringr::str_squish() |>
    stringr::str_split("\\s+") |>
    unlist() |>
    as.numeric()
  
  if (length(board_numbers) %% 25 != 0) {
    stop("Board data length is not a multiple of 25.")
  }
  
  n_boards <- length(board_numbers) / 25
  boards <- array(board_numbers, dim = c(5, 5, n_boards))
  
  list(
    draws  = draws,
    boards = boards
  )
}

#------------------------------------------------------------------------------
# Core logic (pure functions)
#------------------------------------------------------------------------------

mark_board <- function(board, draw) {
  board[board == draw] <- NA_real_
  board
}

is_winner <- function(board) {
  any(apply(is.na(board), 1, all)) ||
    any(apply(is.na(board), 2, all))
}

score_board <- function(board, last_draw) {
  sum(board, na.rm = TRUE) * last_draw
}

#' Solve Part 1: score of the first winning board
#'
#' @param dat list(draws, boards) from parse_input()
#' @return numeric scalar
solve_part1 <- function(dat) {
  draws  <- dat$draws
  boards <- dat$boards
  
  boards1 <- boards
  n_boards <- dim(boards1)[3]
  
  for (d in draws) {
    for (b in seq_len(n_boards)) {
      boards1[, , b] <- mark_board(boards1[, , b], d)
      if (is_winner(boards1[, , b])) {
        return(score_board(boards1[, , b], d))
      }
    }
  }
  
  stop("No winning board found in Part 1.")
}

#' Solve Part 2: score of the last winning board
#'
#' @param dat list(draws, boards) from parse_input()
#' @return numeric scalar
solve_part2 <- function(dat) {
  draws  <- dat$draws
  boards <- dat$boards
  
  boards2 <- boards
  n_boards <- dim(boards2)[3]
  won <- rep(FALSE, n_boards)
  last_score <- NA_real_
  
  for (d in draws) {
    for (b in seq_len(n_boards)) {
      if (!won[b]) {
        boards2[, , b] <- mark_board(boards2[, , b], d)
        if (is_winner(boards2[, , b])) {
          won[b] <- TRUE
          last_score <- score_board(boards2[, , b], d)
        }
      }
    }
  }
  
  if (is.na(last_score)) {
    stop("No winning board found in Part 2.")
  }
  
  last_score
}

#------------------------------------------------------------------------------
# Sanity checks / examples
#------------------------------------------------------------------------------

run_checks <- function() {
  # If you save the official example as:
  #   2021/Day4/example.txt
  # you can turn on these checks.
  #
  # example_raw <- read_lines(here("2021", "Day4", "example.txt"))
  # example_dat <- parse_input(example_raw)
  #
  # stopifnot(
  #   solve_part1(example_dat) == 4512,
  #   solve_part2(example_dat) == 1924
  # )
  
  invisible(TRUE)
}

#------------------------------------------------------------------------------
# Main orchestration
#------------------------------------------------------------------------------

main <- function(
    input_path = here("2021", "Day4", "input.txt"),
    verbose = TRUE
) {
  if (!file.exists(input_path)) {
    stop("Input file does not exist: ", input_path)
  }
  
  if (verbose) message("Reading input from: ", input_path)
  
  raw <- read_lines(input_path)
  dat <- parse_input(raw)
  
  if (verbose) message("Solving Part 1 …")
  ans1 <- solve_part1(dat)
  
  if (verbose) message("Solving Part 2 …")
  ans2 <- solve_part2(dat)
  
  if (verbose) {
    message("Part 1: ", ans1)
    message("Part 2: ", ans2)
  }
  
  invisible(list(part1 = ans1, part2 = ans2))
}

#------------------------------------------------------------------------------
# CLI entrypoint
#------------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  
  input_arg <- if (length(args) >= 1L) args[[1]] else here("2021", "Day4", "input.txt")
  
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}
