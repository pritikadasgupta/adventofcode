#!/usr/bin/env Rscript

###############################################################################
# Script:    day12.R
# Project:   Advent of Code (AoC) 2025
# Author:    GRID
# Created:   12-01-2025 (MM-DD-YYYY)
# Purpose:   Solve AoC 2025 Day 12 – Christmas Tree Farm
# Link:      https://adventofcode.com/2025/day/12
#
# Usage (CLI, from repo root):
#   Rscript 2025/Day12/day12.R 2025/Day12/input.txt
#
# Usage (interactive, from repo root):
#   source("2025/Day12/day12.R")
#   main("2025/Day12/input.txt")
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
  shapes <- list()
  regions <- list()
  
  i <- 1
  # Parse shapes
  while (i <= length(raw_lines) && grepl("^\\d+:$", raw_lines[i])) {
    shape_id <- as.integer(gsub(":", "", raw_lines[i]))
    i <- i + 1
    shape_rows <- c()
    while (i <= length(raw_lines) && grepl("^[#.]+$", raw_lines[i])) {
      shape_rows <- c(shape_rows, raw_lines[i])
      i <- i + 1
    }
    # Convert to matrix of coordinates
    shapes[[shape_id + 1]] <- parse_shape(shape_rows)
    while (i <= length(raw_lines) && raw_lines[i] == "") i <- i + 1
  }
  
  # Parse regions
  while (i <= length(raw_lines)) {
    if (raw_lines[i] != "") {
      parts <- strsplit(raw_lines[i], ": ")[[1]]
      dims <- as.integer(strsplit(parts[1], "x")[[1]])
      counts <- as.integer(strsplit(parts[2], " ")[[1]])
      regions[[length(regions) + 1]] <- list(
        width = dims[1], height = dims[2], counts = counts
      )
    }
    i <- i + 1
  }
  
  list(shapes = shapes, regions = regions)
}

parse_shape <- function(rows) {
  coords <- c()
  for (r in seq_along(rows)) {
    chars <- strsplit(rows[r], "")[[1]]
    for (c in seq_along(chars)) {
      if (chars[c] == "#") {
        coords <- rbind(coords, c(r - 1, c - 1))
      }
    }
  }
  # Normalize to origin
  coords[,1] <- coords[,1] - min(coords[,1])
  coords[,2] <- coords[,2] - min(coords[,2])
  coords
}

get_rotations_flips <- function(shape) {
  orientations <- list()
  s <- shape
  for (flip in 1:2) {
    for (rot in 1:4) {
      # Normalize
      s[,1] <- s[,1] - min(s[,1])
      s[,2] <- s[,2] - min(s[,2])
      # Add unique orientation
      key <- paste(s[order(s[,1], s[,2]),], collapse = ",")
      if (!key %in% names(orientations)) {
        orientations[[key]] <- s
      }
      # Rotate 90 degrees: (r,c) -> (c, -r)
      s <- cbind(s[,2], -s[,1])
    }
    # Flip: (r,c) -> (r, -c)
    s <- cbind(shape[,1], -shape[,2])
  }
  orientations
}

can_place <- function(grid, shape, start_r, start_c) {
  for (i in 1:nrow(shape)) {
    r <- start_r + shape[i, 1]
    c <- start_c + shape[i, 2]
    if (r < 1 || r > nrow(grid) || c < 1 || c > ncol(grid)) return(FALSE)
    if (grid[r, c] != 0) return(FALSE)
  }
  TRUE
}

place_shape <- function(grid, shape, start_r, start_c, id) {
  for (i in 1:nrow(shape)) {
    r <- start_r + shape[i, 1]
    c <- start_c + shape[i, 2]
    grid[r, c] <- id
  }
  grid
}

solve_region <- function(shapes, region) {
  width <- region$width
  height <- region$height
  counts <- region$counts
  
  # Build list of pieces to place
  pieces <- list()
  for (shape_idx in seq_along(counts)) {
    for (k in seq_len(counts[shape_idx])) {
      pieces[[length(pieces) + 1]] <- shapes[[shape_idx]]
    }
  }
  
  if (length(pieces) == 0) return(TRUE)
  
  grid <- matrix(0, nrow = height, ncol = width)
  
  # Backtracking
  solve_backtrack <- function(piece_idx) {
    if (piece_idx > length(pieces)) return(TRUE)
    
    shape <- pieces[[piece_idx]]
    orientations <- get_rotations_flips(shape)
    
    for (orient in orientations) {
      for (r in 1:height) {
        for (c in 1:width) {
          if (can_place(grid, orient, r, c)) {
            grid <<- place_shape(grid, orient, r, c, piece_idx)
            if (solve_backtrack(piece_idx + 1)) return(TRUE)
            grid <<- place_shape(grid, orient, r, c, 0)  # Undo
          }
        }
      }
    }
    FALSE
  }
  
  solve_backtrack(1)
}

#------------------------------------------------------------------------------
# Core Logic / Solvers
#------------------------------------------------------------------------------

solve_part1 <- function(dat) {
  sum(sapply(dat$regions, function(region) {
    if (solve_region(dat$shapes, region)) 1 else 0
  }))
}

solve_part2 <- function(dat) {
  # answer for Part 2
  NA_real_  # replace with actual logic
}

#------------------------------------------------------------------------------
# Quick checks / examples
#------------------------------------------------------------------------------

run_checks <- function() {
  example_raw <- read_lines(here("2025", "Day12", "example.txt"))
  example_dat <- parse_input(example_raw)

  stopifnot(
    solve_part1(example_dat) == 2
    # solve_part2(example_dat) == <expected2>
  )
  invisible(TRUE)
}

#------------------------------------------------------------------------------
# Main / Orchestration
#------------------------------------------------------------------------------

main <- function(
    input_path = here("2025", "Day12", "input.txt"),
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
  default_input <- here("2025", "Day12", "input.txt")
  input_arg <- if (length(args) >= 1L) args[[1]] else default_input
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}
