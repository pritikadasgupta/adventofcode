#!/usr/bin/env Rscript

###############################################################################
# Script:    day09.R
# Project:   Advent of Code (AoC) 2025
# Author:    GRID
# Created:   12-01-2025 (MM-DD-YYYY)
# Purpose:   Solve AoC 2025 Day 9 – Movie Theater
# Link:      https://adventofcode.com/2025/day/9
#
# Usage (CLI, from repo root):
#   Rscript 2025/Day9/day09.R 2025/Day9/input.txt
#
# Usage (interactive, from repo root):
#   source("2025/Day9/day09.R")
#   main("2025/Day9/input.txt")
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
  coords <- do.call(rbind, lapply(raw_lines, function(line) {
    as.numeric(strsplit(line, ",")[[1]])
  }))
  colnames(coords) <- c("x", "y")
  coords
}

#------------------------------------------------------------------------------
# Core Logic / Solvers
#------------------------------------------------------------------------------

solve_part1 <- function(coords) {
  n <- nrow(coords)
  max_area <- 0
  
  # Check all pairs of red tiles as opposite corners
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      # +1 because rectangle includes both corner tiles
      width <- abs(coords[j, "x"] - coords[i, "x"]) + 1
      height <- abs(coords[j, "y"] - coords[i, "y"]) + 1
      area <- width * height
      max_area <- max(max_area, area)
    }
  }
  
  max_area
}

solve_part2 <- function(coords) {
  n <- nrow(coords)
  
  # Build set of all red and green tiles
  red_green <- matrix(FALSE, nrow = max(coords[,"y"]) + 1, ncol = max(coords[,"x"]) + 1)
  
  # Mark red tiles
  for (i in 1:n) {
    red_green[coords[i, "y"], coords[i, "x"]] <- TRUE
  }
  
  # Mark green tiles along edges between consecutive red tiles (list wraps)
  for (i in 1:n) {
    next_i <- if (i == n) 1 else i + 1
    x1 <- coords[i, "x"]; y1 <- coords[i, "y"]
    x2 <- coords[next_i, "x"]; y2 <- coords[next_i, "y"]
    
    # Draw line between them
    if (x1 == x2) {
      # Vertical line
      for (y in min(y1, y2):max(y1, y2)) {
        red_green[y, x1] <- TRUE
      }
    } else {
      # Horizontal line
      for (x in min(x1, x2):max(x1, x2)) {
        red_green[y1, x] <- TRUE
      }
    }
  }
  
  # Flood fill interior using ray casting or scanline
  # For each cell, count crossings to determine if inside polygon
  # Build polygon edges for ray casting
  edges <- list()
  for (i in 1:n) {
    next_i <- if (i == n) 1 else i + 1
    edges[[i]] <- list(
      x1 = coords[i, "x"], y1 = coords[i, "y"],
      x2 = coords[next_i, "x"], y2 = coords[next_i, "y"]
    )
  }
  
  # Check each cell if it's inside the polygon
  for (y in 1:nrow(red_green)) {
    for (x in 1:ncol(red_green)) {
      if (!red_green[y, x] && point_in_polygon(x - 0.5, y - 0.5, edges)) {
        red_green[y, x] <- TRUE
      }
    }
  }
  
  # Now find largest rectangle with red corners where all tiles are red/green
  max_area <- 0
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      x1 <- coords[i, "x"]; y1 <- coords[i, "y"]
      x2 <- coords[j, "x"]; y2 <- coords[j, "y"]
      
      # Check if all tiles in rectangle are red/green
      min_x <- min(x1, x2); max_x <- max(x1, x2)
      min_y <- min(y1, y2); max_y <- max(y1, y2)
      
      all_valid <- TRUE
      for (y in min_y:max_y) {
        for (x in min_x:max_x) {
          if (!red_green[y, x]) {
            all_valid <- FALSE
            break
          }
        }
        if (!all_valid) break
      }
      
      if (all_valid) {
        width <- max_x - min_x + 1
        height <- max_y - min_y + 1
        area <- width * height
        max_area <- max(max_area, area)
      }
    }
  }
  
  max_area
}

# Ray casting algorithm to check if point is inside polygon
point_in_polygon <- function(px, py, edges) {
  crossings <- 0
  
  for (edge in edges) {
    x1 <- edge$x1; y1 <- edge$y1
    x2 <- edge$x2; y2 <- edge$y2
    
    # Check if horizontal ray from (px, py) going right crosses this edge
    if ((y1 > py) != (y2 > py)) {
      # Edge crosses the horizontal line at py
      x_intersect <- x1 + (py - y1) / (y2 - y1) * (x2 - x1)
      if (px < x_intersect) {
        crossings <- crossings + 1
      }
    }
  }
  
  (crossings %% 2) == 1
}

#------------------------------------------------------------------------------
# Quick checks / examples
#------------------------------------------------------------------------------

run_checks <- function() {
  example_raw <- read_lines(here("2025", "Day9", "example.txt"))
  example_dat <- parse_input(example_raw)

  stopifnot(
    solve_part1(example_dat) == 50,
    solve_part2(example_dat) == 24
  )
  invisible(TRUE)
}

#------------------------------------------------------------------------------
# Main / Orchestration
#------------------------------------------------------------------------------

main <- function(
    input_path = here("2025", "Day9", "input.txt"),
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
  default_input <- here("2025", "Day9", "input.txt")
  input_arg <- if (length(args) >= 1L) args[[1]] else default_input
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}
