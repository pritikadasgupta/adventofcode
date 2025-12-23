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
  
  # Build polygon edges (connecting consecutive red tiles)
  edges <- list()
  for (i in 1:n) {
    next_i <- if (i == n) 1 else i + 1
    edges[[i]] <- list(
      x1 = coords[i, "x"], y1 = coords[i, "y"],
      x2 = coords[next_i, "x"], y2 = coords[next_i, "y"]
    )
  }
  
  # Get all unique y-values and sort them
  all_y <- sort(unique(c(coords[, "y"], sapply(edges, function(e) c(e$y1, e$y2)))))
  
  # For each horizontal slab between consecutive y-values, compute valid x-intervals
  # Use scanline: track which vertical edges are "active"
  
  # Precompute valid x-ranges for each y-coordinate using ray casting
  # But only for y-values we care about (unique y's from coords)
  
  get_x_range_at_y <- function(y) {
    # Find all x-coordinates where the polygon boundary intersects this y
    x_intersections <- c()
    
    for (edge in edges) {
      y1 <- edge$y1; y2 <- edge$y2
      x1 <- edge$x1; x2 <- edge$x2
      
      if (y1 == y2) {
        # Horizontal edge at this y
        if (y1 == y) {
          x_intersections <- c(x_intersections, min(x1, x2), max(x1, x2))
        }
      } else {
        # Vertical edge - check if y is in range
        if (y >= min(y1, y2) && y <= max(y1, y2)) {
          x_intersections <- c(x_intersections, x1)  # x1 == x2 for vertical
        }
      }
    }
    
    if (length(x_intersections) == 0) return(NULL)
    
    # For rectilinear polygon, the valid range at this y is between boundary points
    x_intersections <- sort(unique(x_intersections))
    
    # Return the full range covered by polygon at this y
    list(min_x = min(x_intersections), max_x = max(x_intersections))
  }
  
  # Check if a rectangle is entirely within the polygon
  rect_in_polygon <- function(rx1, ry1, rx2, ry2) {
    min_x <- min(rx1, rx2); max_x <- max(rx1, rx2)
    min_y <- min(ry1, ry2); max_y <- max(ry1, ry2)
    
    # Get all y-values we need to check (polygon vertices in range + rectangle bounds)
    check_ys <- unique(c(min_y, max_y, all_y[all_y >= min_y & all_y <= max_y]))
    
    for (y in check_ys) {
      range_at_y <- get_x_range_at_y(y)
      if (is.null(range_at_y)) return(FALSE)
      if (min_x < range_at_y$min_x || max_x > range_at_y$max_x) {
        return(FALSE)
      }
    }
    
    TRUE
  }
  
  # Find largest valid rectangle
  max_area <- 0
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      x1 <- coords[i, "x"]; y1 <- coords[i, "y"]
      x2 <- coords[j, "x"]; y2 <- coords[j, "y"]
      
      # Quick area check - skip if can't beat current best
      width <- abs(x2 - x1) + 1
      height <- abs(y2 - y1) + 1
      area <- width * height
      
      if (area <= max_area) next
      
      # Check if rectangle is valid
      if (rect_in_polygon(x1, y1, x2, y2)) {
        max_area <- area
      }
    }
  }
  
  max_area
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
