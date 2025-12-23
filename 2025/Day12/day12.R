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
  while (i <= length(raw_lines) && grepl("^\\d+:", raw_lines[i])) {
    shape_id <- as.integer(gsub(":", "", raw_lines[i]))
    i <- i + 1
    shape_rows <- c()
    while (i <= length(raw_lines) && grepl("^[#.]+$", raw_lines[i])) {
      shape_rows <- c(shape_rows, raw_lines[i])
      i <- i + 1
    }
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
  coords[,1] <- coords[,1] - min(coords[,1])
  coords[,2] <- coords[,2] - min(coords[,2])
  coords
}

# Precompute all unique orientations for a shape
get_all_orientations <- function(shape) {
  orientations <- list()
  seen <- character()
  s <- shape
  
  for (flip in 1:2) {
    for (rot in 1:4) {
      # Normalize to origin
      s[,1] <- s[,1] - min(s[,1])
      s[,2] <- s[,2] - min(s[,2])
      # Sort for consistent key
      ord <- order(s[,1], s[,2])
      s_sorted <- s[ord, , drop = FALSE]
      key <- paste(s_sorted, collapse = ",")
      
      if (!key %in% seen) {
        seen <- c(seen, key)
        orientations[[length(orientations) + 1]] <- s_sorted
      }
      # Rotate 90°: (r,c) -> (c, -r)
      s <- cbind(s[,2], -s[,1])
    }
    # Flip horizontally
    s <- cbind(shape[,1], -shape[,2])
  }
  orientations
}

# Precompute valid placements for each orientation at each starting position
precompute_placements <- function(orientations, height, width) {
  placements <- list()
  
  for (orient in orientations) {
    max_r <- max(orient[,1])
    max_c <- max(orient[,2])
    
    for (start_r in 0:(height - 1 - max_r)) {
      for (start_c in 0:(width - 1 - max_c)) {
        # Convert to grid indices (cells covered)
        cells <- orient
        cells[,1] <- cells[,1] + start_r
        cells[,2] <- cells[,2] + start_c
        # Convert to single indices for faster lookup
        indices <- cells[,1] * width + cells[,2] + 1
        placements[[length(placements) + 1]] <- sort(indices)
      }
    }
  }
  
  # Remove duplicates
  unique(placements)
}

solve_region <- function(shapes, region) {
  width <- region$width
  height <- region$height
  counts <- region$counts
  total_cells <- width * height
  
  # Build list of pieces with precomputed placements
  pieces <- list()
  total_shape_cells <- 0
  
  for (shape_idx in seq_along(counts)) {
    if (counts[shape_idx] > 0) {
      orientations <- get_all_orientations(shapes[[shape_idx]])
      placements <- precompute_placements(orientations, height, width)
      shape_size <- nrow(shapes[[shape_idx]])
      total_shape_cells <- total_shape_cells + counts[shape_idx] * shape_size
      
      for (k in seq_len(counts[shape_idx])) {
        pieces[[length(pieces) + 1]] <- list(
          placements = placements,
          size = shape_size
        )
      }
    }
  }
  
  if (length(pieces) == 0) return(TRUE)
  
  # Early pruning: if total cells needed > available, impossible
  if (total_shape_cells > total_cells) return(FALSE)
  
  # Sort pieces by fewest placements first (most constrained)
  pieces <- pieces[order(sapply(pieces, function(p) length(p$placements)))]
  
  # Grid as logical vector for speed
  grid <- rep(FALSE, total_cells)
  
  # Backtracking WITHOUT first-empty-cell heuristic
  # (shapes don't need to tile perfectly)
  solve_backtrack <- function(piece_idx) {
    if (piece_idx > length(pieces)) return(TRUE)
    
    piece <- pieces[[piece_idx]]
    
    for (placement in piece$placements) {
      # Check if all cells are free
      if (any(grid[placement])) next
      
      # Place piece
      grid[placement] <<- TRUE
      
      if (solve_backtrack(piece_idx + 1)) return(TRUE)
      
      # Remove piece
      grid[placement] <<- FALSE
    }
    
    FALSE
  }
  
  solve_backtrack(1)
}

#------------------------------------------------------------------------------
# Core Logic / Solvers
#------------------------------------------------------------------------------

solve_part1 <- function(dat) {
  sum(sapply(seq_along(dat$regions), function(i) {
    cat("Checking region", i, "of", length(dat$regions), "\n")
    if (solve_region(dat$shapes, dat$regions[[i]])) 1 else 0
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
