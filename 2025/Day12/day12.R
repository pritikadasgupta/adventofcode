#!/usr/bin/env Rscript

###############################################################################
# Script:    day12.R
# Project:   Advent of Code (AoC) 2025
# Author:    GRID
# Created:   12-01-2025 (MM-DD-YYYY)
# Purpose:   Solve AoC 2025 Day 12 – Christmas Tree Farm
# Link:      https://adventofcode.com/2025/day/12
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
# Helpers
#------------------------------------------------------------------------------

log_info <- function(..., .time = TRUE) {
  msg <- paste0(...)
  if (.time) {
    ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    msg <- sprintf("[%s] %s", ts, msg)
  }
  message(msg)
}

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
# Parsing
#------------------------------------------------------------------------------

parse_shape <- function(rows) {
  coords <- NULL
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

parse_input <- function(raw_lines) {
  shapes <- list()
  regions <- list()
  
  i <- 1
  
  # Parse shapes (format: "N:" followed by lines of "#" and ".")
  while (i <= length(raw_lines) && grepl("^\\d+:$", raw_lines[i])) {
    shape_id <- as.integer(gsub(":", "", raw_lines[i]))
    i <- i + 1
    shape_rows <- c()
    while (i <= length(raw_lines) && grepl("^[#.]+$", raw_lines[i])) {
      shape_rows <- c(shape_rows, raw_lines[i])
      i <- i + 1
    }
    shapes[[shape_id + 1]] <- parse_shape(shape_rows)
    # Skip blank lines
    while (i <= length(raw_lines) && raw_lines[i] == "") i <- i + 1
  }
  
  # Parse regions (format: "WxH: c0 c1 c2 ...")
  while (i <= length(raw_lines)) {
    if (raw_lines[i] != "") {
      parts <- strsplit(raw_lines[i], ": ")[[1]]
      dims <- as.integer(strsplit(parts[1], "x")[[1]])
      counts <- as.integer(strsplit(parts[2], " ")[[1]])
      regions[[length(regions) + 1]] <- list(
        width = dims[1], 
        height = dims[2], 
        counts = counts
      )
    }
    i <- i + 1
  }
  
  list(shapes = shapes, regions = regions)
}

#------------------------------------------------------------------------------
# Shape Orientation and Placement
#------------------------------------------------------------------------------

get_all_orientations <- function(shape) {
  orientations <- list()
  seen <- character()
  s <- shape
  
  for (flip in 1:2) {
    for (rot in 1:4) {
      # Normalize to origin
      s[,1] <- s[,1] - min(s[,1])
      s[,2] <- s[,2] - min(s[,2])
      
      # Create canonical key for deduplication
      ord <- order(s[,1], s[,2])
      s_sorted <- s[ord, , drop = FALSE]
      key <- paste(s_sorted, collapse = ",")
      
      if (!key %in% seen) {
        seen <- c(seen, key)
        orientations[[length(orientations) + 1]] <- s_sorted
      }
      
      # Rotate 90° clockwise: (r, c) -> (c, -r)
      s <- cbind(s[,2], -s[,1])
    }
    # Flip horizontally: (r, c) -> (r, -c)
    s <- cbind(shape[,1], -shape[,2])
  }
  
  orientations
}

precompute_placements <- function(orientations, height, width) {
  placements <- list()
  
  for (orient in orientations) {
    max_r <- max(orient[,1])
    max_c <- max(orient[,2])
    
    # Valid starting positions
    for (start_r in 0:(height - 1 - max_r)) {
      for (start_c in 0:(width - 1 - max_c)) {
        cells <- orient
        cells[,1] <- cells[,1] + start_r
        cells[,2] <- cells[,2] + start_c
        # Convert to linear indices for fast grid lookup
        indices <- cells[,1] * width + cells[,2] + 1
        placements[[length(placements) + 1]] <- sort(indices)
      }
    }
  }
  
  # Remove duplicate placements
  unique(placements)
}

#------------------------------------------------------------------------------
# Core Solver
#------------------------------------------------------------------------------

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
      
      # Add one entry per piece instance
      for (k in seq_len(counts[shape_idx])) {
        pieces[[length(pieces) + 1]] <- list(
          placements = placements,
          size = shape_size
        )
      }
    }
  }
  
  # No pieces to place
  if (length(pieces) == 0) return(TRUE)
  
  # Early pruning: total cells needed exceeds available space
  if (total_shape_cells > total_cells) return(FALSE)
  
  # Sort pieces by constraint level (fewest placements first)
  pieces <- pieces[order(sapply(pieces, function(p) length(p$placements)))]
  
  # Grid represented as logical vector
  grid <- rep(FALSE, total_cells)
  
  # Backtracking search
  solve_backtrack <- function(piece_idx) {
    if (piece_idx > length(pieces)) return(TRUE)
    
    piece <- pieces[[piece_idx]]
    
    for (placement in piece$placements) {
      # Check if all cells are free
      if (any(grid[placement])) next
      
      # Place piece
      grid[placement] <<- TRUE
      
      if (solve_backtrack(piece_idx + 1)) return(TRUE)
      
      # Remove piece (backtrack)
      grid[placement] <<- FALSE
    }
    
    FALSE
  }
  
  solve_backtrack(1)
}

#------------------------------------------------------------------------------
# Part 1 and Part 2 Solvers
#------------------------------------------------------------------------------

solve_part1 <- function(dat) {
  results <- sapply(seq_along(dat$regions), function(i) {
    if (solve_region(dat$shapes, dat$regions[[i]])) 1 else 0
  })
  sum(results)
}

solve_part2 <- function(dat) {
  NA_real_
}

#------------------------------------------------------------------------------
# Checks and Main
#------------------------------------------------------------------------------

run_checks <- function() {
  example_raw <- read_lines(here("2025", "Day12", "example.txt"))
  example_dat <- parse_input(example_raw)
  
  stopifnot(solve_part1(example_dat) == 2)
  invisible(TRUE)
}

main <- function(input_path = here("2025", "Day12", "input.txt"), verbose = TRUE) {
  if (verbose) log_info("Reading input from: ", input_path)
  raw <- read_lines(input_path)
  
  if (verbose) log_info("Parsing input...")
  dat <- parse_input(raw)
  
  if (verbose) log_info("Solving Part 1...")
  ans1 <- solve_part1(dat)
  
  if (verbose) log_info("Solving Part 2...")
  ans2 <- solve_part2(dat)
  
  if (verbose) {
    log_info("Part 1 result: ", ans1)
    log_info("Part 2 result: ", ans2)
  }
  
  invisible(list(part1 = ans1, part2 = ans2))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  default_input <- here("2025", "Day12", "input.txt")
  input_arg <- if (length(args) >= 1L) args[[1]] else default_input
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}