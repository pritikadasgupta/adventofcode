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
        coords <- rbind(coords, c(r - 1L, c - 1L))
      }
    }
  }
  coords[,1] <- coords[,1] - min(coords[,1])
  coords[,2] <- coords[,2] - min(coords[,2])
  storage.mode(coords) <- "integer"
  coords
}

parse_input <- function(raw_lines) {
  shapes <- list()
  regions <- list()
  i <- 1L
  
  while (i <= length(raw_lines) && grepl("^\\d+:$", raw_lines[i])) {
    shape_id <- as.integer(gsub(":", "", raw_lines[i]))
    i <- i + 1L
    shape_rows <- character()
    while (i <= length(raw_lines) && grepl("^[#.]+$", raw_lines[i])) {
      shape_rows <- c(shape_rows, raw_lines[i])
      i <- i + 1L
    }
    shapes[[shape_id + 1L]] <- parse_shape(shape_rows)
    while (i <= length(raw_lines) && raw_lines[i] == "") i <- i + 1L
  }
  
  while (i <= length(raw_lines)) {
    if (raw_lines[i] != "") {
      parts <- strsplit(raw_lines[i], ": ")[[1]]
      dims <- as.integer(strsplit(parts[1], "x")[[1]])
      counts <- as.integer(strsplit(parts[2], " ")[[1]])
      regions[[length(regions) + 1L]] <- list(
        width = dims[1], height = dims[2], counts = counts
      )
    }
    i <- i + 1L
  }
  
  list(shapes = shapes, regions = regions)
}

#------------------------------------------------------------------------------
# Shape Orientation and Placement (Optimized!!)
#------------------------------------------------------------------------------

get_all_orientations <- function(shape) {
  orientations <- list()
  seen <- new.env(hash = TRUE)  # Hash-based lookup for speed
  s <- shape
  
  for (flip in 1:2) {
    for (rot in 1:4) {
      s[,1] <- s[,1] - min(s[,1])
      s[,2] <- s[,2] - min(s[,2])
      ord <- order(s[,1], s[,2])
      s_sorted <- s[ord, , drop = FALSE]
      key <- paste(c(s_sorted), collapse = ",")
      
      if (!exists(key, envir = seen, inherits = FALSE)) {
        assign(key, TRUE, envir = seen)
        orientations[[length(orientations) + 1L]] <- s_sorted
      }
      s <- cbind(s[,2], -s[,1])
    }
    s <- cbind(shape[,1], -shape[,2])
  }
  orientations
}

precompute_placements <- function(orientations, height, width) {
  placements <- vector("list", length(orientations) * height * width)
  idx <- 0L
  
  for (orient in orientations) {
    max_r <- max(orient[,1])
    max_c <- max(orient[,2])
    
    for (start_r in 0:(height - 1L - max_r)) {
      for (start_c in 0:(width - 1L - max_c)) {
        indices <- (orient[,1] + start_r) * width + (orient[,2] + start_c) + 1L
        idx <- idx + 1L
        placements[[idx]] <- sort.int(indices, method = "radix")
      }
    }
  }
  
  placements <- placements[seq_len(idx)]
  
  # Deduplicate using hash
  seen <- new.env(hash = TRUE)
  unique_placements <- vector("list", idx)
  unique_idx <- 0L
  
  for (p in placements) {
    key <- paste(p, collapse = ",")
    if (!exists(key, envir = seen, inherits = FALSE)) {
      assign(key, TRUE, envir = seen)
      unique_idx <- unique_idx + 1L
      unique_placements[[unique_idx]] <- p
    }
  }
  
  unique_placements[seq_len(unique_idx)]
}

#------------------------------------------------------------------------------
# Core Solver (Optimized with symmetry breaking and pruning!!!!)
#------------------------------------------------------------------------------

solve_region <- function(shapes, region) {
  width <- region$width
  height <- region$height
  counts <- region$counts
  total_cells <- width * height
  
  # Build piece groups (pieces of the same shape)
  piece_groups <- list()
  total_shape_cells <- 0L
  
  for (shape_idx in seq_along(counts)) {
    if (counts[shape_idx] > 0L) {
      orientations <- get_all_orientations(shapes[[shape_idx]])
      placements <- precompute_placements(orientations, height, width)
      
      # Early exit: if any required shape has no valid placements
      if (length(placements) == 0L) return(FALSE)
      
      shape_size <- nrow(shapes[[shape_idx]])
      total_shape_cells <- total_shape_cells + counts[shape_idx] * shape_size
      
      piece_groups[[length(piece_groups) + 1L]] <- list(
        placements = placements,
        count = counts[shape_idx],
        size = shape_size,
        shape_idx = shape_idx
      )
    }
  }
  
  if (length(piece_groups) == 0L) return(TRUE)
  if (total_shape_cells > total_cells) return(FALSE)
  
  # Sort groups: largest pieces first, then by constraint (fewest placements)
  piece_groups <- piece_groups[order(
    -sapply(piece_groups, `[[`, "size"),
    sapply(piece_groups, function(g) length(g$placements))
  )]
  
  # Flatten into individual pieces with symmetry breaking info
  pieces <- list()
  for (group in piece_groups) {
    for (k in seq_len(group$count)) {
      pieces[[length(pieces) + 1L]] <- list(
        placements = group$placements,
        size = group$size,
        shape_idx = group$shape_idx,
        instance = k,  # Which instance of this shape
        group_count = group$count
      )
    }
  }
  
  # Grid as integer vector (0 = empty, 1 = filled)
  grid <- integer(total_cells)
  
  # Track minimum placement index for symmetry breaking within same shape
  last_placement_by_shape <- new.env(hash = TRUE)
  
  # Backtracking with optimizations
  solve_backtrack <- function(piece_idx) {
    if (piece_idx > length(pieces)) return(TRUE)
    
    piece <- pieces[[piece_idx]]
    placements <- piece$placements
    shape_key <- as.character(piece$shape_idx)
    
    # Symmetry breaking: for same-shape pieces, enforce ordering
    min_placement_idx <- 1L
    if (piece$instance > 1L && exists(shape_key, envir = last_placement_by_shape, inherits = FALSE)) {
      min_placement_idx <- get(shape_key, envir = last_placement_by_shape) + 1L
    }
    
    n_placements <- length(placements)
    if (min_placement_idx > n_placements) return(FALSE)
    
    for (p_idx in min_placement_idx:n_placements) {
      placement <- placements[[p_idx]]
      
      # Fast collision check using sum
      if (sum(grid[placement]) > 0L) next
      
      # Place piece
      grid[placement] <<- 1L
      
      # Update symmetry tracking
      old_val <- if (exists(shape_key, envir = last_placement_by_shape, inherits = FALSE)) {
        get(shape_key, envir = last_placement_by_shape)
      } else {
        NA_integer_
      }
      assign(shape_key, p_idx, envir = last_placement_by_shape)
      
      if (solve_backtrack(piece_idx + 1L)) return(TRUE)
      
      # Restore state
      grid[placement] <<- 0L
      if (is.na(old_val)) {
        rm(list = shape_key, envir = last_placement_by_shape)
      } else {
        assign(shape_key, old_val, envir = last_placement_by_shape)
      }
    }
    
    FALSE
  }
  
  solve_backtrack(1L)
}

#------------------------------------------------------------------------------
# Solvers
#------------------------------------------------------------------------------

solve_part1 <- function(dat) {
  n_regions <- length(dat$regions)
  results <- integer(n_regions)
  
  for (i in seq_len(n_regions)) {
    results[i] <- if (solve_region(dat$shapes, dat$regions[[i]])) 1L else 0L
  }
  
  sum(results)
}

solve_part2 <- function(dat) {
  message("Part 2: Click 'Finish Decorating the North Pole' on adventofcode.com")
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