#!/usr/bin/env Rscript

###############################################################################
# Script:    day08.R
# Project:   Advent of Code (AoC) 2025
# Author:    GRID
# Created:   12-01-2025 (MM-DD-YYYY)
# Purpose:   Solve AoC 2025 Day 8 – Playground
# Link:      https://adventofcode.com/2025/day/8
#
# Usage (CLI, from repo root):
#   Rscript 2025/Day8/day08.R 2025/Day8/input.txt
#
# Usage (interactive, from repo root):
#   source("2025/Day8/day08.R")
#   main("2025/Day8/input.txt")
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
  # Parse "X,Y,Z" lines into a matrix
  coords <- do.call(rbind, lapply(raw_lines, function(line) {
    as.numeric(strsplit(line, ",")[[1]])
  }))
  colnames(coords) <- c("x", "y", "z")
  coords
}

#------------------------------------------------------------------------------
# Core Logic / Solvers
#------------------------------------------------------------------------------

solve_part1 <- function(coords, n_connections = 1000) { #union find problem?
  
  n <- nrow(coords)
  
  # Compute all pairwise distances
  pairs <- list()
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      dist <- sqrt(sum((coords[i,] - coords[j,])^2))
      pairs[[length(pairs) + 1]] <- list(i = i, j = j, dist = dist)
    }
  }
  
  # Sort by distance
  dists <- sapply(pairs, function(p) p$dist)
  pairs <- pairs[order(dists)]
  
  # Union-Find data structure
  parent <- 1:n
  rank <- rep(0, n)
  
  find <- function(x) {
    if (parent[x] != x) {
      parent[x] <<- find(parent[x])
    }
    parent[x]
  }
  
  union <- function(x, y) {
    px <- find(x)
    py <- find(y)
    if (px == py) return(FALSE)
    
    if (rank[px] < rank[py]) {
      parent[px] <<- py
    } else if (rank[px] > rank[py]) {
      parent[py] <<- px
    } else {
      parent[py] <<- px
      rank[px] <<- rank[px] + 1
    }
    TRUE
  }
  
  # Process the n_connections closest PAIRS (not n_connections new connections)
  for (idx in 1:min(n_connections, length(pairs))) {
    p <- pairs[[idx]]
    union(p$i, p$j)  # May or may not create new connection
  }
  
  # Count circuit sizes
  for (i in 1:n) find(i)
  circuit_sizes <- table(sapply(1:n, find))
  
  # Get 3 largest and multiply
  top3 <- sort(as.numeric(circuit_sizes), decreasing = TRUE)[1:3]
  prod(top3)
}

solve_part2 <- function(coords, n_connections = 1000) {
  n <- nrow(coords)
  
  
  # Compute all pairwise distances
  pairs <- list()
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      dist <- sqrt(sum((coords[i,] - coords[j,])^2))
      pairs[[length(pairs) + 1]] <- list(i = i, j = j, dist = dist)
    }
  }
  
  # Sort by distance
  dists <- sapply(pairs, function(p) p$dist)
  pairs <- pairs[order(dists)]
  
  # Union-Find data structure
  parent <- 1:n
  rank <- rep(0, n)
  num_circuits <- n  # Start with n individual circuits
  
  find <- function(x) {
    if (parent[x] != x) {
      parent[x] <<- find(parent[x])
    }
    parent[x]
  }
  
  union <- function(x, y) {
    px <- find(x)
    py <- find(y)
    if (px == py) return(FALSE)
    
    if (rank[px] < rank[py]) {
      parent[px] <<- py
    } else if (rank[px] > rank[py]) {
      parent[py] <<- px
    } else {
      parent[py] <<- px
      rank[px] <<- rank[px] + 1
    }
    num_circuits <<- num_circuits - 1  # Merged two circuits into one
    TRUE
  }
  
  # Process pairs until all in one circuit
  for (idx in seq_along(pairs)) {
    p <- pairs[[idx]]
    if (union(p$i, p$j)) {
      # Check if we now have just one circuit
      if (num_circuits == 1) {
        # Return product of X coordinates
        return(coords[p$i, "x"] * coords[p$j, "x"])
      }
    }
  }
  
  NA_real_  # never reach here
}

#------------------------------------------------------------------------------
# Quick checks / examples
#------------------------------------------------------------------------------

run_checks <- function() {
  example_raw <- read_lines(here("2025", "Day8", "example.txt"))
  example_dat <- parse_input(example_raw)

  stopifnot(
    solve_part1(example_dat, n_connections = 10) == 40,  # example uses 10 (part 1 uses 1000)
    solve_part2(example_dat, n_connections = 10) == 25272
  )
  invisible(TRUE)
}

#------------------------------------------------------------------------------
# Main / Orchestration
#------------------------------------------------------------------------------

main <- function(
    input_path = here("2025", "Day8", "input.txt"),
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
  default_input <- here("2025", "Day8", "input.txt")
  input_arg <- if (length(args) >= 1L) args[[1]] else default_input
  run_checks()
  main(input_path = input_arg, verbose = TRUE)
}
