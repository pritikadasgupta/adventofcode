#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries
library(tidyverse)
library(tidyr)
library(dplyr)

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------

#Use this if you're running from the command line:
# mydata <- strsplit(readLines(file("stdin"))," ")

#Use this if you're opening this repo as a R project, using relative paths:
# mydata <- strsplit(readLines('2020/Day19/input_test1.txt')," ")
mydata <- readLines('2020/Day20/input_test1.txt')

#------------------------------------------------------------------------------------------
#Functions
#------------------------------------------------------------------------------------------
# 90 degree clockwise rotation of matrix (stackoverflow)
rotate <- function(x) t(apply(x, 2, rev))


#following two functions are from https://github.com/rundel/advent_of_code_2020/blob/master/day20/day20.R
parse = function(file, n=10) {
  read_file(file) %>%
    str_trim() %>%
    str_split("\n\n") %>%
    .[[1]] %>%
    tibble::tibble(d = .) %>%
    separate(d, c("id", "tile"), sep = ":\n") %>%
    mutate(
      id = str_remove(id, "Tile ") %>% as.numeric(),
      tile = map(tile, ~str_split(.x, "\n", simplify=TRUE)) %>%
        map(~str_split(.x, "", simplify=TRUE)),
      edges = map(
        tile,
        ~ list(L = .x[,1], R = .x[,n], T = .x[1,], B = .x[n,],
               Lr = rev(.x[,1]), Rr = rev(.x[,n]),
               Tr = rev(.x[1,]), Br = rev(.x[n,]))
      ),
      edges = map(edges, ~ map_chr(.x, paste, collapse=""))
    )
}

get_nbs = function(d) {
  d %>%
    select(-tile) %>%
    expand_grid(x=., y=.) %>%
    do.call(cbind, .) %>%
    setNames(c("x_id", "x_edges", "y_id", "y_edges")) %>%
    filter(x_id != y_id) %>%
    mutate(
      matches = map2(x_edges, y_edges, ~ .x %in% .y),
      n_matches = map_int(matches, sum),
    ) %>%
    filter(n_matches != 0) %>%
    group_by(x_id) %>%
    summarize(
      n = n(),
      matches = list(c(matches)),
      nbs = list(c(y_id)),
      .groups = "drop"
    ) %>%
    rename(id = x_id)
}

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

d = parse(mydata)

idx <- d %>%
  get_nbs() %>%
  filter(n == 2) %>%
  pull(id)

print(paste(prod(idx)))
