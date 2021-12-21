#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
mydata <- readLines("2021/Day21/input.txt")
example <- readLines("2021/Day21/example.txt")
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

part1 <- function(x){
  x <- unlist(regmatches(x, gregexpr('\\(?[0-9,.]+', x)))
  player1_start <- as.numeric(x[2])
  player2_start <- as.numeric(x[4])
  die <- 1:100
  player1_score <- 0
  player2_score <- 0
  die_roll <- 0
  repeat{
    player1_score_c <- die[1]+die[2]+die[3]
    die_roll <- die_roll + 3
    player1_start <- player1_start + player1_score_c
    
    if(player1_start > 10){
      player1_start <- (player1_start-1) %% 10 + 1
      player1_score <- player1_score + player1_start
    }else if (player1_start <= 10){
      player1_start <- player1_start
      player1_score <- player1_score + player1_start
    }
    
    if(player1_score >=1000 ) break
    
    player2_score_c <- die[4]+die[5]+die[6]
    die_roll <- die_roll + 3
    player2_start <- player2_start + player2_score_c
    if(player2_start > 10){
      player2_start <- (player2_start-1) %% 10 + 1
      player2_score <- player2_score + player2_start
    }else if (player2_start <= 10){
      player2_start <- player2_start
      player2_score <- player2_score + player2_start
    }
    
    if(player2_score >=1000 ) break
    
    die <- c(die[-c(1:6)],die[c(1:6)])
  }
  
  return(c(player1_score,player2_score,die_roll))
}

result <- part1(mydata)
min(result[1],result[2])*result[3]

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
library(tidyverse)
# heavy inspiration and learning about tidyverse operations for this type of problem from:
# https://github.com/czeildi/advent-of-code/blob/main/solutions_2021/day21.R
# thank you for posting!

x <- mydata
x <- unlist(regmatches(x, gregexpr('\\(?[0-9,.]+', x)))
player1_start <- as.numeric(x[2])
player2_start <- as.numeric(x[4])

# transition matrix
die_sum_frequencies <- cross3(1:3, 1:3, 1:3) %>% 
  map_int(~sum(unlist(.))) %>% 
  table() %>% 
  as.list()

n_possibility_in_single_roll <- function(from, to) {
  score <- to - from
  if (score < 0) score <- score + 10
  if (score < 3) return(0)
  die_sum_frequencies[[as.character(score)]]
}

transitions <- crossing(from = 1:10, to = 1:10) %>% 
  rowwise() %>% 
  mutate(n_possibility = n_possibility_in_single_roll(from, to))

winning_score <- 21
winning_universes <- c(0, 0)

roll_die <- function(state) {
  state %>% 
    inner_join(transitions, by = "from") %>% 
    mutate(n = n * n_possibility, score = score + to) %>% 
    filter(n != 0) %>% 
    select(from = to, n, score) %>% 
    group_by(from, score) %>% 
    summarize(n = sum(n), .groups = "drop")
}

universes <- list(
  tibble(from = player1_start, n = 1, score = 0),
  tibble(from = player2_start, n = 1, score = 0)
)

roll_for_player_p2 <- function(player_idx) {
  after_roll <- roll_die(universes[[player_idx]])
  
  win_for_player <- after_roll %>% 
    filter(score >= winning_score) %>% 
    pull(n) %>% 
    sum()
  
  loses_for_other_player <- sum(universes[[3 - player_idx]]$n)
  winning_universes[player_idx] <<- winning_universes[player_idx] + win_for_player * loses_for_other_player
  
  universes[[player_idx]] <<- after_roll %>% 
    filter(score < winning_score)
}

while(nrow(universes[[1]]) > 0 && nrow(universes[[2]]) > 0) {
  roll_for_player_p2(1)
  if (nrow(universes[[1]]) > 0 && nrow(universes[[2]]) > 0) {
    roll_for_player_p2(2)
  }
}

options(scipen = 99) #necessary for big numbers, prevent scientific notation

max(winning_universes)
