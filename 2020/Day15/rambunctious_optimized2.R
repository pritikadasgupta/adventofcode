#!/usr/bin/Rscript

#these solns are copied and pasted from https://selbydavid.com/2020/12/06/advent-2020/
#David Selby


#clear workspace
rm(list = ls())

#data
# input_test <- "stdin"
input_test <- "1,20,11,6,12,0"
# input_test <- "0,3,6"
input_test_numeric <- as.numeric(strsplit(input_test,",")[[1]])

#functions:
memory_game <- function(n, start) {
  nstart <- length(start)
  spoken <- integer(10)
  spoken[1:nstart] <- start
  for (i in nstart:(n-1)) {
    before <- which(spoken[1:(i-1)] == spoken[i])
    if (!length(before)) {
      spoken[i+1] <- 0
    } else spoken[i+1] <- i - tail(before, 1)
  }
  spoken[n]
}


memory_game2 <- function(n, start) {
  nstart <- length(start)
  spoken <- start[-nstart]
  when <- seq_along(spoken)
  current <- start[nstart]
  for (i in nstart:(n-1)) {
    if (!current %in% spoken) {
      next_number <- 0
      spoken <- c(spoken, current)
      when <- c(when, i)
    } else {
      next_number <- i - when[spoken == current]
      when[spoken == current] <- i
    }
    current <- next_number
  }
  current
}

memory_game3 <- function(n, start) {
  nstart <- length(start)
  spoken <- numeric(max(n, start) + 1)
  spoken[start[-nstart] + 1] <- seq_len(nstart - 1)
  current <- start[nstart]
  for (i in nstart:(n-1)) {
    next_number <- (spoken[current + 1] > 0) * i - spoken[current + 1]
    spoken[current + 1] <- i
    current <- next_number
  }
  current
}

# part1
memory_game3(2020, input_test_numeric)

# part2
memory_game3(30000000, input_test_numeric)

