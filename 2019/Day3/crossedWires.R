#!/usr/bin/env Rscript

#learning from https://colinfay.me/aoc-2019-03/

#clear workspace
rm(list = ls())

#Libraries

#Load Data
#Use this if you're running from the command line:
# data1 <- readLines(file("stdin)) #read in file

#Use this if you're opening this repo as a R project, using relative paths:
data1 <- scan( "2019/Day3/exercise3.input.txt", what = character(), sep = "\n")
first <- strsplit(data1[1], split = ",")[[1]]
sec <- strsplit(data1[2], split = ",")[[1]] 
# 
# 
# test <- "R8,U5,L5,D3"
# test2 <- "U7,R6,D4,L4"
# data1 <- test
# data2 <- test2


#-------------------------------------------------------------------------
#PART 1
#-------------------------------------------------------------------------

directions <- function(ipt, x, y){
  dir <- substr(ipt, 1, 1)
  how_m <- as.numeric(substr(ipt, 2, nchar(ipt)))
  if (dir == "R"){
    x <- x + how_m
  } else if (dir == "L"){
    x <- x - how_m
  } else if (dir == "U"){
    y <- y + how_m
  } else if (dir == "D"){
    y <- y - how_m
  }
  return(list(x = x, y = y))
}

get_dir <- function(vec){
  out <- data.frame(
    x = 0, 
    y = 0
  )
  for (i in seq_along(vec)){
    y_m_1 <- out$y[nrow(out)]
    x_m_1 <- out$x[nrow(out)]
    res <- directions(vec[i], x = x_m_1, y = y_m_1)
    out %<>% rbind(
      data.frame(
        x = x_m_1:res$x, 
        y = y_m_1:res$y
      )[-1, ]
    )
  }
  out$step <- 1:nrow(out) 
  out
}

out_a <- get_dir(first)
out_b <- get_dir(sec)

#part 1:
res <- merge(out_a, out_b, by = c("x", "y"))
res$path <- abs(res$x) + abs(res$y)
sort(unique(res$path))[2]-sort(unique(res$path))[1]

#part 2:
res$tot_step <- res$step.x + res$step.y
sort(unique(res$tot_step))[2]-sort(unique(res$tot_step))[1]
