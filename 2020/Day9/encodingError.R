#!/usr/bin/env Rscript

#clear workspace
rm(list = ls())

#Libraries
# library(tidyverse)
# library(igraph)

#Set working directory and load in data
#Use this if you're running from the command line:
# data1 <- as.numeric(readLines(file("stdin")))

#Use this if you're opening this repo as a R project, using relative paths:
data1 <- as.numeric(readLines("2020/Day9/input"))

#Functions
#-------------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))


#-------------------------------------------------------------------------
#PART 1
#-------------------------------------------------------------------------
# The first step of attacking the weakness in the XMAS data is to find the 
# first number in the list (after the preamble) 
# which is not the sum of two of the 25 numbers before it. 
# What is the first number that does not have this property?


preamble_length <- 25
part1_answer = NA
for (i in (preamble_length+1):length(data1)) {
  current_num <- data1[i]
  current_preamble <- data1[(i-preamble_length):(i-1)]
  
  check <- vector()
  for(j in current_preamble){
    check_num <- as.numeric(current_num)-as.numeric(j)
    if(check_num %in% current_preamble){
      check <- append(check,as.numeric(current_num))
    }else{
      check <- append(check,0)
    }
  }
  check <- unique(check)
  check1 <- check[!is.na(check)]
  check_num2 <- sum(check1)
  
  if(current_num!=check_num2){
    part1_answer = current_num
    part1_preamble = current_preamble
  }

  
}

print(paste("Part 1:",part1_answer))


#-------------------------------------------------------------------------
#PART 2
#-------------------------------------------------------------------------

#contiguous set of k
for(k in 2:length(data1)){
  for(j in 1:(length(data1)-k)){
    cont_set <- data1[c(j:(j + (k-1) ))]
    sum_to_check <- sum(cont_set)
    sum_to_answer <- min(cont_set)+max(cont_set)
    if(sum_to_check == part1_answer){
      print(paste("Part 2:",sum_to_answer))
    }
  }
}



