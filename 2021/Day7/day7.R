#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- as.numeric(strsplit(readLines("2021/Day7/input.txt"),",",fixed=TRUE)[[1]])
example <- as.numeric(strsplit(readLines("2021/Day7/example.txt"),",",fixed=TRUE)[[1]])

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

crabs <- function(x){
  len <- max(x)
  cost <- rep(0,len)
  for(pos in 1:len){
    for(i in 1:length(x)){
      cost[pos] <- cost[pos] + abs(x[i]-pos)
    }
  }
  return(min(cost))
}

crabs_optimized <- function(x){
  return(sum(abs(x-median(x))))
}

crabs(example)
crabs(mydata) #approach 1
crabs_optimized(mydata) #approach 2

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------

crabs2 <- function(x){
  len <- max(x)
  cost <- rep(0,len)
  for(pos in 1:len){
    for(i in 1:length(x)){
      difference <- abs(x[i]-pos)
      cost[pos] <- cost[pos] + (difference*(1+difference)/2)
    }
  }
  return(min(cost))
}

crabs2(example)
crabs2(mydata) #approach 1

