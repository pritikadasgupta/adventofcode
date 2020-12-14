#!/usr/bin/env Rscript
#clear workspace
rm(list = ls())
#Libraries
# library(tidyverse)
# library(igraph)

#Load Data
#Use this if you're running from the command line:
# mydata <- as.numeric(file("stdin")) #read in file

#Use this if you're opening this repo as a R project, using relative paths:
data1 <- as.numeric(readLines("2019/Day1/exercise1.input.txt")) #read in file

#Functions
#-------------------------------------------------------------------------

rocketEquation <- function(x){
   x1 <- as.integer(x/3)-2
  return(x1)
}


#-------------------------------------------------------------------------
#PART 1
#-------------------------------------------------------------------------

sum <-0
for(j in data1){
  sum <- sum + rocketEquation(j)
}

print(paste("Part 1:",sum))

#-------------------------------------------------------------------------
#PART 2
#-------------------------------------------------------------------------


sum <-0
sum_vector <- vector()
for(j in data1){
  fuel <- rocketEquation(j)
  while(fuel > 0){
    sum <- sum + fuel
    fuel <- rocketEquation(fuel)
  }
}

print(paste("Part 2:",sum))

