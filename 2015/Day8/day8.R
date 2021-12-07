#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines("2015/Day8/input.txt")
example <- readLines("2015/Day8/example.txt")
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

characters <- function(x){
  sum1 <- 0
  sum2 <- 0
  for(i in 1:length(x)){
    sum1 <- sum1 + nchar(x[i])
    sum2 <- sum2 + nchar(eval(parse(text = x[i])), type = "bytes")
  }
  return(sum1-sum2)
}
characters(example)
characters(mydata)

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
library(stringi)
characters2 <- function(x){
  sum1 <- 0
  sum3 <- 0
  for(i in 1:length(x)){
    sum1 <- sum1 + nchar(x[i])
    sum3 <- sum3 + nchar(stri_escape_unicode(x[i]))+2
  }
  return(sum3-sum1)
}
characters2(example)
characters2(mydata)
