#!/usr/bin/env Rscript
#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines("2021/Day2/input.txt")
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

submarine <-function(x){
  horizontal <- 0
  depth <- 0
  for(i in 1:length(x)){
    operation <- x[i]
    number <- as.numeric(strsplit(operation, "\\D+")[[1]][-1])
    if(grepl("forward", operation, fixed = TRUE)){
      horizontal <- horizontal + number
    }
    if(grepl("up", operation, fixed = TRUE)){
      depth <- depth - number
    }
    if(grepl("down", operation, fixed = TRUE)){
      depth <- depth + number
    }
  }
  return(horizontal*depth)
}

submarine(mydata)
#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------

submarine2 <-function(x){
  horizontal <- 0
  depth <- 0
  aim <- 0
  for(i in 1:length(x)){
    operation <- x[i]
    number <- as.numeric(strsplit(operation, "\\D+")[[1]][-1])
    if(grepl("forward", operation, fixed = TRUE)){
      horizontal <- horizontal + number
      depth <- depth + (number*aim)
    }
    if(grepl("up", operation, fixed = TRUE)){
      aim <- aim - number
    }
    if(grepl("down", operation, fixed = TRUE)){
      aim <- aim + number
    }
  }
  return(horizontal*depth)
}

submarine2(mydata)





