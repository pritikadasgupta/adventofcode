#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- as.numeric(strsplit(readLines("2021/Day6/input.txt"),",",fixed=TRUE)[[1]])
example <- as.numeric(strsplit(readLines("2021/Day6/example.txt"),",",fixed=TRUE)[[1]])

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
lanternfish <- function(x,days){
  for(j in 1:days){
    for(i in 1:length(x)){
      if(x[i]==0){
        x <- append(x,8)
        x[i] <- 6
      }else{
        x[i] <- x[i]-1
      }
    }
  }
  return(length(x))
}


lanternfish(example,80)
lanternfish(mydata,80) #took a long time, don't recommend

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------

lanternfish2 <- function(x,days){
  fish <- rep(0,9)
  for(i in 0:8){
    if(i %in% names(table(x))){
      fish[i+1] <- table(x)[names(table(x)) == (i)]
    }else{
      fish[i+1] <-0
    }
  }
  
  for(j in 1:days) {
    fish[8] = fish[8] + fish[1]
    fish = c(fish[-1], fish[1])
  }
  return(sum(fish))
}
lanternfish2(example,256)
lanternfish2(mydata,256)
