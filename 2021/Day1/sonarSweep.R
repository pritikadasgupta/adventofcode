#!/usr/bin/env Rscript
#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- as.numeric(readLines("2021/Day1/input.txt"))
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
mydata1 <- c(199,200,208,210,200,207,240,269,260,263)

# my initial solution
larger1 <- function(x){
  tracker <- c(NA)
  for(i in 2:length(x)){
    if(x[i]>x[i-1]){
      tracker <- append(tracker,"increased")
    }else if(x[i]==x[i-1]){
      tracker <- append(tracker,"no change")
    }else{
      tracker <- append(tracker,"decreased")
    }
  }
  return(table(tracker))
}
larger1(mydata1)
larger1(mydata)

#another solution: one line
larger1_fast <- function(x){
  return(sum(diff(x)>0))
}

larger1_fast(mydata)
#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
# my initial solution
larger2 <- function(x){
  tracker <- c(NA)
  for(i in 3:(length(x)-1)){
    sum1 <- sum(x[i],x[i-1],x[i-2])
    sum2 <- sum(x[i+1],x[i],x[i-1])
    if(sum2 >sum1){
      tracker <- append(tracker,"increased")
    }else if(sum1==sum2){
      tracker <- append(tracker,"no change")
    }else{
      tracker <- append(tracker,"decreased")
    }
  }
  return(table(tracker))
}

#another solution: one line
larger2_fast <- function(x){
  return(sum(diff(x,lag=3)>0))
}

larger2_fast(mydata)

larger2(mydata1)
larger2(mydata)
