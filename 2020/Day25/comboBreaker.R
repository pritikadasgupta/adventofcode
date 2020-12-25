#!/usr/bin/env Rscript
#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines("2020/Day25/input.txt")
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
a <- as.integer(mydata[1])
b <- as.integer(mydata[2])

val <- 1L
result <- 0
while(val != a){
  val <- val*7L
  val <- val %% 20201227L
  result <- result + 1L
}


val <- 1
for(i in 1:result){
  val <- val*b
  val <- val %% 20201227L
}

print(val)
