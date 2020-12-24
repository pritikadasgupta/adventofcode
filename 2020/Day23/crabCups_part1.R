#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#get your puzzle input from https://adventofcode.com/2020/day/23
mydata <- "158937462"
# mydata <- "389125467" #example

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

#Cups
cups_original <- as.numeric(strsplit(mydata,"")[[1]])

#function to loop around and find index
idx <- function(i,n){
  return(1 + (i - 1) %% n)
} 

#cup game
crabCupGame <- function(cups_original,moves){
  n <- length(cups_original)
  j=1
  current_cup <- cups_original[idx(j,n)]
  for(i in 1:moves){
    print(paste("Move",i))
    current_cup <- cups_original[idx(j,n)]
    # The crab picks up the three cups that are immediately clockwise of the current cup.
    # They are removed from the circle; cup spacing is adjusted as necessary to maintain 
    # the circle.
    pickup <- cups_original[idx(seq(j + 1, j + 3),n)]
    cups <- setdiff(cups_original, pickup)
    print("cups:")
    print(cups_original)
    print("pickup:")
    print(pickup)
    # The crab selects a destination cup: the cup with a label equal to the current cup's label 
    # minus one. If this would select one of the cups that was just picked up, 
    # the crab will keep subtracting one until it finds a cup that wasn't just picked up. 
    # If at any point in this process the value goes below the lowest value on any cup's label, 
    # it wraps around to the highest value on any cup's label instead.
    destination <- idx(current_cup - 1:4,n)
    destination <- destination[which.min(destination %in% pickup)]
    print(paste("destination:",destination))
    destination_loc <- which(cups==(destination))
    
    # The crab places the cups it just picked up so that they are immediately clockwise 
    # of the destination cup. They keep the same order as when they were picked up.
    cups <- append(cups,pickup,after=destination_loc)
    
    # The crab selects a new current cup: the cup which is immediately clockwise of 
    # the current cup.
    cups_original <- cups
    j = which(cups ==(current_cup)) + 1
  }
  n <- length(cups_original)
  k <- which(cups_original == 1)
  result <- cups_original[1 + (k + 0:(n - 2)) %% n]
  return(paste(result, collapse = ""))
}

print(paste("Part 1:",crabCupGame(cups_original,100)))

