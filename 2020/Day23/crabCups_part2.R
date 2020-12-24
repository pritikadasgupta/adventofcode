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
sorted_cups_original <- sort(cups_original,decreasing=TRUE)
sorted_cups_original <- paste(sorted_cups_original,collapse="")
#function to loop around and find index

#heavy learning and inspo by https://github.com/rundel/advent_of_code_2020/blob/master/day23/day23.R
to_vals = function(x, n = length(x)) {
  j = 1
  for(i in seq_len(n)) {
    cat(j, " ", sep="")
    j = x[j]
  }
  cat("\n")
}

#cup game
crabCupGame <- function(cups_original,moves,ncups){
  n_ <- length(cups_original)
  j=1
  
  if (n_ < ncups){
    cups_original = c(cups_original, (max(cups_original)+1):ncups)
  }
    
  
  new = lead(cups_original)
  new[length(cups_original)] = cups_original[1]
  
  d = seq_along(cups_original)
  d[cups_original] = new
  
  n = length(d)
  
  cur = cups_original[1]
  
  for(i in seq_len(moves)) {
    #cat("Move ", i, "\n")
    #cat("Cur : ",  cur, "\n")
    #cat("Cups: ")
    #to_cups_original(d)
    
    sel1 = d[cur]
    sel2 = d[sel1]
    sel3 = d[sel2]
    
    #cat("pick up: ", sel1, sel2, sel3, "\n")
    
    d[cur] = d[sel3]
    
    dest = cur-1
    while(dest %in% c(0, sel1, sel2, sel3)) {
      if (dest == 0)
        dest = n
      else
        dest = dest - 1
    }
    
    #cat("Dest: ", dest, "\n\n")
    
    end = d[dest]
    d[dest] = sel1
    d[sel3] = end
    
    cur = d[cur]
    #to_cups_original(d)
  }
  
  c(d[1], d[ d[1] ])
  
}




result <- crabCupGame(cups_original,moves = 10000000,ncups = 1000000)

# two cups that will end up immediately clockwise of cup 1

print(paste("Part 2:",prod(result)))


