#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines("2021/Day14/input.txt")
example <- readLines("2021/Day14/example.txt")

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

polymer_growth <- function(x,steps){
  polymer_template <- strsplit(x[1],"")[[1]]
  instructions <- x[3:length(x)]
  x1 <- c()
  x2 <- c()
  for(i in 1:length(instructions)){
    y <- strsplit(instructions[i],"")[[1]]
    x1 <- c(x1,paste0(c(y[1],y[2]),collapse=""))
    x2 <- c(x2,paste0(c(y[length(y)]),collapse=""))
  }
  df <- as.data.frame(cbind(x1,x2))
  
  
  
  for(step in 1:steps){
    idx <- 1
    while(idx < length(polymer_template)){
      pair <- paste0(c(polymer_template[idx],polymer_template[idx+1]),collapse="")
      to_insert <- df[df$x1 %in% pair, 2] 
      if(length(to_insert)!=0){
        polymer_template <- c(polymer_template[1:idx],to_insert,polymer_template[(idx+1):length(polymer_template)])
        idx <- idx+2
      }else{
        idx <- idx+1
      }
    }
  }
  return(polymer_template)
}

final_polymer <- sort(table(polymer_growth(mydata,10)),decreasing=TRUE)
most_common <- final_polymer[1]
least_common <- final_polymer[length(final_polymer)]
most_common-least_common
#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
#takes a long time, need to optimize
final_polymer <- sort(table(polymer_growth(mydata,40)),decreasing=TRUE)
most_common <- final_polymer[1]
least_common <- final_polymer[length(final_polymer)]
most_common-least_common








