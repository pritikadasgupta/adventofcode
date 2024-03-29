#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- "3113322113"
example <- "111221"
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
look_and_say <- function(x,iterations){
  for(i in 1:iterations){
    x <- as.numeric(strsplit(x,"")[[1]])
    x <- as.numeric(matrix(unlist(rle(x)),nrow=2,byrow=TRUE))
    x <- paste0(x,collapse="")
  }
  return(nchar(x))
}

look_and_say(mydata,40)
#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
look_and_say(mydata,50)
