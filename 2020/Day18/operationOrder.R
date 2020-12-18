#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries
library(tidyverse)
#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------

#Use this if you're running from the command line:
# mydata <- strsplit(readLines(file("stdin")),"")

#Use this if you're opening this repo as a R project, using relative paths:
mydata <- strsplit(readLines('2020/Day18/input.txt')," ")


#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
`%+%` <- function(a, b) a + b

`%*%` <- function(a, b) a * b

hasParantheses <- function(x){
  x <- strsplit(x,"")
  idx <- vector()
  for(i in 1:length(x)){
    idx_track<-which(x[[i]]==")")
    idx <- append(idx,idx_track)
  }

  if(length(idx)==0){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

new_eval <- function(mydata){
  results <- vector()
  for (i in 1:length(mydata)) {
    x <- mydata[[i]]
    
    for(a in 1:length(x)){
      if(x[a]=="+"){
        x[a] <- "%+%"
      }
      
      if(x[a]=="*"){
        x[a] <- "%*%"
      }
    }
    
    if(hasParantheses(x)){
      result <- eval(parse(text=paste(x,collapse="")))
      results <- append(results,result)
    }else{
      j=1
      while(j==1){
        if(j==length(x)){
          result <- eval(parse(text=paste(x[j],collapse="")))
          j=0
          results <- append(results,result)
        }else{
          x_1_replacement <- eval(parse(text=paste(x[j:(j+2)],collapse="")))
          j=1
          x <- x[c(-1,-2,-3)]
          x <- append(x,x_1_replacement,after=0)
        }
      }
      
    }
  }
  return(results)
}

get_sum <- function(mydata){
  return(sum(new_eval(mydata)))
}

print(paste("Part 1:",get_sum(mydata)))
# input_test1.txt sum:
# 71+51+26+437+12240+13632
# 26457

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
PEASMD <- function(expression_) {
  #reset
  `+` <- function(a, b) base::`*`(a, b)
  `*` <- function(a, b) base::`+`(a, b)
  expression_ <- gsub('\\+', 'tmp', expression_)
  expression_ <- gsub('\\*', '+', expression_)
  expression_ <- gsub('tmp', '*', expression_)
  return(eval(parse(text = expression_)))
}

# PEASMD("1 + 2 * 3 + 4 * 5 + 6")

new_eval2 <- function(mydata){
  results <- vector()
  for (i in 1:length(mydata)) {
    x <- mydata[[i]]
    result <- PEASMD(parse(text=paste(x,collapse="")))
    results <- append(results,result)
  }
  return(results)
}


print(paste("Part 2:",sum(new_eval2(mydata))))
# input_test1.txt sum:
# 231+51+46+1445+669060+23340
# 694171

