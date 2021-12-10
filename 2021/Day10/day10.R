#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- as.character(readLines("2021/Day10/input.txt"))
example <- as.character(readLines("2021/Day10/example.txt"))

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

openings <- c("(","[","{","<")
closings <- c(")","]","}",">")
table_pairs <- c("]" = "[", ")" = "(", "}" = "{", ">" = "<")
points = c(")" = 3,"]" = 57,"}" = 1197,">" = 25137)

corrupted <- function(x){
  incomplete <- vector()
  num_points <- 0
  for(i in 1:length(x)){
    y <- strsplit(x[i],"")[[1]]
    corrupt <- FALSE
    stack <- vector()
    for(j in 1:length(y)){
      if(y[j] %in% openings){
        stack <- append(stack,y[j])
      }else{
        
        if(table_pairs[y[j]] != stack[length(stack)]){
          corrupt <- TRUE
          break
        }else{
          stack <- stack[-length(stack)]
        }
      }
    }
    if(corrupt){
      num_points <- num_points + points[y[j]]
    }else{
      incomplete <- append(incomplete,x[i])
    }
  }
  return(list(num_points,incomplete))
}

corrupted(example)[[1]]
corrupted(mydata)[[1]]

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------

table_pairs_2 <- c("[" = "]", "(" = ")", "{" = "}", "<" = ">")
points_2 = c(")" = 1,"]" = 2,"}" = 3,">" = 4)


part2 <- function(x){
  scores <- vector()
  for(i in 1:length(x)){
    y <- strsplit(x[i],"")[[1]]
    stack <- vector()

    for(j in 1:length(y)){
      if(y[j] %in% openings){
        stack <- append(stack,y[j])
      }else{
        stack <- stack[-length(stack)]
      }
    }
    points_ <- points_2[table_pairs_2[rev(stack)]]
    score <- 0
    for(k in 1:length(points_)){
      score <- score*5
      score <- score + points_[k]
    }
    scores <- append(scores, score)
  }
  return(sort(scores)[ceiling(length(scores)/2)]) #could also use median
}

part2(corrupted(example)[[2]])
part2(corrupted(mydata)[[2]])
