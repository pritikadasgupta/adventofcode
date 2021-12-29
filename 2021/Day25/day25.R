#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
mydata <- do.call(rbind,strsplit(readLines("2021/Day25/input.txt"),""))
example <- do.call(rbind,strsplit(readLines("2021/Day25/example.txt"),""))

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
east <- function(x){
  y <- x
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      cur <- y[i,j]
      
      if(j==ncol(x)){
        front <- y[i,(1)]
        if(cur==".") next
        if(cur==">"){
          if(front=="."){
            x[i,(1)] <- ">"
            x[i,j] <- "."
          }
        }
      }else{
        front <- y[i,(j+1)]
        if(cur==".") next
        if(cur==">"){
          if(front=="."){
            x[i,(j+1)] <- ">"
            x[i,j] <- "."
          }
        }
      }
      
    }
  }
  x
}

south <- function(x){
  y <- x
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      cur <- y[i,j]
      
      if(i==nrow(x)){
        front <- y[1,j]
        if(cur==".") next
        if(cur=="v"){
          if(front=="."){
            x[(1),j] <- "v"
            x[i,j] <- "."
          }
        }
      }else{
        front <- y[(i+1),j]
        if(cur==".") next
        if(cur=="v"){
          if(front=="."){
            x[(i+1),j] <- "v"
            x[i,j] <- "."
          }
        }
      }
      
    }
  }
  x
}


part1 <- function(mydata){
  moves <-1
  steps <- 0
  check <- sum(mydata==mydata)
  while(moves!=0){
    mydata1 <-south(east(mydata))
    steps <- steps + 1
    if(sum(mydata==mydata1)==check){
      moves <- 0
    }else{
      mydata <- mydata1
    }
  }
  steps
}

part1(example)
part1(mydata)

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------

