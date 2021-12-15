#!/usr/bin/env Rscript
library(igraph)
#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- as.character(readLines("2021/Day15/input.txt"))
example <- as.character(readLines("2021/Day15/example.txt"))

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
x <- example
#format
mat <- matrix(0,nrow=length(x),ncol=nchar(x[1]))
for(i in 1:length(x)){
  y <- as.numeric(strsplit(x[i],"")[[1]])
  mat[i,] <- y
}
g <- graph.adjacency(mat, weighted=TRUE,diag=FALSE)

distance_table(g, directed = FALSE)

(s.paths <- distances(g, algorithm = "dijkstra"))
# 
# lowriskpath <- function(mat)  {
#   start <- 0
#   i <- 1
#   j <- 1
#   lowrisk <- c(mat[i,j])
#   while(i < nrow(mat) & j < ncol(mat)){
#     pointer <- mat[i,j]
#     if(j==ncol(mat) & i==nrow(mat)){
#       print(lowrisk)
#     }else if(i==nrow(mat) & j!=ncol(mat)){
#       right_pointer <- mat[i,j+1]
#       i <- i
#       j <- j+1
#       lowrisk <- c(lowrisk,right_pointer)
#     }else if(i!=nrow(mat) & j==ncol(mat)){
#       down_pointer <- mat[i+1,j]
#       i <- i+1
#       j <- j
#       lowrisk <- c(lowrisk,down_pointer)
#     }else{
#       right_pointer <- mat[i,j+1]
#       down_pointer <- mat[i+1,j]
#       if(right_pointer < down_pointer){
#         i <- i
#         j <- j+1
#         lowrisk <- c(lowrisk,right_pointer)
#       }else if (down_pointer < right_pointer){
#         i <- i+1
#         j <- j
#         lowrisk <- c(lowrisk,down_pointer)
#       }else if (down_pointer == right_pointer){
#         down_lowrisk <- lowriskpath(mat[(i+1):nrow(mat),j:ncol(mat)])
#         right_lowrisk <- lowriskpath(mat[i:nrow(mat),(j+1):ncol(mat)])
#         if(sum(down_lowrisk)<sum(right_lowrisk)) lowrisk <- down_lowrisk
#         if(sum(right_lowrisk)<sum(down_lowrisk)) lowrisk <- right_lowrisk
#       }
#     }
#     
#   }
#       
#   return(lowrisk)
# }
# 
# lowriskpath(example)


#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
rotate <- function(x) t(apply(x, 2, rev))

all_flash_steps <- function(x)  {
  
  #format
  mat <- matrix(0,nrow=length(x),ncol=nchar(x[1]))
  for(i in 1:length(x)){
    y <- as.numeric(strsplit(x[i],"")[[1]])
    mat[i,] <- y
  }
  
  all_flash_step <- NA
  step <- 1
  a <- 0
  
  png(file = paste("myplot",step,".png",sep=""))
  image(rotate(mat),main=paste("Step",step,sep=" "))
  dev.off()
  
  while(a==0) {
    mat <- mat + 1 #update energy level
    
    repeat{
      indices = which(mat > 9, arr.ind = TRUE) #flash these
      if (nrow(indices) == 0) break
      i <- indices[1,1]
      j <- indices[1,2]
      mat[i,j] <- NA
      #update energy level
      for(horiz in c(-1,0,1)) {
        for(vert in c(-1,0,1)) {
          if (horiz ==0 & vert == 0) next
          if (i+horiz < 0 | i+horiz > nrow(mat)) next
          if (j+vert < 0 | j+vert > ncol(mat)) next
          mat[i+horiz,j+vert] <- mat[i+horiz,j+vert] + 1
        }
      }
    }
    if(sum(is.na(mat))==length(mat)){
      a <- 1
      all_flash_step <- step
    }else{
      step <- step + 1
    }
    
    png(file = paste("myplot",step,".png",sep=""))
    image(rotate(mat),main=paste("Step",step,sep=" "))
    dev.off()
    
    
    mat[is.na(mat)] <- 0 #reset
  }
  
  return(all_flash_step)
}

all_flash_steps(mydata)

n <- all_flash_steps(mydata)
library(animation)
bm.files = sprintf("myplot%i.png", 1:n)
head(bm.files)
im.convert(files = bm.files, output = "day11.gif")












