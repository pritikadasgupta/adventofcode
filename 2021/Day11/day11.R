#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- as.character(readLines("2021/Day11/input.txt"))
example1 <- as.character(readLines("2021/Day11/example1.txt"))
example2 <- as.character(readLines("2021/Day11/example2.txt"))

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

flash_steps <- function(x, steps)  {
  
  #format
  mat <- matrix(0,nrow=length(x),ncol=nchar(x[1]))
  for(i in 1:length(x)){
    y <- as.numeric(strsplit(x[i],"")[[1]])
    mat[i,] <- y
  }
  
  flashes <- 0
  
  for(s in 1:steps) {
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
    flashes <- flashes + sum(is.na(mat))
    
    mat[is.na(mat)] <- 0 #reset
  }
  
  return(flashes)
}

flash_steps(mydata,100)

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













