#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- as.character(readLines("2021/Day9/input.txt"))
example <- as.character(readLines("2021/Day9/example.txt"))

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

risklevel <- function(x){
  mat <- matrix(0,ncol=nchar(x[1]),nrow=length(x))
  for(i in 1:length(x)){
    for(j in 1:nchar(x[1])){
      mat[i,j] <- as.numeric(strsplit(x[i],"")[[1]][j])
    }
  }
  
  mat_low <- matrix(NA,ncol=nchar(x[1]),nrow=length(x))
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      
      if(i==1){
        up <-  TRUE
        down <- mat[i,j]<mat[i+1,j]
        
        if(j==1){
          left <- TRUE
          right <- mat[i,j]<mat[i,j+1]
        }else if (j==ncol(mat)){
          left <- mat[i,j]<mat[i,j-1]
          right <- TRUE
        }else{
          left <- mat[i,j]<mat[i,j-1]
          right <- mat[i,j]<mat[i,j+1]
        }
      }else if(i==nrow(mat)){
        up <-  mat[i,j]<mat[i-1,j]
        down <- TRUE
        if(j==1){
          left <- TRUE
          right <- mat[i,j]<mat[i,j+1]
        }else if (j==ncol(mat)){
          left <- mat[i,j]<mat[i,j-1]
          right <- TRUE
        }else{
          left <- mat[i,j]<mat[i,j-1]
          right <- mat[i,j]<mat[i,j+1]
        }
      }else{
        up <-  mat[i,j]<mat[i-1,j]
        down <- mat[i,j]<mat[i+1,j]
        
        if(j==1){
          left <- TRUE
          right <- mat[i,j]<mat[i,j+1]
        }else if (j==ncol(mat)){
          left <- mat[i,j]<mat[i,j-1]
          right <- TRUE
        }else{
          left <- mat[i,j]<mat[i,j-1]
          right <- mat[i,j]<mat[i,j+1]
        }
      }
      
      if(up&down&left&right){
        mat_low[i,j] <- mat[i,j]+1
      }
      
    }
  }
  
  return(list(mat,mat_low))
}

sum(risklevel(example)[[2]],na.rm=TRUE)
sum(risklevel(mydata)[[2]],na.rm=TRUE) #part 1 answer
#------------------------------------------------------------------------------------------
#Part 2 (using the raster library)
#------------------------------------------------------------------------------------------
library(raster)

mat_r <- raster(risklevel(mydata)[[1]])
mat_r[mat_r!= 9] <- 1
mat_r[mat_r== 9] <- 0
basins <- clump(mat_r, directions=4)
prod(sort(table(basins[]),decreasing=TRUE)[1:3]) #part 2 answer

plot(mat_r)

#------------------------------------------------------------------------------------------
#Part 2 (using a recursive function)
#------------------------------------------------------------------------------------------
mat <- risklevel(mydata)[[1]]
mat_low <- risklevel(mydata)[[2]]



