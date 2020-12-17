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
mydata <- strsplit(readLines('2020/Day17/input.txt'),"")

cubes_df <- mydata[[1]]
for(i in 2:length(mydata)){
  cubes_df <- rbind(cubes_df,mydata[[i]])
}
cubes_df <- as.data.frame(cubes_df)

#------------------------------------------------------------------------------------------
# Part 1
#------------------------------------------------------------------------------------------

# If a cube is active and exactly 2 or 3 of its neighbors are also active, 
# the cube remains active. Otherwise, the cube becomes inactive.

# If a cube is inactive but exactly 3 of its neighbors are active, 
# the cube becomes active. Otherwise, the cube remains inactive.

find_cubes <- function(cubes,n){
  for(i in 1:n){
    results <- list()
    for(x in (min(cubes$X_vals)-1):(max(cubes$X_vals)+1)){
      for(y in (min(cubes$Y_vals)-1):(max(cubes$Y_vals)+1)){
        for(z in (min(cubes$Z_vals)-1):(max(cubes$Z_vals)+1)){
          adjacent <- 
            cubes %>% 
            filter(X_vals>=(x-1)&X_vals<=(x+1),
                   Y_vals>=(y-1)&Y_vals<=(y+1),
                   Z_vals>=(z-1)&Z_vals<=(z+1)) %>% 
            nrow()
          
          current <- 
            cubes %>% 
            filter(X_vals==(x)&X_vals==(x),
                   Y_vals==(y)&Y_vals==(y),
                   Z_vals==(z)&Z_vals==(z)) %>% 
            nrow()
          if(current==0&adjacent==3){
            results[[paste(x,y,z)]] <- data.frame("X_vals"=x,"Y_vals"=y,"Z_vals"=z)
          }
          if(current==1&(adjacent==(2+1)|adjacent==(3+1))){
            results[[paste(x,y,z)]] <- data.frame("X_vals"=x,"Y_vals"=y,"Z_vals"=z)
          }
        }
      }
    }
    
    cubes <- bind_rows(results)
  }
  cubes %>% nrow()
}

#make cubes df
X_vals <- vector()
Y_vals <- vector()
Z_vals <- vector()
for(x in 1:length(mydata[[1]])){
  for(y in 1:length(mydata[[1]])){
      if(cubes_df[x,y]=="#"){
        X_vals <- append(X_vals,x)
        Y_vals <- append(Y_vals,y)
        Z_vals <- append(Z_vals,0)
      }
  }
}
cubes <- as.data.frame(cbind(X_vals,Y_vals,Z_vals))
n <- 6
part1 <- find_cubes(cubes,n)
print(paste("Part 1:",part1))

#------------------------------------------------------------------------------------------
# Part 2
#------------------------------------------------------------------------------------------

find_cubes_4 <- function(cubes,n){
  for(i in 1:n){
    results <- list()
    for(x in (min(cubes$X_vals)-1):(max(cubes$X_vals)+1)){
      for(y in (min(cubes$Y_vals)-1):(max(cubes$Y_vals)+1)){
        for(z in (min(cubes$Z_vals)-1):(max(cubes$Z_vals)+1)){
          for(w in (min(cubes$W_vals)-1):(max(cubes$W_vals)+1)){
            touching <- 
              cubes %>% 
              filter(X_vals>=(x-1)&X_vals<=(x+1),
                     Y_vals>=(y-1)&Y_vals<=(y+1),
                     Z_vals>=(z-1)&Z_vals<=(z+1),
                     W_vals>=(w-1)&W_vals<=(w+1)) %>% 
              nrow()
            
            current <- 
              cubes %>% 
              filter(X_vals==(x)&X_vals==(x),
                     Y_vals==(y)&Y_vals==(y),
                     Z_vals==(z)&Z_vals==(z),
                     W_vals==(w)&W_vals==(w)) %>% 
              nrow()
            if(current==0&touching==3){
              results[[paste(x,y,z,w)]] <- data.frame("X_vals"=x,"Y_vals"=y,"Z_vals"=z,"W_vals"=w)
            }
            if(current==1&(touching==(2+1)|touching==(3+1))){
              results[[paste(x,y,z,w)]] <- data.frame("X_vals"=x,"Y_vals"=y,"Z_vals"=z,"W_vals"=w)
            }
          }
        }
      }
    }
    print(i)
    cubes <- bind_rows(results)
  }
  cubes %>% nrow()
}

#make cubes df
X_vals <- vector()
Y_vals <- vector()
Z_vals <- vector()
W_vals <- vector()
for(x in 1:length(mydata[[1]])){
  for(y in 1:length(mydata[[1]])){
    if(cubes_df[x,y]=="#"){
      X_vals <- append(X_vals,x)
      Y_vals <- append(Y_vals,y)
      Z_vals <- append(Z_vals,0)
      W_vals <- append(W_vals,0)
    }
  }
}
cubes <- as.data.frame(cbind(X_vals,Y_vals,Z_vals,W_vals))
n <- 6
part2 <- find_cubes_4(cubes,n)
print(paste("Part 2:",part2))




