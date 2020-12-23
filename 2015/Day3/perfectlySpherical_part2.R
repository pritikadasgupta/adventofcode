#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------

#Use this if you're running from the command line:
# mydata <- readLines(file("stdin"))

#Use this if you're opening this repo as a R project, using relative paths:
# mydata <- readLines('2015/Day3/input.txt')
mydata <- strsplit(readLines('2015/Day3/input.txt'),"")[[1]]
# mydata <- strsplit("^v^v^v^v^v","")[[1]]

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

x1 <- 0
y1 <- 0

x2 <- 0
y2 <- 0

places1 <- list()
places1[[1]] <- c(x1,y1)

places2 <- list()
places2[[1]] <- c(x2,y2)

for(i in 2:(length(mydata))+1){
  direction <- mydata[i-1]

  if(i%%2 ==0){
    if(direction == "v"){
      y1 <- y1-1
    }
    
    if(direction == ">"){
      x1 <- x1+1
    }
    
    if(direction == "^"){
      y1 <- y1+1
    }
    
    if(direction == "<"){
      x1 <- x1-1
    }
    places1[[i]] <- c(x1,y1)
  }else{
    if(direction == "v"){
      y2 <- y2-1
    }
    
    if(direction == ">"){
      x2 <- x2+1
    }
    
    if(direction == "^"){
      y2 <- y2+1
    }
    
    if(direction == "<"){
      x2 <- x2-1
    }
    places2[[i]] <- c(x2,y2)
  }
  
}

places1[sapply(places1, is.null)] <- NULL
places2[sapply(places2, is.null)] <- NULL
places <- unique(append(places1,places2))

length(unique(places))-1
