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

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

x <- 0
y <- 0

places <- list()
places[[1]] <- c(x,y)
i=2

for(direction in mydata){
  if(direction == "v"){
    y <- y-1
  }
  
  if(direction == ">"){
    x <- x+1
  }
  
  if(direction == "^"){
    y <- y+1
  }
  
  if(direction == "<"){
    x <- x-1
  }
  
  places[[i]] <- c(x,y)
  i <- i+1
  
}

unique_places <- unique(places)
i=1
place_count <- list()
for(j in 1:length(unique_places)){
  count = 0
  place_count[[j]] <- vector()
  for(i in 1:length(places)){
    if(sum(unique_places[[j]] == places[[i]])==2){
      count = count+1
      
    }
  }
  place_count[[j]] <- count

}


length(which(place_count >=1))

