#-------------------
#clear workspace
rm(list = ls())

#Libraries
library(tidyverse)

#Set working directory and load In Data
# set(wd)
data1 <- readLines(file("stdin")) #read in file

#-------------------
#PART 1
#-------------------

#FUNCTIONS FOR EACH LETTER
charF <-function(x){ #take the lower half
  return(c(x[1],
           ((x[2]-x[1]-1)/2) + x[1]
           ))
}

charB <-function(x){ #take the upper half
  return(c(((x[2]-x[1]+1)/2) + x[1],
           x[2]))
}

charL <- charF #take the lower half
charR <- charB #take the upper half

row_ <- vector()
column_ <- vector()

for (x in data1) {
  rowVector <- c(0,127)
  colVector <- c(0,7)
  xsplit <- strsplit(x,"")[[1]]
  for (y in xsplit){
    if(y=="F"){
      rowVector <- charF(rowVector)
    }else if(y=="B"){
      rowVector <- charB(rowVector)
    }else if(y=="L"){
      colVector <- charL(colVector)
    }else if(y=="R"){
      colVector <- charR(colVector)
    }
  }
  
  if(xsplit[7]=="F"){
    row_ <- append(row_,rowVector[1])
  }else{
    row_ <- append(row_,rowVector[2])
  }
  
  if(xsplit[10]=="L"){
    column_ <- append(column_,colVector[1])
  }else{
    column_ <- append(column_,colVector[2])
  }
}

#Part 1: Highest seat ID
part1 <- max(8*row_+column_) #multiply the row by 8, then add the column

#Part 2:

#IDs that I have
listIDs <- 8*row_+column_
listIDs <- sort(listIDs)

#All IDs
allIDs <- seq(listIDs[1],listIDs[length(listIDs)])

#Which ID is mine?
setdiff(allIDs,listIDs)
