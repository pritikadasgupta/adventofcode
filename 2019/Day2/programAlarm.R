#!/usr/bin/env Rscript
#clear workspace
rm(list = ls())
#Libraries
# library(tidyverse)
# library(igraph)

#Load Data
#Use this if you're running from the command line:
# data1 <- readLines(file("stdin)) #read in file

#Use this if you're opening this repo as a R project, using relative paths:
data1 <- readLines("2019/Day2/exercise2.input.txt") #read in file

test1 <- "1,9,10,3,2,3,11,0,99,30,40,50"
test2 <- "1,0,0,0,99"
test3 <- "2,3,0,3,99"
test4 <- "2,4,4,5,99,0"
test5 <- "1,1,1,4,99,5,6,0,99"

# 1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
# 2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
# 2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
# 1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.

#Functions
#-------------------------------------------------------------------------

opcode_1 <- function(x,i){
  i<-i+1
  pos1 <- x[i]
  pos2 <- x[i+1]
  
  return(x[pos1+1]+x[pos2+1])
}

opcode_2 <- function(x,i){
  i<-i+1
  pos1 <- x[i]
  pos2 <- x[i+1]
  
  return(x[pos1+1]*x[pos2+1])
}


opcode_99 <- function(x,i){
  break
}

#Clean up data
#-------------------------------------------------------------------------
# data2 <- as.numeric(strsplit(data1,",")[[1]])
main_data <- as.numeric(strsplit(data1,",")[[1]])
data2 <- main_data
value <- vector()
position <- vector()


#-------------------------------------------------------------------------
#PART 1
#-------------------------------------------------------------------------
# To do this, before running the program, 
# replace position 1 with the value 12 and 
# replace position 2 with the value 2
data2[2]<- 12
data2[3]<- 2

for(i in seq(1,length(data2),by=4)){
  if(data2[i]==1){
    value <- append(value,opcode_1(data2,i))
    position <- append(position,data2[i+3]+1)
    data2[position] <- value
    print(data2)
  }else if(data2[i]==2){
    value <- append(value,opcode_2(data2,i))
    position <- append(position,data2[i+3]+1)
    data2[position] <- value
    print(data2)
  }else if(data2[i]==99){
    value <- append(value,0)
    position <- append(position,0)
    break
  }
  
}

print(paste("Part 1:",data2[1]))



#-------------------------------------------------------------------------
#PART 2
#-------------------------------------------------------------------------

# To complete the gravity assist, 
# you need to determine what pair of inputs produces the output 19690720."

gravity <- function(input1,input2,data2){
  data2 <- main_data
  data2[2]<- input1
  data2[3]<- input2
  value <- vector()
  position <- vector()
  
  for(i in seq(1,length(data2),by=4)){
    if(data2[i]==1){
      value <- append(value,opcode_1(data2,i))
      position <- append(position,data2[i+3]+1)
      data2[data2[i+3]+1] <- opcode_1(data2,i)
    }else if(data2[i]==2){
      value <- append(value,opcode_2(data2,i))
      position <- append(position,data2[i+3]+1)
      data2[data2[i+3]+1] <- opcode_2(data2,i)
    }else if(data2[i]==99){
      value <- append(value,0)
      position <- append(position,0)
      break
    }
    
  }
  return(data2[1])
}



for(a in 1:100){
  for(b in 1:100){
    check_num <- gravity(a,b,data2)
    if(check_num==19690720){
      part2 <- 100*(a)+(b)
      print(paste("Part 2:",part2))
      break
    }
  }
}



