#!/usr/bin/env Rscript
#clear workspace
rm(list = ls())

#Libraries

#Set working directory and load in data
# data1 <- as.numeric(readLines(file("stdin"))) #read in file
setwd("~/Documents/adventofcode1/adventofcode/2019/Day3")
file1 <- "exercise3.testinput.txt" #read in file

# file1 <- "stdin"
con <- file(file1, open = "r")
data1 <- readLines(con) #read in file
close(con)

test <- "R8,U5,L5,D3"
test2 <- "U7,R6,D4,L4"
data1 <- test
data2 <- test2



#Function to clean up data
#-------------------------------------------------------------------------

cleandata <- function(data){
  
  data <- as.character(strsplit(data,",")[[1]])
  
  directions <- vector()
  for (datum in data) {
    directions <- append(directions,strsplit(datum,"")[[1]][1])
  }
  
  values <- vector()
  for(datum in data){
    x <- strsplit(datum,"")[[1]]
    x <- as.numeric(x[2:length(x)])
    x <- paste(c(x),collapse="")
    
    values <- append(values,as.numeric(x))
  }
  
  df <- as.data.frame(cbind(directions,values))
  return(df)
}

# data_1 <- cleandata(data1[1])
# data_2 <- cleandata(data1[2])

data_1 <- cleandata(data1)
data_2 <- cleandata(data2)


#-------------------------------------------------------------------------
#PART 1
#-------------------------------------------------------------------------

walk <- function(df){
  x=0
  y=0
  for(step in 1:length(df$directions)){
    c=df$directions[step]
    n = as.numeric(df$values[step])
    if(c=="U"){
      y=y+n
    }else if(c=="L"){
      x=x-n
    }else if(c=="D"){
      y=y-n
    }else if(c=="R"){
      x=x+n
    }
    print(paste(x,y))
  }
  return(c(x,y))
}

distance_origin <- function(a1,a2,b1,b2){
  c1 <- sqrt(a1^2 + b1^2)
  # c2 <- sqrt(a1^2 + b2^2)
  # c3 <- sqrt(a2^2 + b1^2)
  c4 <- sqrt(a2^2 + b2^2)
  
  c_vector <- c(c1,c4)
  
  x <-which(c_vector==min(c_vector))
  
  if(x==1){
    return(c(a1,b1))
  }
  
  if(x==2){
    return(c(a2,b2))
  }

  
}

i = walk(data_1)
j = walk(data_2)

dist <- distance_origin(i[1],i[2],j[1],j[2])
manhattan_distance = abs(dist[1]) + abs(dist[2])
print(paste("Part 1:",manhattan_distance))


# 5319
# 122514
