#!/usr/bin/env Rscript

#Libraries
library(tidyverse)

#Use this if you're running from the command line:
# data1 <- readLines(file("stdin")) #read in file

#Use this if you're opening this repo as a R project, using relative paths:
data1 <- read_file("2020/Day6/input")

#-------------------
#PART 1
#-------------------

#Clean up data
data2 <- paste(data1,collapse = "\n") #concatenate everything into one string
data3 <- strsplit(data2,"\n\n")[[1]] #find where there are two new lines to separate btwn blanks
data4 <- gsub("\n"," ",data3) #get rid of the \n


# For each group, count the number of questions to which 
# anyone answered "yes". What is the sum of those counts?
counts <- vector() #initialize counts vector 
#For each group, find unique letters
for(x in data4){
  x <- strsplit(x,"")[[1]] #split the answers
  unique_x <- unique(x)
  x_noblank <- unique_x[!(unique_x %in% c(" "))]
  counts <- append(counts,length(x_noblank)) #append counts
}

part1 <- sum(counts)
print(part1)

#-------------------
#PART 2
#-------------------
counts2 <- vector()
# questions to which everyone answered "yes"

num <- length(data4) #number of groups



for(x in data4){
  #find all unique elements
  split_x <- strsplit(x,"")[[1]] #split the answers
  x_noblank <- split_x[!(split_x %in% c(" "))]
  
  y <- strsplit(x," ")[[1]] #split the answers for each person in the group
  num_people <- length(y)

  a <- rle(sort(x_noblank))
  counts2 <- append(counts2,sum(a$lengths ==num_people))
}

part2 <- sum(counts2)
print(part2)

