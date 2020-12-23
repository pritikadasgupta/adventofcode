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
# mydata <- readLines('2015/Day1/input.txt')
mydata <- readLines('2015/Day1/input.txt')

#------------------------------------------------------------------------------------------
#Part 1 and 2
#------------------------------------------------------------------------------------------
mydata <- strsplit(mydata,"")[[1]]
santa = 0
santa_position <- vector()
for(i in 1:length(mydata)){
  if(mydata[i]=="("){
    santa=santa+1
  }else{
    santa=santa-1
  }
  
  if(santa==-1){
    santa_position <- append(santa_position,i)
  }
}

print(paste("Part 1:",santa))
print(paste("Part 2:",santa_position[1]))


