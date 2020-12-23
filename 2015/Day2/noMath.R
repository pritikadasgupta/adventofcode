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
# mydata <- readLines('2015/Day2/input.txt')
mydata <- readLines('2015/Day2/input.txt')

#------------------------------------------------------------------------------------------
#Part 1 & 2
#------------------------------------------------------------------------------------------
wrapping = 0
ribbon = 0
for(numbers in mydata){
  measurements <- sort(as.numeric(strsplit(numbers,"x")[[1]]))
  present_small <- prod(measurements[c(1:2)])
  l <- measurements[1]
  w <- measurements[2]
  h <- measurements[3]
  present_main <- (2*l*w + 2*w*h + 2*h*l)
  wrapping <- wrapping + present_small + present_main
  
  bow <-l*w*h
  ribbon1 <- l+l+w+w
  ribbon <- ribbon + bow+ ribbon1
  
}

print(paste("Part 1:",wrapping))
print(paste("Part 2:",ribbon))




