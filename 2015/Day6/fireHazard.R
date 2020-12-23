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
# mydata <- readLines('2015/Day6/input.txt')
mydata <- readLines('2015/Day6/input.txt')

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
df <- setNames(data.frame(matrix(ncol = 5, nrow = length(mydata))), 
                   c("command","lower_x","lower_y","upper_x","upper_y"))

for(i in 1:length(mydata)){
  mystring <- strsplit(mydata[i]," ")[[1]]
  if(mystring[1]=="turn"){
    df$command[i] <- paste(mystring[1],mystring[2],sep=" ")
    
    df$lower_x[i] <- as.numeric(strsplit(mystring[3],",")[[1]][1])
    df$lower_y[i] <- as.numeric(strsplit(mystring[3],",")[[1]][2])
    
    df$upper_x[i] <- as.numeric(strsplit(mystring[5],",")[[1]][1])
    df$upper_y[i] <- as.numeric(strsplit(mystring[5],",")[[1]][2])
  }
  
  
  if(mystring[1]=="toggle"){
    df$command[i] <- "toggle"
    
    df$lower_x[i] <- as.numeric(strsplit(mystring[2],",")[[1]][1])
    df$lower_y[i] <- as.numeric(strsplit(mystring[2],",")[[1]][2])
    
    df$upper_x[i] <- as.numeric(strsplit(mystring[4],",")[[1]][1])
    df$upper_y[i] <- as.numeric(strsplit(mystring[4],",")[[1]][2])
  }
  
}


lightMatrix <- matrix(FALSE,ncol = 1000, nrow = 1000)
toggle <- FALSE
data_ <- FALSE

for(i in 1:nrow(df)){
  if(df$command[i]=="turn off"){
    data_ <- FALSE
  }else if(df$command[i]=="turn on"){
    data_ <- TRUE
  }else if(df$command[i]=="toggle"){
    toggle <- TRUE
  }
  
  for (x_val in (df$lower_x[i]):(df$upper_x[i])) {
    for (y_val in (df$lower_y[i]):(df$upper_y[i])){
      if(toggle){
        lightMatrix[x_val,y_val] <- !lightMatrix[x_val,y_val]
      }else{
        lightMatrix[x_val,y_val] <- data_
      }
    }
  }
}

light_counter <- sum(lightMatrix)
light_counter
# 569999
#not getting the right answer
