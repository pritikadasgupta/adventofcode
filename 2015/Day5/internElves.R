#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries
library(utils)

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------

mydata <- readLines('2015/Day5/input.txt')

#------------------------------------------------------------------------------------------
#Functions
#------------------------------------------------------------------------------------------

check_vowel <- function(mystring){
  nice <- FALSE
  mystring <- strsplit(mystring,"")[[1]]
  vowels <- c("a","e","i","o","u")
  vowel_count <- 0
  for(character in mystring){
    if(character %in% vowels){
      vowel_count <- vowel_count+1
    }
  }
  
  if(vowel_count >=3){
    nice=TRUE
  }
  
  return(nice)
}


check_twice <- function(mystring){
  nice <- FALSE
  mystring <- strsplit(mystring,"")[[1]]
  for(i in 1:(length(mystring)-1)){
    mycharacter <- mystring[i]
    nextcharacter <- mystring[i+1]
    
    if(mycharacter == nextcharacter){
      nice <- TRUE
    }
    
  }
  return(nice)
}



check_strings <- function(mystring){
  nice <- TRUE
  mystring <- strsplit(mystring,"")[[1]]
  checkstrings <- c("ab","cd","pq","xy")
  for(i in 1:(length(mystring)-1)){
    mycharacter <- mystring[i]
    nextcharacter <- mystring[i+1]
    checkcharacter <- paste(mycharacter,nextcharacter,sep="")
    if(checkcharacter %in% checkstrings){
      nice <- FALSE
    }
    
  }
  return(nice)
}



check_double <- function(mystring){
  nice <- FALSE
  mystring <- strsplit(mystring,"")[[1]]
  
  for(i in 1:(length(mystring)-1)){
    mycharacter <- mystring[i]
    nextcharacter <- mystring[i+1]
    checkcharacter <- paste(mycharacter,nextcharacter,sep="")
    check_against <- mystring[c(-i,-(i+1))]
    
    #all two-ples
    two_ <- combn(check_against, 2)
    for(j in 1:ncol(two_)){
      if(checkcharacter %in% two_[,j]){
        nice <- TRUE
      }
    }
    
  }
  return(nice)

}

check_letterrepeat <- function(mystring){
  nice <- FALSE
  mystring <- strsplit(mystring,"")[[1]]
  
  for(i in 1:(length(mystring)-2)){
    mycharacter <- mystring[i]
    nextcharacter <- mystring[i+2]
    if(mycharacter == nextcharacter){
      if(mycharacter != mystring[i+1]){
        nice <- TRUE
      }
    }
    
  }
  return(nice)
}




#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------


nice <- vector()
naughty <- vector()
for(i in 1:length(mydata)){
  mystring <- mydata[i]
  
  #check vowels: aeiou
  a <-check_vowel(mystring)
  
  #check doubles
  b <-check_twice(mystring)
  
  #check strings
  c <-check_strings(mystring)
  
  if(a && b && c){
    nice <- append(nice,mystring)
  }else{
    naughty <- append(naughty,mystring)
  }
  
}

print(paste("Part 1:",length(nice)))


#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------


nice <- vector()
naughty <- vector()
for(i in 1:length(mydata)){
  mystring <- mydata[i]
  a <-check_letterrepeat(mystring)
  b <-check_double(mystring)
  if(a && b){
    nice <- append(nice,mystring)
  }else{
    naughty <- append(naughty,mystring)
  }
  
}

print(paste("Part 2:",length(nice)))

# mystring <- "qjhvhtzxzqqjkmpb"
