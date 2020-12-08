#!/usr/bin/env Rscript
#clear workspace
rm(list = ls())
#Libraries
# library(tidyverse)
# library(igraph)

#Set working directory and load in data
data1 <- readLines(file("stdin")) #read in file
# setwd("~/Documents/adventofcode1/adventofcode/2020/Day8")
# data1 <- readLines("exercise8.input.txt") #read in file

#-------------------------------------------------------------------------
#Clean up data
#-------------------------------------------------------------------------

dat = as.data.frame(do.call(rbind, strsplit(data1, split=" ")), stringsAsFactors=FALSE)
names(dat) = c("operation","argument")


'%!in%' <- function(x,y)!('%in%'(x,y))

#-------------------------------------------------------------------------
#PART 1
#-------------------------------------------------------------------------


# Each instruction consists of an operation (acc, jmp, or nop) 
# and an argument (a signed number like +4 or -20).


accumulator <- 0 #accumulator starts at 0
i=1 #position marker
i_vector <- vector()
dontstop = 100

while(dontstop >0){
  
  #check to see if this operation has been operated on
  if(i %!in% i_vector){
    dontstop = 100
  }else{
    dontstop = -1
    break
  }
  
  i_vector <- append(i_vector,i)
  
  #get operation and argument
  curr_operation <- dat[i,1]
  curr_argument <- dat[i,2]
  # curr_argument_num <- as.numeric(gsub("([0-9]{1,4}).*","\\1",  curr_argument))
  curr_argument_num <- as.numeric(curr_argument)
  curr_argument_sign <- strsplit(curr_argument, "")[[1]][1]
  
  if(curr_operation == "acc"){
    i=i+1
    accumulator=accumulator+curr_argument_num
  }else if(curr_operation == "jmp"){
    i=i+curr_argument_num
    print(paste(i-curr_argument_num,"jumps to",i))
  }else if(curr_operation == "nop"){
    i=i+1
    print(paste(i-1,"jumps to (nop)",i))
  }
  
}


print(paste("Part 1:",accumulator))

#-------------------------------------------------------------------------
#PART 2
#-------------------------------------------------------------------------


idx_nopjmp <- which(dat$operation=="nop" | dat$operation=="jmp")
j_vector <- vector()
for(k in 124:length(idx_nopjmp)){
  j <- idx_nopjmp[k]
  newdat <- dat
  if(newdat$operation[j]=="nop"){
    newdat$operation[j]="jmp"
  }else if(newdat$operation[j]=="jmp"){
    newdat$operation[j]="nop"
  }
  
  accumulator <- 0 #accumulator starts at 0
  i=1 #position marker
  i_vector <- vector()
  dontstop = 100
  
  while(dontstop >0){
    
    #check to see if this operation has been operated on
    if(i %!in% i_vector){
      dontstop = 100
    }else{
      dontstop = -1
      j_vector <- append(j_vector,j)
      print(paste(j,"idx doesn't work",dontstop))
      break
    }
    
    i_vector <- append(i_vector,i)
    
    #get operation and argument
    curr_operation <- newdat[i,1]
    curr_argument <- newdat[i,2]
    # curr_argument_num <- as.numeric(gsub("([0-9]{1,4}).*","\\1",  curr_argument))
    curr_argument_num <- as.numeric(curr_argument)
    curr_argument_sign <- strsplit(curr_argument, "")[[1]][1]
    
    if(curr_operation == "acc"){
      i=i+1
      accumulator=accumulator+curr_argument_num
    }else if(curr_operation == "jmp"){
      i=i+curr_argument_num
    }else if(curr_operation == "nop"){
      i=i+1
    }
    
  }
  
  
}

#------
k=setdiff(idx_nopjmp,j_vector)
#-------

j <- idx_nopjmp[123]
newdat <- dat
if(newdat$operation[j]=="nop"){
  newdat$operation[j]="jmp"
}else if(newdat$operation[j]=="jmp"){
  newdat$operation[j]="nop"
}

accumulator <- 0 #accumulator starts at 0
i=1 #position marker
i_vector <- vector()
dontstop = 100

while(dontstop >0){
  
  #check to see if this operation has been operated on
  if(i %!in% i_vector){
    dontstop = 100
  }else{
    dontstop = -1
    j_vector <- append(j_vector,j)
    print(paste(j,"idx doesn't work",dontstop))
    break
  }
  
  i_vector <- append(i_vector,i)
  
  #get operation and argument
  curr_operation <- newdat[i,1]
  curr_argument <- newdat[i,2]
  # curr_argument_num <- as.numeric(gsub("([0-9]{1,4}).*","\\1",  curr_argument))
  curr_argument_num <- as.numeric(curr_argument)
  curr_argument_sign <- strsplit(curr_argument, "")[[1]][1]
  
  if(curr_operation == "acc"){
    i=i+1
    accumulator=accumulator+curr_argument_num
  }else if(curr_operation == "jmp"){
    i=i+curr_argument_num
  }else if(curr_operation == "nop"){
    i=i+1
  }
  
}


print(paste("Part 2:",accumulator))
