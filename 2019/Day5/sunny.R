#!/usr/bin/env Rscript
#clear workspace
rm(list = ls())

#Libraries

#Load Data
#Use this if you're running from the command line:
# file1 <- readLines(file("stdin)) #read in file

#Use this if you're opening this repo as a R project, using relative paths:
file1 <- readLines("2019/Day5/exercise5.input.txt") #read in file #read in file


#Clean up data
code <- as.integer(strsplit(file1,",")[[1]])

intcodeArray <- function(int1){
  intcode = list(as.character(int1))
  if(length(intcode)<5){
    for (i in 1:(5-length(intcode))) {
      intcode <- append(intcode,0)
    }
  }
  for (i in 1:(length(intcode))) {
    intcode[i] <- as.integer(intcode[i])
  }
  return(intcode)
}

value <- function(mode,position){
  val=0
  if(mode==0){
    val=code[code[position]]
  }else{
    val=code[position]
  }
  return(val)
}

pos=1

while(code[pos]!=99){
  inst=intcodeArray(code[pos])
  
  if(inst[4]==1){
    two=pos+1
    three=pos+2
    four=code[pos+3]
    
    code[four]=value(inst[2],two)+value(inst[1],three)
    pos = pos+4
  }else if (inst[4]==2){
    two=pos+1
    three=pos+2
    four=code[pos+3]
    code[four]=value(inst[2],two)*value(inst[1],three)
    pos=pos+4
  }else if(inst[4]==3){
    print("What is your input?")
    choice=readline("stdin")
    choice=as.integer(choice)
    if(inst[2]==0){
      code[code[pos+1]]=choice
    }else{
      code[pos+1]=choice
    }
    pos=pos+2
  }else if(inst[4]==4){
    print(value(inst[2],(pos_1)))
    pos=pos+2
  }
}

