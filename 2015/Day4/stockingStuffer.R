#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries
library(digest)

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
# mydata <- "abcdef"
mydata <- "yzbqklnj"
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

i=1
while(i >0){
  mytext <- paste(mydata,i,sep="")
  hashes <- sapply(mytext, digest, algo="md5",serialize=F)
  firstfive <-paste(strsplit(hashes,"")[[1]][1:5],collapse="")
  if(firstfive =="00000"){
    print(i)
    break
  }
  i=i+1
}

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------


i=100000
while(i >0){
  mytext <- paste(mydata,i,sep="")
  hashes <- sapply(mytext, digest, algo="md5",serialize=F)
  firstfive <-paste(strsplit(hashes,"")[[1]][1:6],collapse="")
  if(firstfive =="000000"){
    print(i)
    break
  }
  i=i+1
}

