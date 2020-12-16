#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

#data
input_test <- "stdin"
# input_test <- "1,20,11,6,12,0"
# input_test <- "0,3,6"
input_test <- strsplit(input_test,",")[[1]]
input_test_long <- rep(0,30000000)

#let's do 30000000 turns
i=length(input_test)+1
input_test_long[1:length(input_test)] <- input_test

while(i <=30000000){
  spoken <- input_test_long[1:i-1]
  
  #consider last spoken number
  if(input_test_long[i-1] %in% spoken){
    idx <- which(as.numeric(spoken)==as.numeric(input_test_long[i-1]))
    
    if(length(idx)==1){
      input_test_long[i] <- 0
    }else{
      input_test_long[i] <- as.numeric(idx[length(idx)])-as.numeric(idx[(length(idx)-1)])
    }
    
    
  }else{
    input_test_long[i] <- 0
  }
  
  #update i
  i=i+1
}

# part1
input_test_long[2020]

# part2
input_test_long[30000000]

