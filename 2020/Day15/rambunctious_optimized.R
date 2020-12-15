#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

#data
# input_test1 <- "stdin" #prompt you for the input
input_test1 <- "1,20,11,6,12,0"

main_fn <- function(turns){
  input_test2 <- strsplit(input_test1,",")[[1]]
  length_input <- length(input_test2)
  while(length_iput <- turns-1){
    cur_num = input_test1[length_input]
    if(cur_num %in% input_test2){
      input_test1<- append(input_test1,length_input-input_test2[cur_num])
    }else{
      input_test1<- append(input_test1,0)
    }
    input_test2[cur_num] = length_input
    length_input = length_input + 1
  }
  return(input_test1[length(input_test1)])
}

print(paste("Part 1:",main_fn(2020)))
print(paste("Part 2:",main_fn(30000000)))