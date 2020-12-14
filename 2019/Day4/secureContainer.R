#!/usr/bin/env Rscript
#clear workspace
rm(list = ls())

#Load Data
#Use this if you're running from the command line:
# data1 <- readLines("stdin) #read in data

data <- "130254-678275"
data1 <- as.numeric(strsplit(data,"-")[[1]])

password <- function(pass,data1){
  valid = FALSE
  # It is a six-digit number.
  numeric_vec <- as.numeric(strsplit(as.character(pass),"")[[1]])
  thelength <- length(numeric_vec)
  
  # The value is within the range given in your puzzle input.
  a <- (pass >=data1[1])
  b <- (pass <=data1[2])
  
  # Two adjacent digits are the same (like 22 in 122345).
  l <-  rle(numeric_vec)$lengths
  c1 <- sum(rep(ifelse(l == 1, 0, 1), times = l))
  c <- (c1 == 0)
  
  # Going from left to right, the digits never decrease; 
  # they only ever increase or stay the same (like 111123 or 135679).
  sorted_pass <- as.numeric(paste(sort(numeric_vec),sep="",collapse=""))
  d <- (pass == sorted_pass)
  
  
  if(a & b & c & d & (thelength==6)){
    valid = TRUE
  }
  return(valid)
}

# curvector <- vector()
# for(i in data1[1]:data1[2]){
  # current <- password(i,data1)
  # curvector <- append(curvector,current)
  # print(current)
# }

# sum(curvector)


rm(list = ls())
passwords <- strsplit(as.character(130254:678275), "")
sorted <- passwords[!vapply(passwords, is.unsorted, TRUE)]
freq <- lapply(sorted, table)
total_1 <- sum(vapply(freq, function(x) any(x > 1), TRUE))
total_2 <- sum(vapply(freq, function(x) 2 %in% x, TRUE))
print(total_1)
print(total_2)




