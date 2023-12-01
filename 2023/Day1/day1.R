#!/usr/bin/env Rscript
#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
df <- readLines("2023/Day1/input.txt")
df_test <- readLines("2023/Day1/test.txt")

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

find_numbers <- function(df){
  numbers <- c()
  for (i in 1:length(df)){
    data <- as.numeric(strsplit(df[i],"")[[1]])
    x <- data[!is.na(data)]
    first <- as.character(x[1])
    last <- as.character(x[length(x)])
    number <- as.numeric(paste(c(first,last),collapse=''))
    numbers <- append(numbers, number)
  }
  return(sum(numbers))
}
test1 <- find_numbers(df_test)
answer1 <- find_numbers(df)
answer1

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------

convert <- function(df){
  y <- seq(1,9)
  valid_numbers <- c("one","two","three","four","five","six","seven","eight","nine")

  for (k in 1:length(df)){
    df4 <- strsplit(df[k],"")[[1]]
    c=1
    while(c<length(df4)){
      j=1
      while(j<10){
        thenum <-strsplit(valid_numbers[j],"")[[1]]
        len <- length(thenum)
        if((c+len-1)>=length(df4)){
          df5 <- paste(df4[c:length(df4)],collapse="")
          df6 <- ""
          if(c==1){
            df7 <- ""
          }else{
            df7 <- paste(df4[1:(c-1)],collapse="")
          }
        }else{
          df5 <- paste(df4[c:(c+len-1)],collapse="")
          if(len==length(df4)){
            df6 <- ""
          }else{
            df6 <- paste(df4[(c+len):(length(df4))],collapse="")
          }
          if(c==1){
            df7 <- ""
          }else{
            df7 <- paste(df4[1:(c-1)],collapse="")
          }
        }
        df5 <- gsub(valid_numbers[j], paste(as.character(y[j]),thenum[length(thenum)],collapse=""), df5)
        
        df8 <-paste(c(df7,df5,df6),collapse="")
        df9 <- strsplit(df8[1],"")[[1]]
        
        if(length(df9)!=length(df4)){
          df4 <- df9
          j=1
          c=1
        }else{
          j=j+1
          df4 <- df9
        }
        
      }
      c = c+1
    }
    df[k] <- paste(df4,collapse="")
  }

  return(df)
}

test2 <- find_numbers(convert(df_test))

answer2 <- find_numbers(convert(df))

answer2
