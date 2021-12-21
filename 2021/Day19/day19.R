#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
mydata <- readLines("2021/Day19/input.txt")
example1 <- readLines("2021/Day19/example1.txt")
example2 <- readLines("2021/Day19/example2.txt")
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
x <- example2

get_positions <- function(x){
  df <- data.frame(matrix(NA,nrow=length(x),ncol=4))
  colnames(df) <- c("scanner","x","y","z")
  for(i in 1:length(x)){
    if(grepl("scanner",x[i])){
      j <- as.numeric(regmatches(x[i], gregexpr("[[:digit:]]+", x[i]))[[1]])+1
      df$scanner[i] <- j-1
    }else{
      df$scanner[i] <- j-1
      coord <- as.numeric(strsplit(x[i],",")[[1]])
      df$x[i] <- coord[1]
      df$y[i] <- coord[2]
      df$z[i] <- coord[3]
    }
  }
  df <- df[complete.cases(df),]
  
  # testdf <- df
  scanner_num <- unique(df$scanner)
  
  start_x <- 0
  start_y <- 0
  start_z <- 0
  for(i in 1:length(scanner_num)){
    scanner_seq <- which(df$scanner==scanner_num[i])
    for(j in scanner_seq){
      df$beacon_x[j] <- start_x + df$x[j]
      df$beacon_y[j] <- start_y + df$y[j]
      df$beacon_z[j] <- start_z + df$z[j]
    }
    
  }
  return(df)
}










#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
