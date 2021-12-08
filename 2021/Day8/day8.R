#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines("2021/Day8/input.txt")
example1 <- readLines("2021/Day8/example1.txt")
example2 <- readLines("2021/Day8/example2.txt")

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
unique_segments <- function(x){
  y <- strsplit(x," ")
  tally <- 0
  for(i in 1:length(y)){
    z <- y[[i]][12:15]
    tally <- tally + length(which(nchar(z)==2 |nchar(z)==4 |nchar(z)==3 |nchar(z)==7))
  }
  return(tally)
}
unique_segments(mydata)

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
translate_patterns <- function(x){
  y <- strsplit(x," ")
  all_codes <- 0
  for(i in 1:length(y)){
    code <- y[[i]][12:15]
    patterns <- y[[i]][1:10]
    
    #unique patterns
    pattern1 <- strsplit(patterns[which(nchar(patterns)==2)],"")[[1]] #1
    pattern4 <- strsplit(patterns[which(nchar(patterns)==4)],"")[[1]] #4
    pattern7 <- strsplit(patterns[which(nchar(patterns)==3)],"")[[1]] #7
    pattern8 <- strsplit(patterns[which(nchar(patterns)==7)],"")[[1]] #8
    
    
    pattern069 <- patterns[which(nchar(patterns)==6)] #0,6,9
    pattern235 <- patterns[which(nchar(patterns)==5)] #2,3,5
  
    
    for(j in 1:length(pattern069)){
      pattern_ <- strsplit(pattern069[j],"")[[1]]
      
      a <- pattern1[! pattern1 %in% pattern_] 
      b <- pattern1[! pattern4 %in% pattern_] 
      
      if(length(a)==0 & length(b)==0){
        pattern9 <- strsplit(pattern069[j],"")[[1]]
      }else if(length(a)!=0 & length(b)!=0){
        pattern6 <- strsplit(pattern069[j],"")[[1]]
      }else{
        pattern0 <- strsplit(pattern069[j],"")[[1]]
      }
    }
    
    top_right_char <- pattern1[! pattern1 %in% pattern6] 
    
    for(j in 1:length(pattern235)){
      pattern_ <- strsplit(pattern235[j],"")[[1]]
      
      a <- length(pattern1[! pattern1 %in% pattern_])==0
      b <- top_right_char %in% pattern_
      
      if(a){
        pattern3 <- strsplit(pattern235[j],"")[[1]]
      }else if(b){
        pattern2 <- strsplit(pattern235[j],"")[[1]]
      }else{
        pattern5 <- strsplit(pattern235[j],"")[[1]]
      }
    }
    
    patterns_list <- list(pattern0,pattern1,pattern2,pattern3,pattern4,pattern5,pattern6,pattern7,pattern8,pattern9)
    code_deciphered <- vector()
    for(k in 1:length(code)){
      code_ <- strsplit(code[k],"")[[1]]
      
      for(p in 1:length(patterns_list)){
        if(setequal(code_,patterns_list[[p]])){
          code_deciphered <- append(code_deciphered,(p-1))
        }
      }
    }
    code_deciphered_val <- as.numeric(paste0(code_deciphered,collapse=""))
    all_codes <- all_codes + code_deciphered_val
  }
  return(all_codes)
}

translate_patterns(example1)
translate_patterns(example2)
translate_patterns(mydata)
