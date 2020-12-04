#Libraries
library(tidyverse)

#Load In Data
df <- read.table(file("stdin"), header = FALSE, sep = " ")

#-------------------
#Clean up dataframe
#-------------------
#get min and max: lowest and highest number of times 
# a given letter must appear for the password to be valid
df1 <- df %>% separate(V1, c("min", "max")) 

#get rid of the ":" for the letter
df2 <- df1 %>% separate(V2, c("letter", NA))

#-------------------
#PART 1
#-------------------

#-------------------
#Create function to count how many times a character occurs in a string
#-------------------
countLetter <- function(x){
  char <- x[1]
  s <- x[2]
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}

df2$count <- apply(df2[,3:4], 1, countLetter)

#-------------------
#Check validity
#-------------------
validPassword <- function(x){
  x <- as.numeric(x)
  if(x[5] <= x[2] && x[5]>= x[1]){
    return(1)
  }else{
    return(0)
  }
}

df2$valid <- apply(df2, 1, validPassword)

#-------------------
#Part 1 Answer
#-------------------
sum(df2$valid)



#-------------------
#PART 2
#-------------------

df3 <- df2[,1:4]

#-------------------
#Check validity
#-------------------
validPassword2 <- function(x){
  string <- strsplit(as.character(x[4]),"")[[1]]
  min <- as.numeric(x[1])
  max <- as.numeric(x[2])
  letter <- as.character(x[3])
  if(string[min] == letter && string[max]==letter){
    return(0)
  }else if(string[min] == letter || string[max]==letter){
    return(1)
  }else{
    return(0) 
  }
}

df3$valid <- apply(df3, 1, validPassword2)

#-------------------
#Part 2 Answer
#-------------------
sum(df3$valid)
