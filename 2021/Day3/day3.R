#!/usr/bin/env Rscript
#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines("2021/Day3/input.txt")
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
df <- as.data.frame(mydata)
df1 <- data.frame(do.call("rbind", strsplit(as.character(df$mydata), "", fixed = TRUE)))

gamma <- vector()
epsilon <- vector()

for(i in 1:ncol(df1)){
  tabdf1 <- sort(table(df1[,i]), decreasing = TRUE)[1:2]
  gamma <- append(gamma,names(tabdf1)[1])
  epsilon <- append(epsilon,names(tabdf1)[2])
}

gamma_val <- strtoi(paste(gamma, collapse=""), base = 2)
epsilon_val <- strtoi(paste(epsilon, collapse=""), base = 2)
print(gamma_val*epsilon_val)

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------

df2 <- df1
df3 <- df1
for(i in 1:ncol(df2)){
  tabdf2 <- sort(table(df2[,i]), decreasing = TRUE)[1:2]
  if(length(tabdf2)==1){
    df2 <- df2[which(df2[,i]==names(tabdf2)[1]),] #keep rows
  }else if(tabdf2[1]==tabdf2[2]){
    df2 <- df2[which(df2[,i]==1),] #keep rows
  }else{
    df2 <- df2[which(df2[,i]==names(tabdf2)[1]),] #keep rows
  }
}
oxygen <- strtoi(paste(df2, collapse=""), base = 2)

for(i in 1:ncol(df3)){
  tabdf3 <- sort(table(df3[,i]), decreasing = TRUE)[1:2]
  if(length(tabdf3)==1){
    df3 <- df3[which(df3[,i]==names(tabdf3)[1]),] #keep rows
  }else if(tabdf3[1]==tabdf3[2]){
    df3 <- df3[which(df3[,i]==0),] #keep rows
  }else{
    df3 <- df3[which(df3[,i]==names(tabdf3)[2]),] #keep rows
  }
}
scrubber <- strtoi(paste(df3, collapse=""), base = 2)

print(oxygen*scrubber)
