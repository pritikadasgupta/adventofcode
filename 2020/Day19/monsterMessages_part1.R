#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries
library(tidyverse)

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------

#Use this if you're running from the command line:
# mydata <- strsplit(readLines(file("stdin"))," ")

#Use this if you're opening this repo as a R project, using relative paths:
# mydata <- strsplit(readLines('2020/Day19/input_test1.txt')," ")
mydata <- readLines('2020/Day19/input_test1.txt')
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

#separate out rules and messages
find_null <- function(mydata){
  idx <- 0
  for(i in 1:length(mydata)){
    if(mydata[i]==""){
      idx <- i
    }
  }
  return(idx)
}

null_idx <- find_null(mydata)

subsetList <- function(myList, elementNames) {
  lapply(elementNames, FUN=function(x) myList[[x]])
}

messages <- subsetList(mydata,c((null_idx+1):length(mydata)))
rules <- subsetList(mydata,c((1):(null_idx-1)))

#rule cleaning
id <- vector()
rule <- vector()
for(j in 1:length(rules)){
  rule_ <- strsplit(rules[[j]],":")[[1]][2]
  rule_ <- as.character(strsplit(rule_," ")[[1]])
  rule_ <- rule_[which(rule_!="")]
  rule_ <- paste(rule_,collapse=" ")
  rule_ <- paste("( ", rule_, " )",sep="")
  
  id <- append(id,j-1)

  rule <- append(rule,rule_)
}

rules_df <- as.data.frame(cbind(id,rule))

sub = !grepl(" [0-9]+ ", rules_df$rule)
rules_done_df = as.data.frame(rules_df[sub,])
for(b in 1:nrow(rules_done_df)){
  mystring <- gsub("[^[:alnum:][:space:]]","",rules_done_df$rule[b])
  mystring <-strsplit(mystring," ")[[1]]
  mystring <- mystring[which(mystring!="")]
  rules_done_df$rule[b] <- mystring
}
rules_df = as.data.frame(rules_df[!sub,])

rules_df = as.data.frame(rbind(rules_df,rules_done_df))





# build rule-checker and regex from rules

repeat {
  
  sub = !grepl(" [0-9]+ ", rules_df$rule)
  if (sum(!sub) == 0) {break}
  
  done_df = rules_df[sub,]
  rules_df = rules_df[!sub,]
  
  for(i in seq_len(nrow(done_df))) {
    id = done_df$id[i]
    sub = done_df$rule[i]
    
    repeat {
      prev_df = rules_df
      rules_df = rules_df %>% mutate(
        rule = str_replace_all(
          rule,
          paste0(" ", done_df$id[i], " "),
          paste0(" ", done_df$rule[i], " ")
        )
      )
      if (identical(rules_df, prev_df)){break}
    }
  }
}

#regex expression
final = rules_df$rule[rules_df$id == 0]
final_rule = paste0("^(", str_remove_all(final, " "), ")$")
final_rule
# "^((a((aa|bb)(ab|ba)|(ab|ba)(aa|bb))b))$"


valid <- vector()
for(d in 1:length(messages)){
  valid <- append(valid,grepl(final_rule, messages[[d]]))
}

sum(valid)
