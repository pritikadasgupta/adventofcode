#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries


# You collect the rules for ticket fields, 
# the numbers on your ticket, 
# and the numbers on other nearby tickets for the same train service 

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------

#Use this if you're running from the command line:
# mydata <- read.table(file("stdin"), sep = '=', strip.white = TRUE,col.names = c('key', 'value'))

#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines('2020/Day16/input.txt')

#------------------------------------------------------------------------------------------
#Functions
#------------------------------------------------------------------------------------------

#create rule parser
rule_parser <- function(rule){
  ruletype <- strsplit(rule,":")[[1]][1]
  rulerange <- strsplit(rule,":")[[1]][2]
  rulerange1 <- strsplit(rulerange,"or")[[1]][1]
  rulerange2 <- strsplit(rulerange,"or")[[1]][2]
  rulerange1_start <- as.numeric(strsplit(rulerange1,"-")[[1]][1])
  rulerange1_end <- as.numeric(strsplit(rulerange1,"-")[[1]][2])
  rulerange2_start <- as.numeric(strsplit(rulerange2,"-")[[1]][1])
  rulerange2_end <- as.numeric(strsplit(rulerange2,"-")[[1]][2])
  
  valid_num <- vector()
  for(i in rulerange1_start:rulerange1_end){
    valid_num <- append(valid_num,i)
  }
  for(j in rulerange2_start:rulerange2_end){
    valid_num <- append(valid_num,j)
  }
  return(valid_num)
}

#------------------------------------------------------------------------------------------
#Clean Data
#------------------------------------------------------------------------------------------
yourticket_idx <- which(mydata == "your ticket:")
rules <- mydata[1:(yourticket_idx-2)]
myticket <- as.numeric(strsplit(mydata[yourticket_idx+1],",")[[1]])

nearbyticket_idx <- which(mydata == "nearby tickets:")

nearbyticket <- vector()
for(i in (nearbyticket_idx+1):(length(mydata))){
  nearbyticket <- append(nearbyticket,as.numeric(strsplit(mydata[i],",")[[1]]))
}

valid_num <- vector()
for(i in 1:length(rules)){
  valid_num <- append(valid_num,rule_parser(rules[i]))
}

invalid_num <- nearbyticket[!nearbyticket %in% valid_num]
valid_tickets <- nearbyticket[nearbyticket %in% valid_num]

#part 1
print(paste("Part 1:",sum(invalid_num)))
