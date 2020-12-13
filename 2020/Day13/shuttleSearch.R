#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

#Set working directory and load in data
# con <- file("stdin", open = "r")
# data1 <- readLines(con) #read in file
# close(con)
setwd("~/Documents/adventofcode1/adventofcode/2020/Day13")
mydata <- readLines("exercise13.input.txt")
data1 <- mydata[1]
data2 <- mydata[2]

#-------------------------------------------------------------------------
#Clean up bus data 
#-------------------------------------------------------------------------
data1_numeric <- as.numeric(data1) #change your time to numeric
data2_split <- strsplit(data2,",")[[1]] #get all buses
time_buses <- seq(1,data1_numeric+50,by=1)

#get all bus types
bus_type <- vector()
for(datum in data2_split){
  if(datum!="x"){
    bus_type <- append(bus_type,as.numeric(datum))
  }
}

numberofcolumns <- length(bus_type)
numberofrows <- length(time_buses)

#create data frame for bus types and times
df <- data.frame(matrix(ncol = numberofcolumns, nrow = numberofrows))
df[,1] <- time_buses

colnames(df)
colnames(df)[1] <- "time"

for(column in 2:ncol(df)){
  # colnames(df)[column] <- paste("bus",as.character(bus_type[column-1]))
  colnames(df)[column] <- as.character(bus_type[column-1])
}


# for(j in 1:length(bus_type)){
#   start <- as.numeric(bus_type[j])
#   end <- as.numeric(nrow(df))
#   for(k in seq(start,end,by=start)){
#     df[k,j+1] <- 1
#   }
# }


#-------------------------------------------------------------------------
#Clean up your data
#-------------------------------------------------------------------------

mytime <- seq(1,data1_numeric+50,by=1)
mytime_available <- rep(NA,length(mytime))


#-------------------------------------------------------------------------
#Part 1:
#-------------------------------------------------------------------------
