#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

#Load Data
#Use this if you're running from the command line:
# mydata <- readLines(file("stdin")) #read in file

#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines("2020/Day13/input")

data1 <- mydata[1]
data2 <- mydata[2]

#-------------------------------------------------------------------------
#Clean up bus data 
#-------------------------------------------------------------------------
data1_numeric <- as.numeric(data1) #change your time to numeric
data2_split <- strsplit(data2,",")[[1]] #get all buses
time_buses <- seq(1,data1_numeric+500000,by=1)

#-------------------------------------------------------------------------
#Part 2:
#-------------------------------------------------------------------------

#a start of a very time-consuming soln
# startingpoint <- which(finaldf[,2]==1)
# possibilities <- vector()
# for(i in startingpoint){
#   for(j in 3:length(bus_type)){
#     checkpoints <- which(finaldf[,j]==1)
#     for(k in checkpoints){
#       if(i==k+data2_minutes[j-1]){
#         print(paste("The i, j, and k are",i,j,k))
#         possibilities <- append(possibilities,i)
#       }
#     }
#   }
# }



data2_split_int <- as.integer(strsplit(data2, ',')[[1]])

bus_stuff <- function(i, j, k, l, m = 1e5) {
  x <- i + k * (0:m)
  x[which.max(x %% l == j %% l)]
}

find <- function(a) {
  offsets <- -(seq_along(a) - 1)[!is.na(a)] # a
  a <- a[!is.na(a)]                     # n
  x <- offsets[1]
  for (i in 2:length(a))
    x <- bus_stuff(x, offsets[i], prod(head(a, i-1)), a[i])
  return(x)
}

answer = format(find(data2_split_int), sci = FALSE)
print(paste("Part 2:",answer))

