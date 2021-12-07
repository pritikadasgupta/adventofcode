#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- read.table("2015/Day9/input.txt",col.names = c("from","to_","to","equal","distance"))
example <- read.table("2015/Day9/example.txt",col.names = c("from","to_","to","equal","distance"))
#------------------------------------------------------------------------------------------
#Part 1 and 2
#------------------------------------------------------------------------------------------

library(combinat)
distances <- function(x){
  #drop "to" and "="
  x <- subset(x, select = -c(to_, equal))
  x$distance <- as.numeric(x$distance)
  
  #make a list of locations
  locations_from <- unique(c(x[,1],x[,2]))
  
  #get all possible paths
  all_paths <- permn(locations_from)
  
  #find distances
  distance <- vector()
  for(i in 1:length(all_paths)){
    path <- all_paths[[i]]
    distance_amt <- 0
    for(j in 1:(length(path)-1)){
      idx <- which(x$from==path[j] & x$to==path[j+1])
      if(length(idx)==0){
        idx <- which(x$from==path[j+1] & x$to==path[j])
      }
      distance_amt <- distance_amt+x$distance[idx]
    }
    distance <- append(distance, distance_amt)
  }
  
  return(c(min(distance),max(distance)))
}
distances(example)
distances(mydata)