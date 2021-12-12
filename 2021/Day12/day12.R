#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- read.delim("2021/Day12/input.txt",sep="-",header = FALSE,col.names = c("from","to"))
example1 <- read.delim("2021/Day12/example1.txt",sep="-",header = FALSE,col.names = c("from","to"))
example2 <- read.delim("2021/Day12/example2.txt",sep="-",header = FALSE,col.names = c("from","to"))

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

x <- mydata
unique_nodes <- unique(unlist(x))
valid_nodes <- setNames(rep(TRUE, length(unique_nodes)), unique_nodes)
visits <- setNames(rep(0, length(unique_nodes)), unique_nodes)
start <- "start"
finish <- "end"
part <- 1

num_paths <- function(x, valid_nodes, visits, start, finish, part) {
  if (start == finish) {
    return(1)
  }
  
  if (tolower(start) == start) {
    if (start == "start"){
      valid_nodes[start] <- FALSE
    }
    
    visits[start] <- visits[start] + 1
    
    if (visits[start] == part) {
      valid_nodes[names(visits[visits >= 1])] <- FALSE
    } else if (any(visits == part)) {
      valid_nodes[start] <- FALSE
    }
  }
  
  next_nodes <- c(x$from[x$to == start], x$to[x$from == start])
  next_nodes <- next_nodes[next_nodes %in% unique_nodes[valid_nodes]]
  # print(next_nodes)
  if (length(next_nodes) == 0) {
    return(0)
  } else {
    return(sum(sapply(next_nodes, num_paths, x = x, visits = visits,valid_nodes = valid_nodes, finish = finish, part = part)))
  }
}


num_paths(x, valid_nodes, visits, start, finish, part)

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
num_paths(x, valid_nodes, visits, start, finish, part=2)

