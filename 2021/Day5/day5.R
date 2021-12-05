#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- read.table(text=gsub("\\D+"," ",readLines("2021/Day5/input.txt")),fill=T,blank.lines.skip = F)
colnames(mydata) <- c("x1","y1","x2","y2")
example <- read.table(text=gsub("\\D+"," ",readLines("2021/Day5/example.txt")),fill=T,blank.lines.skip = F)
colnames(example) <- c("x1","y1","x2","y2")

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
diagram <- function(x){
  size <- max(x, na.rm=TRUE)
  graph <- matrix(0,nrow=size+1,ncol=size+1)
  for(i in 1:nrow(x)){
    x1_ <- x$x1[i]+1
    x2_ <- x$x2[i]+1
    y1_ <- x$y1[i]+1
    y2_ <- x$y2[i]+1

    if(y1_==y2_){#horizontal lines
      graph[y1_:y2_, x1_:x2_] <- graph[y1_:y2_, x1_:x2_] + 1
    }
    
    if(x1_==x2_){#vertical lines
      graph[y1_:y2_, x1_:x2_] <- graph[y1_:y2_, x1_:x2_] + 1
    }
  }
  return(graph)
}



sum(diagram(example)>=2)
sum(diagram(mydata)>=2)


#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
diagram_d <- function(x){
  size <- max(x, na.rm=TRUE)
  graph <- matrix(0,nrow=size+1,ncol=size+1)
  for(i in 1:nrow(x)){
    x1_ <- x$x1[i]+1
    x2_ <- x$x2[i]+1
    y1_ <- x$y1[i]+1
    y2_ <- x$y2[i]+1
    
    if(y1_==y2_){
      graph[y1_:y2_, x1_:x2_] <- graph[y1_:y2_, x1_:x2_] + 1
    }else if(x1_==x2_){
      graph[y1_:y2_, x1_:x2_] <- graph[y1_:y2_, x1_:x2_] + 1
    }else{
      x_seq <- seq(x1_,x2_)
      y_seq <- seq(y1_,y2_)
      for(k in 1:length(x_seq)){
        graph[y_seq[k], x_seq[k]] <- graph[y_seq[k], x_seq[k]] + 1
      }
    }
  }
  return(graph)
}

sum(diagram_d(example)>=2)
sum(diagram_d(mydata)>=2)
