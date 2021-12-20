#!/usr/bin/env Rscript
library(raster)
#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
parse_data = function(filename) {
  input = readLines(filename)
  
  #image enhancement algorithm
  iea <- strsplit(input[1],"")[[1]]
  #image
  img <- do.call(rbind,strsplit(input[3:length(input)],""))
  
  return(list(iea,img))
}

#Use this if you're opening this repo as a R project, using relative paths:
mydata <- parse_data("2021/Day20/input.txt")
example <- parse_data("2021/Day20/example.txt")

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
#day 16 bin_to_int function
bin_to_int = function(x) {
  sum(x*2^(seq(length(x)-1,0)))
}

process_algorithm = function(x) {
  i <- bin_to_int(x)
  cur <- iea[i+1]
  return(ifelse(cur == "#", 1, 0))
}

part1 <- function(x,applications){
  iea <- x[[1]]
  img <- x[[2]]
  ext <- matrix(c(0,0,ncol(img),nrow(img)),2,2) 
  grow <- matrix(c(-1,-1,1,1),2,2)
  
  new_img <- ifelse(img == "#", 1, 0)
  
  new_ras <- raster(new_img,xmn=0, xmx=ncol(img), ymn=0, ymx=nrow(img))
  
  new_ras <- extend(new_ras,extent(ext + 150*grow), value=0)
  
  #apply algorithm
  for(i in 1:applications){
    new_ras <- focal(new_ras,w = matrix(1, 3,3), fun=process_algorithm, pad=FALSE, padValue=0)
  }
  return(new_ras)
}

# iea <- example[[1]]
iea <- mydata[[1]]
part1_result <- part1(mydata,2)
sum(part1_result[], na.rm=TRUE)
plot(part1_result,main="Part 1",legend=FALSE)


#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------

iea <- mydata[[1]]
part2_result <- part1(mydata,50)
sum(part2_result[], na.rm=TRUE)
plot(part2_result,main="Part 2",legend=FALSE)
