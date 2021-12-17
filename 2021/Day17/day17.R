#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
mydata <- readLines("2021/Day17/input.txt")
example <- readLines("2021/Day17/example.txt")
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
parse_data <- function(x){
  pos_x_numbers <- as.numeric(regmatches(x, gregexpr("[[:digit:]]+", x))[[1]])
  neg_x_numbers <- as.numeric(regmatches(x, gregexpr("-[[:digit:]]+", x))[[1]])
  x_numbers <- c()
  for(i in 1:length(pos_x_numbers)){
    if(pos_x_numbers[i] %in% -neg_x_numbers){
      x_numbers <- c(x_numbers,-pos_x_numbers[i])
    }else{
      x_numbers <- c(x_numbers,pos_x_numbers[i])
    }
  }
  return(x_numbers)
}

check_velocity <- function(velocity, x_numbers) {
  position <- c(0,0)
  max_y <- 0
  
  repeat {
    position[1] <- position[1] + velocity[1]
    position[2] <- position[2] + velocity[2]
    velocity[1] <- ifelse(velocity[1] == 0, 0, sign(velocity[1]) * (abs(velocity[1])-1))
    velocity[2] <- velocity[2] - 1
    max_y <- max(position[2], max_y)
    if (position[1]>=x_numbers[1] & position[1]<=x_numbers[2] & position[2]>=x_numbers[3] & position[2]<=x_numbers[4]) return(max_y)
    if (position[1] > x_numbers[2] | position[2] < x_numbers[3]) return(NA)
  }
}

#takes a while to run
part1 <- function(input){
  result <- 0
  thres <- max(parse_data(input))
  for(x in 1:thres) {
    for(y in -thres:thres) {
      result <- max(result, check_velocity(c(x,y), parse_data(input)), na.rm=TRUE)
    }
  }
  return(result)
}

# part1(example)
part1(mydata)



#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------

part2 <- function(input){
  result <- 0
  thres <- max(parse_data(input))
  for(x in 1:thres) {
    for(y in -thres:thres) {
      z <- check_velocity(c(x,y), parse_data(input))
      if(!is.na(z)) result <- result+1
    }
  }
  return(result)
}

# part2(example)
part2(mydata)

