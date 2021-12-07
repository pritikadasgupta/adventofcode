#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
mydata <- strsplit("vzbxkghb", "")[[1]]
example <- strsplit("abcdefgh", "")[[1]]
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
increment_password <- function(x,pointer){
  y <- match(x, letters)
  
  #increment letter by 1
  y[pointer] <- y[pointer]+1
  
  #no i, o, l in password
  if (any(y %in% c(9, 15, 12))) {
    which_min <- min(which(y %in% c(9, 15, 12)))
    y[which_min] <- y[which_min] + 1
    if (which_min < 8) {
      y[seq(min(which_min + 1, 8), 8)] <- 1
    }
  }
  # print(y[pointer])
  #if letter value exceeds 26 (z)
  if (y[pointer] == 27) {
    y[pointer] <- 1
    if (pointer > 1) {
      y <- match(increment_password(letters[y],pointer-1),letters)
    }
  }
  
  return(letters[y])
}



check_password <- function(x) {
  x <- match(x, letters)
  rle1 <- rle(diff(x))
  
  # Passwords must include one increasing straight of at least three letters, 
  # like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
  check1 <- any(rle1$lengths[rle1$values == 1] >= 2)
  
  # Passwords may not contain the letters i, o, or l, as these letters can be mistaken for 
  # other characters and are therefore confusing.
  check2 <- all(!c(9, 15, 12) %in% x)
  
  # Passwords must contain at least two different, non-overlapping pairs of letters, 
  # like aa, bb, or zz.
  check3 <- sum(rle(x)$lengths >= 2) >= 2
  
  return(check1 & check2 & check3)
}


password <- function(x){
  i <- 0
  while(i==0){
    new_password <- increment_password(x, length(x))
    if(check_password(new_password)){
      i <- 1
    }else{
      x <-new_password
      # print(x)
    }
  }
  return(paste0(new_password, collapse = ""))
}


password(example)
part1 <- password(mydata)
#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
part2 <- password(strsplit(part1, "")[[1]])
