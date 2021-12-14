#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- read.table("2021/Day13/input.txt",colClasses= 'character') 
example <- read.table("2021/Day13/example.txt",colClasses= 'character')

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

folding <- function(x,part){
  split_ind <- which(x == "fold")[1]
  coords <- x[1:(split_ind-1),]
  coords <- strsplit(coords, split=",")
  coords <- matrix(as.numeric(unlist(coords)), nrow=(split_ind - 1), byrow = T)
  folds <- x[split_ind:dim(x)[1],]
  folds <- folds[which(folds != "fold" & folds != "along")]
  
  if(part==1){
    num_folds <- 1
  }else{
    num_folds <- length(folds)
  }
  
  for(i in 1:num_folds){
    aor <- strsplit(folds[i],split="=")[[1]]
    num <- as.numeric(aor[2])
    var <- aor[1]
    if(var == "x"){
      dists <- abs(coords[,1] - num)
      inds <- which(coords[,1] < num)
      coords[inds,1] <- coords[inds,1] + 2*dists[inds]
      coords[,1] <- coords[,1] - (num + 1)
    }
    else {
      dists <- abs(coords[,2] - num)
      inds <- which(coords[,2] > num)
      coords[inds,2] <- coords[inds,2] - 2*dists[inds]
    }
  }
  return(coords)
}


nrow(unique(folding(mydata,part=1)))
#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
part2_df <- as.data.frame(folding(mydata,part=2))
colnames(part2_df) <- c("x","y")

library(ggplot2)
ggplot(part2_df) +
  aes(x, y) +
  geom_tile(fill = '#8712DF') +
  coord_fixed() +
  scale_x_reverse() +
  scale_y_reverse() +
  theme_void()








