#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries
library(tidyverse)
library(tidyr)
library(dplyr)

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------

#Use this if you're running from the command line:
# mydata <- strsplit(readLines(file("stdin"))," ")

#Use this if you're opening this repo as a R project, using relative paths:
# mydata <- strsplit(readLines('2020/Day19/input_test1.txt')," ")
mydata <- readLines('2020/Day20/input_test1.txt')

#------------------------------------------------------------------------------------------
#Functions
#------------------------------------------------------------------------------------------
# 90 degree clockwise rotation of matrix
rotate <- function(x) t(apply(x, 2, rev))

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

tile_idx <- which(grepl("Tile", mydata, fixed = TRUE)==TRUE)
tile_numbers_char <- (strsplit(mydata[tile_idx],":"))
tile_numbers <- vector()
for(i in 1:length(tile_numbers_char)){
  tilenum <- as.numeric(strsplit(tile_numbers_char[[i]],"Tile ")[[1]][2])
  tile_numbers <- append(tile_numbers,tilenum)
}

tiles <- list()
tile_data <- vector()
k=0
for(i in 1:length(mydata)){
  if(i %in% tile_idx){
    k=k+1
  }else if("" %in% mydata[i]){
    tiles[[k]] <- tile_data
    tile_data <- vector()
  }else{
    curdata <- mydata[i]
    tile_data <- append(tile_data,curdata)
  }
}
tiles[[k]] <- tile_data


find_top_bottom <- function(tile){
  top_vals <- tile[1]
  bottom_vals <- tile[length(tile)]
  return(list(top_vals,bottom_vals))
}


find_left_right <- function(tile){
  # tile <- tiles[[1]]
  left_vals <- vector()
  right_vals <- vector()
  for(i in 1:length(tile)){
    tile_ <- strsplit(tile[i],"")[[1]]
    left_vals <- append(left_vals,tile_[1])
    right_vals <- append(right_vals,tile_[length(tile_)])
  }
  left_vals <- paste(left_vals,collapse="")
  right_vals <- paste(right_vals,collapse="")
  return(list(left_vals,right_vals))
}



n=length(tile_numbers)*4
tile_id <- vector()
tile_position <- rep("0",n)
tile_value <- rep("0",n)

a=1
repeat{
  tile_id <- append(tile_id,rep(tile_numbers[a],4))
  a=a+1
  if(a==10){break}
}
df <- as.data.frame(cbind(tile_id,tile_position,tile_value))

a=1
for(b in seq(1,36,by=4)){
  
  x <- find_left_right(tiles[[a]])
  y <- find_top_bottom(tiles[[a]])
  
  df$tile_position[b] <- "left"
  df$tile_position[b+1] <- "right"
  df$tile_position[b+2] <- "top"
  df$tile_position[b+3] <- "bottom"
  df$tile_value[b] <- x[[1]]
  df$tile_value[b+1] <- x[[2]]
  df$tile_value[b+2] <- y[[1]]
  df$tile_value[b+3] <- y[[2]]
  a=a+1
}


df_filtered <- filter(df, df$tile_id != first(df$tile_id))

df_merged <- merge(df,df_filtered,by = 'tile_value')
my_edges_merged <- filter(df_merged,df_merged$tile_id.x != df_merged$tile_id.y) 
my_edges_merged <- add_count(my_edges_merged,tile_id.x, name = 'n.x')
my_edges_merged <- add_count(my_edges_merged,tile_id.y, name = 'n.y')

corner <- union(my_edges_merged$tile_id.x[my_edges_merged$n.x == 2 & my_edges_merged$n.y == 3],
                my_edges_merged$tile_id.y[my_edges_merged$n.y == 2 & my_edges_merged$n.x == 3])

paste(prod(as.numeric(corner)))



