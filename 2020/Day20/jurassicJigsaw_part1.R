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
mydata <- gsub("#", "1", mydata)

#initialize
tile_number <- 0
tile_numbers <- vector()
tiles <- vector("list",length(tile_idx))
k=0
tile_data <- vector()

for(i in 1:length(mydata)){
  if(i %in% tile_idx){
    tile_number = as.numeric(strsplit(strsplit(mydata[i]," ")[[1]][2],":")[[1]])
    tile_numbers <- append(tile_numbers,tile_number)
    k=k+1
  }else if("" %in% mydata[i]){
    tiles[[k]] <- tile_data
    tile_data <- vector()
  }else{
    curdata <- strsplit(mydata[i],"")[[1]]
    curdata <- ifelse(curdata==".","0","1")
    curdata <- as.character(paste(curdata,collapse=""))
    tile_data <- append(tile_data,curdata)
  }
}

tiles[[k]] <- tile_data

tiles2 <- tiles
for(a in 1:length(tiles)){
  mymat <- as.matrix(tiles[[a]])
  mymat2 <- matrix(nrow = length(mymat), ncol = 10)
  for(b in 1:nrow(mymat)){
    replacement <- as.numeric(strsplit(mymat[b,],"")[[1]])
    mymat2[b,] <- replacement
  }
  tiles2[[a]] <- mymat2
}

#NOT ROTATED
df_tile_numbers <- vector()
df_tile_position <- vector()
df_tile_position_v <- vector()

for(b in 1:(length(tile_numbers))){
  cur_tile <- tiles2[[b]]
  df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
  top <- paste(cur_tile[1,],collapse="")
  df_tile_position <- append(df_tile_position,"top")
  df_tile_position_v <- append(df_tile_position_v,top)
  
  
  df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
  bottom <- paste(cur_tile[10,],collapse="")
  df_tile_position <- append(df_tile_position,"bottom")
  df_tile_position_v <- append(df_tile_position_v,bottom)
  
  
  df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
  left <- paste(cur_tile[,1],collapse="")
  df_tile_position <- append(df_tile_position,"left")
  df_tile_position_v <- append(df_tile_position_v,left)
  

  df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
  right <- paste(cur_tile[,10],collapse="")
  df_tile_position <- append(df_tile_position,"right")
  df_tile_position_v <- append(df_tile_position_v,right)
 
}

my_edges_df <- as.data.frame(cbind(df_tile_numbers,df_tile_position,df_tile_position_v))
colnames(my_edges_df) <- c("tile_id","name","value")


# #rotated 90 degrees clockwise
# df_tile_numbers <- vector()
# df_tile_position <- vector()
# df_tile_position_v <- vector()
# 
# for(b in 1:(length(tile_numbers))){
#   cur_tile <- rotate(tiles2[[b]])
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   top <- paste(cur_tile[1,],collapse="")
#   df_tile_position <- append(df_tile_position,"top_r90")
#   df_tile_position_v <- append(df_tile_position_v,top)
#   
#   
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   bottom <- paste(cur_tile[10,],collapse="")
#   df_tile_position <- append(df_tile_position,"bottom_r90")
#   df_tile_position_v <- append(df_tile_position_v,bottom)
#   
#   
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   left <- paste(cur_tile[,1],collapse="")
#   df_tile_position <- append(df_tile_position,"left_r90")
#   df_tile_position_v <- append(df_tile_position_v,left)
#   
#   
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   right <- paste(cur_tile[,10],collapse="")
#   df_tile_position <- append(df_tile_position,"right_r90")
#   df_tile_position_v <- append(df_tile_position_v,right)
#   
# }
# 
# my_edges_r90_df <- as.data.frame(cbind(df_tile_numbers,df_tile_position,df_tile_position_v))
# colnames(my_edges_r90_df) <- c("tile_id","name","value")
# 
# #rotated 180 degrees clockwise
# df_tile_numbers <- vector()
# df_tile_position <- vector()
# df_tile_position_v <- vector()
# 
# for(b in 1:(length(tile_numbers))){
#   cur_tile <- rotate(rotate(tiles2[[b]]))
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   top <- paste(cur_tile[1,],collapse="")
#   df_tile_position <- append(df_tile_position,"top_r180")
#   df_tile_position_v <- append(df_tile_position_v,top)
#   
#   
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   bottom <- paste(cur_tile[10,],collapse="")
#   df_tile_position <- append(df_tile_position,"bottom_r180")
#   df_tile_position_v <- append(df_tile_position_v,bottom)
#   
#   
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   left <- paste(cur_tile[,1],collapse="")
#   df_tile_position <- append(df_tile_position,"left_r180")
#   df_tile_position_v <- append(df_tile_position_v,left)
#   
#   
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   right <- paste(cur_tile[,10],collapse="")
#   df_tile_position <- append(df_tile_position,"right_r180")
#   df_tile_position_v <- append(df_tile_position_v,right)
#   
# }
# 
# my_edges_r180_df <- as.data.frame(cbind(df_tile_numbers,df_tile_position,df_tile_position_v))
# colnames(my_edges_r180_df) <- c("tile_id","name","value")
# 
# 
# 
# #rotated 270 degrees clockwise
# df_tile_numbers <- vector()
# df_tile_position <- vector()
# df_tile_position_v <- vector()
# 
# for(b in 1:(length(tile_numbers))){
#   cur_tile <- rotate(rotate(rotate(tiles2[[b]])))
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   top <- paste(cur_tile[1,],collapse="")
#   df_tile_position <- append(df_tile_position,"top_r270")
#   df_tile_position_v <- append(df_tile_position_v,top)
#   
#   
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   bottom <- paste(cur_tile[10,],collapse="")
#   df_tile_position <- append(df_tile_position,"bottom_r270")
#   df_tile_position_v <- append(df_tile_position_v,bottom)
#   
#   
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   left <- paste(cur_tile[,1],collapse="")
#   df_tile_position <- append(df_tile_position,"left_r270")
#   df_tile_position_v <- append(df_tile_position_v,left)
#   
#   
#   df_tile_numbers <- append(df_tile_numbers,tile_numbers[b])
#   right <- paste(cur_tile[,10],collapse="")
#   df_tile_position <- append(df_tile_position,"right_r270")
#   df_tile_position_v <- append(df_tile_position_v,right)
#   
# }

# my_edges_r270_df <- as.data.frame(cbind(df_tile_numbers,df_tile_position,df_tile_position_v))
# colnames(my_edges_r270_df) <- c("tile_id","name","value")
# 
# my_edges_total <- merge(my_edges_df,my_edges_r90_df,by="value")
# my_edges_total <- merge(my_edges_df,my_edges_r180_df,by="value")
# my_edges_total <- merge(my_edges_df,my_edges_r270_df,by="value")

my_edges_df_filtered <- filter(my_edges_df, tile_id != first(tile_id))
my_edges_merged <- merge(my_edges_df_filtered,
                              my_edges_r180_df,
                              by = 'value')
my_edges_merged <- filter(my_edges_merged,tile_id.x != tile_id.y) 
my_edges_merged <- add_count(my_edges_merged,tile_id.x, name = 'n.x')
my_edges_merged <- add_count(my_edges_merged,tile_id.y, name = 'n.y')


corner <- union(my_edges_merged$tile_id.x[my_edges_merged$n.x == 2 & my_edges_merged$n.y == 3],
                my_edges_merged$tile_id.y[my_edges_merged$n.y == 2 & my_edges_merged$n.x == 3])

paste(prod(as.numeric(corner)))


